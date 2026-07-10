import argparse
import json
import logging
import sys
import traceback
import warnings
from datetime import datetime
from pathlib import Path
from typing import Any, ClassVar, Literal

from beartype import beartype
from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator
from PySide6.QtWidgets import QApplication

from index_service.cli.cli_config import _INDEXER_TYPES, _RESOURCE_TYPES, AppConfig, LoggingConfig
from index_service.gui.collection_views.comfy_input_builder import (
    ComfyInputWidgetBuilder,)
from index_service.gui.collection_views.exif_preview_builder import (
    ExifPreviewrWidgetBuilder,)
from index_service.gui.collection_views.wd_tagger_builder import WdTaggerWidgetBuilder
from index_service.gui.file_tree.base_tree_model import build_file_tree
from index_service.gui.flat_query_preview.window import FlatQueryViewWindow
from index_service.services.core.db import IndexDatabase
from index_service.services.core.indexing_flow import run_indexing_per_root_plan
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import BaseIndexer, RunContext
import commentjson

from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer
from index_service.services.indexers.exif_metadata import ExifMetadataIndexer
from index_service.services.indexers.wd_indexer import WdTagIndexer
from index_service.services.log_config import JsonlFormatter, keep_last_files

from index_service.services.resources.wd_tagger import WdTagger
from index_service.services.utils import (
    dump_with_type,
    get_custom_traceback_handler,
    get_xdg_cache_dir,
    stfu_logs,
)

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)
log = logging.getLogger(__name__)


def showwarning(message, category, filename, lineno, file=None, line=None):
    print(f"{filename}:{lineno}: {category.__name__}: {message}")
    traceback.print_stack()


warnings.showwarning = showwarning


@beartype
class IndexService():

    def get_indexer(self, s: str) -> BaseIndexer:
        for i in self.indexer_instances:
            if i.asset_name == s:
                return i

        assert False, f"No indexer named {s}"

    def __init__(self, cfg: Path) -> None:
        self.cfg = self._load_config(cfg)
        self.indexer_instances = list()
        self.resource_instances = list()

        self.resource_instances = []
        for t in _RESOURCE_TYPES:
            key = t.resource_key
            if key not in self.cfg.resources:
                continue

            for dep in t.required_resources:
                if dep not in self.cfg.resources:
                    raise ValueError(
                        f"Resource '{key}' requires resource '{dep}' to be enabled")

            resource_cfg = self.cfg.resources[key].model_dump()
            self.resource_instances.append(t(**resource_cfg))

        self.indexer_instances: list[BaseIndexer] = []
        for t in _INDEXER_TYPES:
            key = t.asset_name
            if key not in self.cfg.indexers:
                continue

            for dep in t.required_resources:
                if dep not in self.cfg.resources:
                    raise ValueError(
                        f"Indexer '{key}' requires resource '{dep}' to be enabled")

            for dep in t.required_assets:
                if dep not in self.cfg.indexers:
                    raise ValueError(
                        f"Indexer '{key}' requires indexer '{dep}' to be enabled")

            should_load_cache = not self.cfg.enable_cache or key in self.cfg.enable_cache
            log.info(f"Should load cache for {t.asset_name}: {should_load_cache}")
            indexer_cfg = self.cfg.indexers[key].model_dump()
            instance = t(
                should_load_cache=should_load_cache,
                **indexer_cfg,
            )
            self.indexer_instances.append(instance)

    def _setup_runtime_logging(self, log_cfg: LoggingConfig) -> tuple[Path, Path, Path]:
        run_text_dir = get_xdg_cache_dir(["logs", "run", "text"])
        run_json_dir = get_xdg_cache_dir(["logs", "run", "json"])
        perf_dir = get_xdg_cache_dir(["logs", "perf"])

        run_text_dir.mkdir(parents=True, exist_ok=True)
        run_json_dir.mkdir(parents=True, exist_ok=True)
        perf_dir.mkdir(parents=True, exist_ok=True)

        timestamp = datetime.now().isoformat()
        text_log_format = (
            "%(asctime)s %(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s")

        root_logger = logging.getLogger()
        root_logger.setLevel(logging.DEBUG)

        run_text_file = run_text_dir / f"{timestamp}.log"
        run_text_handler = logging.FileHandler(run_text_file, mode="w+")
        run_text_handler.setFormatter(logging.Formatter(text_log_format))
        root_logger.addHandler(run_text_handler)

        run_json_file = run_json_dir / f"{timestamp}.jsonl"
        run_json_handler = logging.FileHandler(run_json_file, mode="w+")
        run_json_handler.setFormatter(JsonlFormatter())
        root_logger.addHandler(run_json_handler)

        if log_cfg.logfile is not None:
            user_handler = logging.FileHandler(str(log_cfg.logfile), mode="w+")
            if log_cfg.logfile_format == "json":
                user_handler.setFormatter(JsonlFormatter())
            else:
                user_handler.setFormatter(logging.Formatter(text_log_format))
            root_logger.addHandler(user_handler)

        keep_last_files(run_text_dir, "*.log", 20)
        keep_last_files(run_json_dir, "*.jsonl", 20)
        keep_last_files(perf_dir, "*.json", 20)

        return run_text_dir, run_json_dir, perf_dir

    def _run_index(self) -> None:

        assert self.cfg.index is not None
        index_cfg = self.cfg.index

        handler = get_custom_traceback_handler(show_args=False)
        _, _, perf_dir = self._setup_runtime_logging(self.cfg.logging)

        log.debug(json.dumps(dump_with_type(index_cfg), indent=2))

        if index_cfg.reset:
            IndexDatabase.reset_database(
                host=self.cfg.db.host,
                db_name=self.cfg.db.db_name,
                username=self.cfg.db.username,
                password=self.cfg.db.password,
            )

        def impl(exc_type: Any, exc_value: Any, exc_traceback: Any):
            print(handler(exc_type, exc_value, exc_traceback))

        sys.excepthook = impl

        db = IndexDatabase(
            host=self.cfg.db.host,
            db_name=self.cfg.db.db_name,
            username=self.cfg.db.username,
            password=self.cfg.db.password,
        )

        ctx = RunContext(db)
        ctx.start_trace()

        assert set(t.asset_name for t in self.indexer_instances) == set(
            self.cfg.indexers.keys())

        with ctx.trace_scope("create runner"):
            runner = IndexRuntime(
                ctx=ctx,
                db=db,
                indexer_types=self.indexer_instances,
                resource_types=self.resource_instances,
            )

        for idx in self.indexer_instances:
            db.enable_index(idx)

        log.info(f"Enabled indexers {[t.asset_name for t in self.indexer_instances]}")

        try:
            run_indexing_per_root_plan(
                db=db,
                runner=runner,
                ctx=ctx,
                paths=index_cfg.paths,
                indexers=tuple(self.cfg.indexers.keys()),
                limit_total=index_cfg.limit_total,
                limit_per_path=index_cfg.limit_per_path,
            )
        except Exception as ex:
            log.critical(f"{ex}", exc_info=ex)
            raise
        finally:
            assert ctx.writer

            perf_file = perf_dir / f"{datetime.now().isoformat()}.json"
            ctx.writer.save(str(perf_file))
            keep_last_files(perf_dir, "*.json", 20)

            if index_cfg.perf_trace_file is not None:
                log.info(f"Trace file {index_cfg.perf_trace_file}")
                ctx.writer.save(str(index_cfg.perf_trace_file))

    def _run_flat_query_view(self) -> None:
        qt_app = QApplication(sys.argv)

        db = IndexDatabase(
            host=self.cfg.db.host,
            db_name=self.cfg.db.db_name,
            username=self.cfg.db.username,
            password=self.cfg.db.password,
        )

        builders = list()
        for inst in self.indexer_instances:
            match inst:
                case ComfyInputIndexer():
                    builders.append(ComfyInputWidgetBuilder(inst))

                case WdTagIndexer():
                    builders.append(WdTaggerWidgetBuilder(inst))

                case ExifMetadataIndexer():
                    builders.append(ExifPreviewrWidgetBuilder(inst))

        win = FlatQueryViewWindow(
            db,
            collection_names=[t for t in self.cfg.indexers.keys()],
            builders=builders,
        )
        win.show()
        sys.exit(qt_app.exec())

    def _run_tree_view(self) -> None:
        qt_app = QApplication(sys.argv)

        db = IndexDatabase(
            host=self.cfg.db.host,
            db_name=self.cfg.db.db_name,
            username=self.cfg.db.username,
            password=self.cfg.db.password,
        )

        assert self.cfg.file_tree_view
        build_file_tree(
            db=db,
            root_directories=[
                Path(p).expanduser().absolute() for p in self.cfg.file_tree_view.root_dirs
            ],
            indexers=self.indexer_instances,
        )

        # sys.exit(qt_app.exec())

    @staticmethod
    def _load_config(path: Path) -> AppConfig:
        cfg_path = path.expanduser().resolve().absolute()
        payload = commentjson.loads(cfg_path.read_text())
        return AppConfig.model_validate(payload)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("command", help="Which action to execute")
    parser.add_argument("config", type=Path, help="Path to JSON config file")
    args = parser.parse_args()

    stfu_logs()
    service = IndexService(Path(args.config))

    match args.command:
        case "index":
            service._run_index()

        case "flat_query_view":
            service._run_flat_query_view()

        case "file_tree_view":
            service._run_tree_view()

        case _:
            raise ValueError(f"Unexpected command {args.command}")


if __name__ == "__main__":
    main()

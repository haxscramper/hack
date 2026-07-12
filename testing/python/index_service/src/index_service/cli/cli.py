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
from sqlalchemy import event, create_engine, URL

from index_service.cli.cli_config import _INDEXER_TYPES, _RESOURCE_TYPES, AppConfig, LoggingConfig
from index_service.gui.collection_views.comfy_input_builder import (
    ComfyInputWidgetBuilder,)
from index_service.gui.collection_views.exif_preview_builder import (
    ExifPreviewrWidgetBuilder,)
from index_service.gui.collection_views.wd_tagger_builder import WdTaggerWidgetBuilder
from index_service.gui.file_tree.base_tree_model import build_file_tree
from index_service.gui.file_tree.qt_tree_window import FileTreeQueryWindow
from index_service.gui.flat_query_preview.window import FlatQueryViewWindow
from index_service.services.core.db import IndexDatabase
from index_service.services.core.hash_cache import HashCache
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

    def __init__(self, cfg: AppConfig, only_short_curcuit_checks: bool) -> None:
        self.cfg = cfg
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

        self.indexer_connection = self.get_cache_connection(self.cfg.index_cache)
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
                database=self.indexer_connection,
                should_load_cache=should_load_cache,
                **indexer_cfg,
            )
            self.indexer_instances.append(instance)

        self.db = IndexDatabase(
            host=self.cfg.db.host,
            db_name=self.cfg.db.db_name,
            username=self.cfg.db.username,
            password=self.cfg.db.password,
            hash_cache=HashCache(Path(self.cfg.hash_cache).expanduser().absolute()),
            only_short_curcuit_checks=only_short_curcuit_checks,
        )

        self.ctx = RunContext(self.db)

    def get_cache_connection(self, database_path: Path):
        engine = create_engine(
            URL.create("sqlite", database=str(database_path)),
            connect_args={"check_same_thread": False},
        )

        @event.listens_for(engine, "connect")
        def configure_sqlite(dbapi_connection, _connection_record) -> None:
            cursor = dbapi_connection.cursor()
            cursor.execute("PRAGMA busy_timeout = 30000")
            cursor.execute("PRAGMA journal_mode = WAL")
            cursor.execute("PRAGMA synchronous = NORMAL")
            cursor.close()

        return engine

    def setup_runtime_logging(self, log_cfg: LoggingConfig) -> tuple[Path, Path, Path]:
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

    def run_index(self) -> None:

        assert self.cfg.index is not None
        index_cfg = self.cfg.index

        handler = get_custom_traceback_handler(show_args=False)

        log.debug(json.dumps(dump_with_type(index_cfg), indent=2))

        def impl(exc_type: Any, exc_value: Any, exc_traceback: Any):
            print(handler(exc_type, exc_value, exc_traceback))

        sys.excepthook = impl
        self.ctx.start_trace()

        assert set(t.asset_name for t in self.indexer_instances) == set(
            self.cfg.indexers.keys())

        with self.ctx.trace_scope("create runner"):
            runner = IndexRuntime(
                ctx=self.ctx,
                db=self.db,
                indexer_types=self.indexer_instances,
                resource_types=self.resource_instances,
            )

        for idx in self.indexer_instances:
            self.db.enable_index(idx)

        log.info(f"Enabled indexers {[t.asset_name for t in self.indexer_instances]}")

        run_indexing_per_root_plan(
            db=self.db,
            runner=runner,
            ctx=self.ctx,
            paths=index_cfg.paths,
            indexers=tuple(self.cfg.indexers.keys()),
            limit_total=index_cfg.limit_total,
            limit_per_path=index_cfg.limit_per_path,
        )

    def run_flat_query_view(self) -> None:
        qt_app = QApplication(sys.argv)
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
            self.db,
            collection_names=[t for t in self.cfg.indexers.keys()],
            builders=builders,
        )
        win.show()
        sys.exit(qt_app.exec())

    def run_tree_view(self) -> None:
        qt_app = QApplication(sys.argv)

        assert self.cfg.file_tree_view
        win = FileTreeQueryWindow(
            ctx=self.ctx,
            file_tree_view=self.cfg.file_tree_view,
            db=self.db,
            indexer_instances=self.indexer_instances,
        )

        win.show()
        sys.exit(qt_app.exec())


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("command", help="Which action to execute")
    parser.add_argument("config", type=Path, help="Path to JSON config file")
    args = parser.parse_args()

    stfu_logs()

    cfg_path = Path(args.config).expanduser().resolve().absolute()
    payload = commentjson.loads(cfg_path.read_text())
    cfg = AppConfig.model_validate(payload)

    if args.command == "index" and cfg.index and cfg.index.reset:
        IndexDatabase.reset_database(
            host=cfg.db.host,
            db_name=cfg.db.db_name,
            username=cfg.db.username,
            password=cfg.db.password,
        )

    service = IndexService(
        cfg,
        only_short_curcuit_checks=args.command != "index",
    )

    _, _, perf_dir = service.setup_runtime_logging(service.cfg.logging)
    service.ctx.start_trace()
    try:
        match args.command:
            case "index":
                service.run_index()

            case "flat_query_view":
                service.run_flat_query_view()

            case "file_tree_view":
                service.run_tree_view()

            case _:
                raise ValueError(f"Unexpected command {args.command}")

    except Exception as ex:
        log.critical(f"{ex}", exc_info=ex)
        raise

    finally:
        service.db.hash_cache.close()
        perf_file = perf_dir / f"{datetime.now().isoformat()}.json"
        service.ctx.writer.save(str(perf_file))
        keep_last_files(perf_dir, "*.json", 20)

        if service.cfg.perf_trace_file is not None:
            log.info(f"Trace file {service.cfg.perf_trace_file}")
            service.ctx.writer.save(str(service.cfg.perf_trace_file))


if __name__ == "__main__":
    main()

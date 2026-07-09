import argparse
import json
import logging
import sys
import traceback
import warnings
from datetime import datetime
from pathlib import Path
from typing import Any, ClassVar, Literal

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator
from PySide6.QtWidgets import QApplication

from index_service.cli.cli_config import _INDEXER_TYPES, _RESOURCE_TYPES, AppConfig, LoggingConfig
from index_service.gui.collection_views.comfy_input_builder import (
    ComfyInputWidgetBuilder,)
from index_service.gui.collection_views.exif_preview_builder import (
    ExifPreviewrWidgetBuilder,)
from index_service.gui.collection_views.wd_tagger_builder import WdTaggerWidgetBuilder
from index_service.gui.window import MainWindow
from index_service.services.core.db import IndexDatabase
from index_service.services.core.indexing_flow import run_indexing_per_root_plan
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import BaseIndexer, RunContext
import commentjson

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


def _setup_runtime_logging(log_cfg: LoggingConfig) -> tuple[Path, Path, Path]:
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


def _run_index(app_config: AppConfig) -> None:
    assert app_config.index is not None
    cfg = app_config.index

    stfu_logs()
    handler = get_custom_traceback_handler(show_args=False)
    _, _, perf_dir = _setup_runtime_logging(app_config.logging)

    log.debug(json.dumps(dump_with_type(cfg), indent=2))

    if cfg.reset:
        IndexDatabase.reset_database(
            host=app_config.db.host,
            db_name=app_config.db.db_name,
            username=app_config.db.username,
            password=app_config.db.password,
        )

    def impl(exc_type: Any, exc_value: Any, exc_traceback: Any):
        print(handler(exc_type, exc_value, exc_traceback))

    sys.excepthook = impl

    db = IndexDatabase(
        host=app_config.db.host,
        db_name=app_config.db.db_name,
        username=app_config.db.username,
        password=app_config.db.password,
    )

    ctx = RunContext(db)
    ctx.start_trace()

    resource_instances = []
    for t in _RESOURCE_TYPES:
        key = t.resource_key
        if key not in cfg.resources:
            continue

        for dep in t.required_resources:
            if dep not in cfg.resources:
                raise ValueError(
                    f"Resource '{key}' requires resource '{dep}' to be enabled")

        resource_cfg = cfg.resources[key].model_dump()
        resource_instances.append(t(**resource_cfg))

    indexer_instances: list[BaseIndexer] = []
    for t in _INDEXER_TYPES:
        key = t.asset_name
        if key not in cfg.indexers:
            continue

        for dep in t.required_resources:
            if dep not in cfg.resources:
                raise ValueError(
                    f"Indexer '{key}' requires resource '{dep}' to be enabled")

        for dep in t.required_assets:
            if dep not in cfg.indexers:
                raise ValueError(
                    f"Indexer '{key}' requires indexer '{dep}' to be enabled")

        indexer_cfg = cfg.indexers[key].model_dump()
        instance = t(
            should_load_cache=key in cfg.enable_cache,
            **indexer_cfg,
        )
        indexer_instances.append(instance)

    assert set(t.asset_name for t in indexer_instances) == set(cfg.indexers.keys())

    with ctx.trace_scope("create runner"):
        runner = IndexRuntime(
            ctx=ctx,
            db=db,
            indexer_types=indexer_instances,
            resource_types=resource_instances,
        )

    for idx in indexer_instances:
        db.enable_index(idx)

    log.info(f"Enabled indexers {[t.asset_name for t in indexer_instances]}")

    try:
        run_indexing_per_root_plan(
            db=db,
            runner=runner,
            ctx=ctx,
            paths=cfg.paths,
            indexers=tuple(cfg.indexers.keys()),
            limit_total=cfg.limit_total,
            limit_per_path=cfg.limit_per_path,
        )
    except Exception as ex:
        log.critical(f"{ex}", exc_info=ex)
        raise
    finally:
        assert ctx.writer

        perf_file = perf_dir / f"{datetime.now().isoformat()}.json"
        ctx.writer.save(str(perf_file))
        keep_last_files(perf_dir, "*.json", 20)

        if cfg.perf_trace_file is not None:
            log.info(f"Trace file {cfg.perf_trace_file}")
            ctx.writer.save(str(cfg.perf_trace_file))


def _run_view(config: AppConfig) -> None:
    stfu_logs()
    app = QApplication(sys.argv)

    db = IndexDatabase(
        host=config.db.host,
        db_name=config.db.db_name,
        username=config.db.username,
        password=config.db.password,
    )

    win = MainWindow(
        db,
        collection_names=[t.asset_name for t in _INDEXER_TYPES],
        builders=[
            ComfyInputWidgetBuilder(),
            WdTaggerWidgetBuilder(),
            ExifPreviewrWidgetBuilder(),
        ],
    )
    win.show()
    sys.exit(app.exec())


def _load_config(path: Path) -> AppConfig:
    cfg_path = path.expanduser().resolve().absolute()
    payload = commentjson.loads(cfg_path.read_text())
    return AppConfig.model_validate(payload)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("config", type=Path, help="Path to JSON config file")
    args = parser.parse_args()

    app_config = _load_config(args.config)

    if app_config.index is not None:
        _run_index(app_config)
    else:
        _run_view(app_config)


if __name__ == "__main__":
    main()

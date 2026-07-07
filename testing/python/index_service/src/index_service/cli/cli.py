import argparse
import json
import logging
import os
import sys
import traceback
import warnings
from datetime import datetime
from pathlib import Path
from typing import Any, ClassVar, Literal

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator
from PySide6.QtWidgets import QApplication

from index_service.gui.collection_views.comfy_input_builder import (
    ComfyInputWidgetBuilder,)
from index_service.gui.collection_views.exif_preview_builder import (
    ExifPreviewrWidgetBuilder,)
from index_service.gui.collection_views.wd_tagger_builder import WdTaggerWidgetBuilder
from index_service.gui.window import MainWindow
from index_service.services.core.db import IndexDatabase
from index_service.services.core.indexing_flow import run_indexing_per_root_plan
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import RunContext
import commentjson
from index_service.services.default_job_types import (
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)
from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer
from index_service.services.indexers.exif_metadata import ExifMetadataIndexer
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer
from index_service.services.indexers.full_document.full_document import DocumentBlockIndexer
from index_service.services.indexers.image_generation import GenerationParamsIndexer
from index_service.services.indexers.media_transcribe import MediaTranscriptionIndexer
from index_service.services.indexers.pdf_indexer import PdfIndexer
from index_service.services.indexers.safetensor_indexer import SafetensorIndexer
from index_service.services.indexers.wd_indexer import WdTagIndexer
from index_service.services.log_config import JsonlFormatter, keep_last_files
from index_service.services.resources.flm_server import FlmServerResource
from index_service.services.resources.pdf.pdf_extractor import PdfExtractor
from index_service.services.resources.wd_tagger import WdTagger
from index_service.services.resources.whisper_transcribe import WhisperTranscribeResource
from index_service.services.utils import (
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

_INDEXER_TYPES = [t for t in DEFAULT_INDEXER_TYPES] + [
    WdTagIndexer,
    PdfIndexer,
    FFProbeIndexer,
    SafetensorIndexer,
    GenerationParamsIndexer,
    ExifMetadataIndexer,
    ComfyInputIndexer,
    DocumentBlockIndexer,
    MediaTranscriptionIndexer,
]

_RESOURCE_TYPES = [t for t in DEFAULT_RESOURCE_TYPES] + [
    FlmServerResource,
    WdTagger,
    PdfExtractor,
    WhisperTranscribeResource,
]

_INDEXER_BY_NAME = {cls.asset_name: cls for cls in _INDEXER_TYPES}
_RESOURCE_BY_NAME = {cls.resource_key: cls for cls in _RESOURCE_TYPES}


def _validate_plugin_config_map(
    raw: dict[str, Any],
    registry: dict[str, type],
    kind: str,
) -> dict[str, BaseModel]:
    validated: dict[str, BaseModel] = {}
    for name, cfg in raw.items():
        if name not in registry:
            raise ValueError(f"Unknown {kind} '{name}'")

        cls = registry[name]
        config_model = getattr(cls, "config_model", None)
        if not isinstance(config_model, type) or not issubclass(config_model, BaseModel):
            raise TypeError(f"{kind.capitalize()} '{name}' has invalid config_model")

        if cfg is None:
            cfg = {}
        if not isinstance(cfg, dict):
            raise TypeError(
                f"{kind.capitalize()} '{name}' config must be an object, got {type(cfg).__name__}"
            )

        validated[name] = config_model.model_validate(cfg)

    return validated


class DatabaseConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")

    host: str = "http://localhost:8529"
    db_name: str
    username: str = "root"
    password: str = "test"

    @field_validator("db_name")
    @classmethod
    def _db_name_not_empty(cls, value: str) -> str:
        if not value.strip():
            raise ValueError("db_name must not be empty")
        return value


class LoggingConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")

    logfile: Path | None = None
    logfile_format: Literal["text", "json"] = "text"

    @field_validator("logfile")
    @classmethod
    def _normalize_logfile(cls, value: Path | None) -> Path | None:
        if value is None:
            return None
        return value.expanduser().resolve().absolute()


class IndexConfig(BaseModel):
    model_config = ConfigDict(extra="forbid", arbitrary_types_allowed=True)

    paths: tuple[Path, ...] = ()
    reset: bool = False
    limit_total: int | None = None
    limit_per_path: int | None = None
    perf_trace_file: Path | None = None
    enable_cache: set[str] | None = None

    # Raw input maps from JSON.
    raw_indexers: dict[str, Any] = Field(default_factory=dict)
    raw_resources: dict[str, Any] = Field(default_factory=dict)

    # Validated typed models (populated during validation).
    indexers: dict[str, BaseModel] = Field(default_factory=dict, exclude=True)
    resources: dict[str, BaseModel] = Field(default_factory=dict, exclude=True)

    media_transcribe_whisper_server: Path | None = None

    @model_validator(mode="before")
    @classmethod
    def _defaults_for_plugin_maps(cls, data: Any) -> Any:
        if not isinstance(data, dict):
            return data

        if "indexers" not in data:
            data["indexers"] = {name: {} for name in _INDEXER_BY_NAME}
        if "resources" not in data:
            data["resources"] = {name: {} for name in _RESOURCE_BY_NAME}
        return data

    @field_validator("paths", mode="before")
    @classmethod
    def _normalize_paths_before(cls, value: Any) -> Any:
        if value is None:
            return ()
        return value

    @field_validator("paths")
    @classmethod
    def _normalize_paths(cls, value: tuple[Path, ...]) -> tuple[Path, ...]:
        return tuple(p.expanduser().resolve().absolute() for p in value)

    @field_validator("perf_trace_file")
    @classmethod
    def _normalize_perf_trace_file(cls, value: Path | None) -> Path | None:
        if value is None:
            return None
        return value.expanduser().resolve().absolute()

    @field_validator("media_transcribe_whisper_server")
    @classmethod
    def _validate_whisper_server(cls, value: Path | None) -> Path | None:
        if value is None:
            env_value = os.environ.get("HAXDEX_MEDIA_TRANSCRIBE_WHISPER_SERVER")
            if env_value:
                value = Path(env_value)

        if value is None:
            return None

        value = value.expanduser().resolve().absolute()
        if not value.exists():
            raise ValueError(f"media_transcribe_whisper_server does not exist: {value}")
        if not os.access(value, os.X_OK):
            raise ValueError(
                f"media_transcribe_whisper_server is not executable: {value}")
        return value

    @model_validator(mode="after")
    def _validate_plugin_configs(self) -> "IndexConfig":
        self.indexers = _validate_plugin_config_map(
            raw=self.raw_indexers,
            registry=_INDEXER_BY_NAME,
            kind="indexer",
        )
        self.resources = _validate_plugin_config_map(
            raw=self.raw_resources,
            registry=_RESOURCE_BY_NAME,
            kind="resource",
        )

        if self.enable_cache is None:
            self.enable_cache = set(self.raw_indexers.keys())

        unknown_cache = set(self.enable_cache) - set(self.raw_indexers.keys())
        if unknown_cache:
            raise ValueError(
                f"enable_cache contains unknown/disabled indexers: {sorted(unknown_cache)}"
            )

        return self


class ViewConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")
    # placeholder for future view-specific options
    pass


class AppConfig(BaseModel):
    model_config = ConfigDict(extra="forbid")

    db: DatabaseConfig
    logging: LoggingConfig = Field(default_factory=LoggingConfig)

    # exactly one of these must be set
    index: IndexConfig | None = None
    view: ViewConfig | None = None

    @model_validator(mode="after")
    def _validate_mode_payload(self) -> "AppConfig":
        has_index = self.index is not None
        has_view = self.view is not None

        if has_index == has_view:
            raise ValueError("Exactly one of 'config' (index mode) or 'view' must be set")
        return self


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
    index_cfg = app_config.index

    stfu_logs()
    handler = get_custom_traceback_handler(show_args=False)
    _, _, perf_dir = _setup_runtime_logging(app_config.logging)

    if index_cfg.media_transcribe_whisper_server is not None:
        os.environ["HAXDEX_MEDIA_TRANSCRIBE_WHISPER_SERVER"] = str(
            index_cfg.media_transcribe_whisper_server)

    if index_cfg.reset:
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

    enabled_resources = set(index_cfg.raw_resources.keys())
    enabled_indexers = set(index_cfg.raw_indexers.keys())

    resource_instances = []
    for t in _RESOURCE_TYPES:
        key = t.resource_key
        if key not in enabled_resources:
            continue

        for dep in t.required_resources:
            if dep not in enabled_resources:
                raise ValueError(
                    f"Resource '{key}' requires resource '{dep}' to be enabled")

        resource_cfg = index_cfg.resources[key].model_dump()

        if t == WdTagger:
            instance = WdTagger.from_huggingface(**resource_cfg)
        else:
            instance = t(**resource_cfg)

        resource_instances.append(instance)

    indexer_instances = []
    for t in _INDEXER_TYPES:
        key = t.asset_name
        if key not in enabled_indexers:
            continue

        for dep in t.required_resources:
            if dep not in enabled_resources:
                raise ValueError(
                    f"Indexer '{key}' requires resource '{dep}' to be enabled")

        for dep in t.required_assets:
            if dep not in enabled_indexers:
                raise ValueError(
                    f"Indexer '{key}' requires indexer '{dep}' to be enabled")

        indexer_cfg = index_cfg.indexers[key].model_dump()
        instance = t(
            should_load_cache=key in index_cfg.enable_cache,
            **indexer_cfg,
        )
        indexer_instances.append(instance)

    with ctx.trace_scope("create runner"):
        runner = IndexRuntime(
            ctx=ctx,
            db=db,
            indexer_types=indexer_instances,
            resource_types=resource_instances,
        )

    for idx in indexer_instances:
        db.enable_index(idx)

    try:
        run_indexing_per_root_plan(
            db=db,
            runner=runner,
            ctx=ctx,
            paths=index_cfg.paths,
            indexers=tuple(index_cfg.raw_indexers.keys()),
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

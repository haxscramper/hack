import os
import json
from pathlib import Path
from beartype import beartype
from beartype.typing import Any, Literal

from index_service.services.default_job_types import (
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)
from pydantic import BaseModel, field_validator, Field, ConfigDict, model_validator
from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer
from index_service.services.indexers.exif_metadata import ExifMetadataIndexer
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer
from index_service.services.indexers.full_document.full_document import DocumentBlockIndexer
from index_service.services.indexers.image_generation import GenerationParamsIndexer
from index_service.services.indexers.media_transcribe import MediaTranscriptionIndexer
from index_service.services.indexers.pdf_indexer import PdfIndexer
from index_service.services.indexers.safetensor_indexer import SafetensorIndexer
from index_service.services.indexers.wd_indexer import WdTagIndexer
from index_service.services.resources.flm_server import FlmServerResource
from index_service.services.resources.pdf.pdf_extractor import PdfExtractor
from index_service.services.resources.wd_tagger import WdTagger
from index_service.services.resources.whisper_transcribe import WhisperTranscribeResource

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

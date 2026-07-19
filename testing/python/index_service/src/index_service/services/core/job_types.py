from __future__ import annotations

import contextlib
import json
import logging
import os
import re
import threading
import time as _time
from abc import ABC, abstractmethod
from pathlib import Path

from accelerate.commands.config.default import description
from beartype import beartype
from beartype.typing import Any, ClassVar, Optional, ParamSpec, TypeVar
from pydantic import BaseModel, Field
from sqlalchemy import (
    JSON,
    Column,
    DateTime,
    Float,
    MetaData,
    String,
    Table,
    Engine,
)

from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import (
    FileRef,
    IndexerOutput,
    IndexerRequest,
    MultiDocumentModel,
)
from index_service.services.pydantic_utils import (
    to_json_safe,)

log = logging.getLogger(__name__)


class TraceWriter:

    def __init__(self):
        self.events = []
        self.lock = threading.Lock()

    def begin(self, name: str, **args):
        self._event(name, "B", args)

    def end(self, name: str, **args):
        self._event(name, "E", args)

    def instant(self, name: str, **args):
        self._event(name, "i", args)

    def _event(self, name: str, phase: str, args):
        ev = {
            "name": name,
            "ph": phase,
            "ts": _time.perf_counter() * 1_000_000,
            "pid": os.getpid(),
            "tid": threading.get_ident(),
        }
        if args:
            ev["args"] = args
        with self.lock:
            self.events.append(ev)

    def save(self, path: str):
        with open(path, "w") as f:
            json.dump({"traceEvents": to_json_safe(self.events)}, f)


@beartype
class RunContext:

    def __init__(self, db: IndexDatabase) -> None:
        self._db = db
        self.writer = None

    def get_path(self, ref: FileRef) -> Path:
        return self._db.get_path(ref)

    def start_trace(self):
        self.writer = TraceWriter()

    @contextlib.contextmanager
    def trace_scope(self, message: str, **args):
        if self.writer:
            self.writer.begin(message, **args)
            try:
                yield
            finally:
                self.writer.end(message)
        else:
            yield


class BaseResourceConfig(BaseModel, extra="forbid"):
    pass


@beartype
class BaseResource(ABC):
    resource_key: str
    required_resources: tuple[str, ...] = ()
    exclusive: bool = False
    config_model: ClassVar[type[BaseModel]] = BaseResourceConfig

    def __init__(self, config: BaseResourceConfig):
        self.config = config

    @abstractmethod
    def handle(
        self,
        ctx: RunContext,
        request: BaseModel,
        resources: dict[str, BaseResource],
    ) -> BaseModel:
        raise NotImplementedError


P = ParamSpec("P")
R = TypeVar("R", bound="IndexerOutput")


class BaseIndexerConfig(BaseModel, extra="forbid"):
    use_cache: bool = True
    reset_db_collection: bool = Field(
        default=False, description="Reset the indexer collecitions in arango DB")


def indexer_cache_table_name(asset_name: str) -> str:
    normalized = re.sub(r"[^a-zA-Z0-9_]", "_", asset_name)
    return f"indexer_cache_{normalized}"


def get_indexer_cache_table(asset_name: str) -> Table:
    metadata = MetaData()

    return Table(
        indexer_cache_table_name(asset_name),
        metadata,
        Column("file_hash", String, primary_key=True),
        Column("schema_hash", String, nullable=False),
        Column("result", JSON, nullable=False),
        Column("function_started_at", DateTime(timezone=True), nullable=False),
        Column("function_duration_seconds", Float, nullable=False),
    )


@beartype
class BaseIndexer(ABC):
    asset_name: str
    result_model: type[BaseModel]
    required_assets: tuple[str, ...] = ()
    required_resources: tuple[str, ...] = ()
    max_parallel: int = 1
    edge_collection_name: Optional[str] = None
    config_model: ClassVar[type[BaseModel]] = BaseIndexerConfig
    "Pydantic data class with object that holds all the configuration parameters"

    def get_document_type_bases(self) -> list[Any]:
        if issubclass(self.result_model, MultiDocumentModel):
            return [self.result_model.document_type]

        else:
            return [self.result_model]

    @property
    def reset_db_collection(self) -> bool:
        return self.config.reset_db_collection

    @property
    def should_load_cache(self) -> bool:
        return self.config.use_cache

    def __init__(self, database: Engine, config: BaseIndexerConfig) -> None:
        self.database = database
        self.config = config
        self.cache_table = get_indexer_cache_table(self.asset_name)
        self.cache_table.create(bind=self.database, checkfirst=True)

    def can_run(self, path: Path) -> bool:
        return True

    def hash_run_parameters(
        self,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> int:
        return 0

    @abstractmethod
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        raise NotImplementedError

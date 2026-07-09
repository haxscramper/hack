from __future__ import annotations

import contextlib
import hashlib
import json
import os
import threading
import time as _time
from abc import ABC, abstractmethod
from functools import wraps
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, Callable, ClassVar, Optional, ParamSpec, TypeVar
from pydantic import BaseModel

from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import (
    ConverterOutput,
    ConverterRequest,
    FileRef,
    IndexDocument,
    IndexMultiDocument,
    IndexerOutput,
    IndexerRequest,
    MultiDocumentModel,
)
from index_service.services.pydantic_utils import (
    model_from_json_data,
    model_to_json_data,
    to_json_safe,
)
from index_service.services.utils import ExceptionContextNote, get_xdg_cache_dir
import logging

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


def cache_indexer_run(func: Callable[P, R]) -> Callable[P, IndexerOutput]:

    @wraps(func)
    def wrapper(
        self: BaseIndexer,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        hash_value = request.file_ref.hash.hash
        hash_prefix = hash_value[:2]
        hash_suffix = hash_value[2:]

        param_hash = hashlib.sha256(
            str(
                self.hash_run_parameters(
                    request=request,
                    resources=resources,
                    assets=assets,
                )).encode("utf-8")).hexdigest()

        schema_hash = hashlib.sha256(
            json.dumps(
                self.result_model.model_json_schema(),
                sort_keys=True,
                separators=(",", ":"),
            ).encode("utf-8")).hexdigest()

        cache_path = get_xdg_cache_dir(
            ["indexer", self.asset_name, hash_prefix,
             hash_suffix]).joinpath(f"{param_hash}.json")

        if cache_path.exists() and self.should_load_cache:
            with (
                    ExceptionContextNote(f"loading JSON cache from {cache_path}"),
                    ctx.trace_scope(
                        "load cache file",
                        file=cache_path,
                    ),
            ):
                cache_doc = json.loads(cache_path.read_text())
                cached_schema_hash = (cache_doc.pop("__schema_hash__", None)
                                      if isinstance(cache_doc, dict) else None)

                if cached_schema_hash == schema_hash:
                    parsed = model_from_json_data(
                        cache_doc,
                        IndexerOutput,
                    )
                    assert parsed.indexer_id == self.asset_name
                    result_value = self.result_model.model_validate(parsed.result)

                    return IndexerOutput(
                        indexer_id=self.asset_name,
                        result=result_value,
                    )

                log.info(
                    "Cache schema mismatch for {} (cached={}, current={}), ignoring cache."
                    .format(cache_path, cached_schema_hash, schema_hash))

        result = func(
            self,  # type: ignore
            ctx=ctx,  # type: ignore
            request=request,  # type: ignore
            resources=resources,  # type: ignore
            assets=assets,  # type: ignore
        )

        cache_doc = model_to_json_data(result)
        assert isinstance(cache_doc, dict)
        cache_doc["__schema_hash__"] = schema_hash

        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_text(json.dumps(cache_doc, indent=2))
        return result

    return wrapper


class BaseIndexerConfig(BaseModel, extra="forbid"):
    pass


@beartype
class BaseIndexer(ABC):
    asset_name: str
    result_model: type[BaseModel]
    required_assets: tuple[str, ...] = ()
    required_resources: tuple[str, ...] = ()
    max_parallel: int = 1
    should_load_cache: bool = True
    edge_collection_name: Optional[str] = None
    config_model: ClassVar[type[BaseModel]] = BaseIndexerConfig
    "Pydantic data class with object that holds all the configuration parameters"

    def get_document_type_bases(self) -> list[Any]:
        if issubclass(self.result_model, MultiDocumentModel):
            return [self.result_model.document_type]

        else:
            return [self.result_model]

    def __init__(self, **kwargs) -> None:
        for key, value in dict(**kwargs).items():
            setattr(self, key, value)

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


@beartype
class BaseConverter(ABC):
    converter_id: str
    result_model: type[BaseModel]
    required_assets: tuple[str, ...] = ()
    required_resources: tuple[str, ...] = ()

    @abstractmethod
    def run(
        self,
        ctx: RunContext,
        request: ConverterRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> ConverterOutput:
        raise NotImplementedError

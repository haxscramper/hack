from __future__ import annotations

from abc import ABC, abstractmethod
from datetime import time
import json
import os
from pathlib import Path
import threading
from beartype.typing import Any, Callable, Optional, ParamSpec, TypeVar, cast
import hashlib
from functools import wraps

from beartype import beartype
from pydantic import BaseModel
import contextlib

from index_service.services.db import IndexDatabase
from index_service.services.pydantic_utils import model_from_json_data, model_to_json_data, to_json_safe
from index_service.services.types import (
    ConverterOutput,
    ConverterRequest,
    FileRef,
    IndexerOutput,
    IndexerRequest,
)
from index_service.services.utils import ExceptionContextNote, get_xdg_cache_dir
import time as _time


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
            "ts": _time.perf_counter() * 1_000_000,  # microseconds
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
class RunContext():

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


@beartype
class BaseResource(ABC):
    resource_key: str

    @abstractmethod
    def handle(self, ctx: RunContext, request: BaseModel) -> BaseModel:
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
        hash = request.file_ref.hash.hash
        hash_prefix = hash[:2]
        hash_suffix = hash[2:]

        param_hash = hashlib.sha256(
            str(
                self.hash_run_parameters(
                    request=request,
                    resources=resources,
                    assets=assets,
                )).encode("utf-8")).hexdigest()

        cache_path = get_xdg_cache_dir([
            "indexer",
            self.asset_name,
            hash_prefix,
            hash_suffix,
        ]).joinpath(f"{param_hash}.json")

        if cache_path.exists() and self.should_load_cache:
            with ExceptionContextNote(
                    f"loading JSON cache from {cache_path}"), ctx.trace_scope(
                        "load cache file", file=cache_path):
                parsed = model_from_json_data(
                    json.loads(cache_path.read_text()),
                    IndexerOutput,
                )

                assert parsed.indexer_id == self.asset_name, (
                    f"Failed to de-serialized, parsed text indexer was {parsed.indexer_id}"
                )

                result_value = self.result_model.model_validate(parsed.result)

            return IndexerOutput(
                indexer_id=self.asset_name,
                result=result_value,
            )

        result = func(
            self,  # type: ignore
            ctx=ctx,  # type: ignore
            request=request,  # type: ignore
            resources=resources,  # type: ignore
            assets=assets  # type: ignore
        )

        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_text(
            json.dumps(
                model_to_json_data(result),
                indent=2,
            ))
        return result

    return wrapper


@beartype
class BaseIndexer(ABC):
    asset_name: str
    result_model: type[BaseModel]
    required_assets: tuple[str, ...] = ()
    required_resources: tuple[str, ...] = ()
    max_parallel: int = 1
    should_load_cache: bool = True

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

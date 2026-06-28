from __future__ import annotations

from abc import ABC, abstractmethod
import json
from pathlib import Path
from beartype.typing import Any, Callable, Optional, ParamSpec, TypeVar, cast
import hashlib
from functools import wraps

from beartype import beartype
from pydantic import BaseModel
from viztracer import VizTracer

from index_service.services.types import (
    ConverterOutput,
    ConverterRequest,
    IndexerOutput,
    IndexerRequest,
)
from index_service.services.utils import get_xdg_cache_dir


@beartype
class RunContext():
    tracer: VizTracer

    def __init__(self) -> None:
        self.tracer = VizTracer(tracer_entries=0)


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
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        md5 = request.file_ref.md5.md5
        md5_prefix = md5[:2]
        md5_suffix = md5[2:]

        param_hash = hashlib.md5(
            str(
                self.hash_run_parameters(
                    request=request,
                    resources=resources,
                    assets=assets,
                )).encode("utf-8")).hexdigest()

        cache_path = (get_xdg_cache_dir([self.asset_name]) / md5_prefix /
                      md5_suffix / f"{param_hash}.json")

        if cache_path.exists() and self.should_load_cache(
                request=request,
                resources=resources,
                assets=assets,
        ):
            parsed_text = json.loads(cache_path.read_text())
            assert parsed_text["indexer_id"] == self.asset_name, (
                f"Failed to de-serialized, parsed text indexer was {parsed_text['indexer_id']}"
            )
            result_value = self.result_model.model_validate(
                parsed_text["result"])

            return IndexerOutput(
                indexer_id=self.asset_name,
                result=result_value,
            )

        result = func(self, request, resources, assets)  # type: ignore

        payload = {
            "indexer_id": result.indexer_id,
            "result": json.loads(result.result.model_dump_json()),
        }

        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_text(json.dumps(payload, indent=2))

        return result

    return wrapper


@beartype
class BaseIndexer(ABC):
    asset_name: str
    result_model: type[BaseModel]
    required_assets: tuple[str, ...] = ()
    required_resources: tuple[str, ...] = ()
    max_parallel: int = 1

    def can_run(self, path: Path) -> bool:
        return True

    def should_load_cache(
        self,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> bool:
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

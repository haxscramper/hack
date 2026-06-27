from __future__ import annotations

from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any

from beartype import beartype
from pydantic import BaseModel

from index_service.protocol import (
    ConverterRequest,
    ConverterOutput,
    IndexerOutput,
    IndexerRequest,
)


@beartype
class BaseResource(ABC):
    resource_key: str

    @abstractmethod
    def handle(self, request: BaseModel) -> BaseModel:
        raise NotImplementedError


@beartype
class BaseIndexer(ABC):
    asset_name: str
    result_model: type[BaseModel]
    dependencies: tuple[str, ...] = ()
    required_resources: tuple[str, ...] = ()

    def can_run(self, path: Path) -> bool:
        return True

    @abstractmethod
    def run(self, request: IndexerRequest, **resources: Any) -> IndexerOutput:
        raise NotImplementedError


@beartype
class BaseConverter(ABC):
    converter_id: str
    result_model: type[BaseModel]
    required_resources: tuple[str, ...] = ()

    @abstractmethod
    def run(self, request: ConverterRequest,
            **resources: Any) -> ConverterOutput:
        raise NotImplementedError

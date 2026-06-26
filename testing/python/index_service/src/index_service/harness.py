from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TypeVar

import pykka
from beartype import beartype
from pydantic import BaseModel

from index_service.protocol import (
    ConverterOutput,
    ConverterRequest,
    IndexerOutput,
    IndexerRequest,
)

RespT = TypeVar("RespT", bound=BaseModel)


@beartype
class BaseServiceActor(pykka.ThreadingActor, ABC):
    actor_id: str


@beartype
class BaseResourceActor(BaseServiceActor, ABC):

    @abstractmethod
    def handle(self, request: BaseModel) -> BaseModel:
        raise NotImplementedError


@beartype
class BaseIndexerActor(BaseServiceActor, ABC):
    dependencies: tuple[str, ...] = ()
    required_resources: tuple[str, ...] = ()

    def __init__(self, resources: dict[str, pykka.ActorRef]) -> None:
        super().__init__()
        self._resources = {
            name: ref.proxy()
            for name, ref in resources.items()
        }

    @abstractmethod
    def handle(self, request: IndexerRequest) -> IndexerOutput:
        raise NotImplementedError

    def request_resource(
        self,
        name: str,
        request: BaseModel,
        response_type: type[RespT],
    ) -> RespT:
        raw = self._resources[name].handle(request).get()
        if isinstance(raw, response_type):
            return raw
        return response_type.model_validate(raw)


@beartype
class BaseConverterActor(BaseServiceActor, ABC):
    required_resources: tuple[str, ...] = ()

    def __init__(self, resources: dict[str, pykka.ActorRef]) -> None:
        super().__init__()
        self._resources = {
            name: ref.proxy()
            for name, ref in resources.items()
        }

    @abstractmethod
    def handle(self, request: ConverterRequest) -> ConverterOutput:
        raise NotImplementedError

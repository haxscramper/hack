from __future__ import annotations

from collections.abc import Sequence

import pykka

from index_service.harness import (
    BaseConverterActor,
    BaseIndexerActor,
    BaseResourceActor,
)
from index_service.protocol import (
    ConverterOutput,
    ConverterRequest,
    FileRef,
    IndexerOutput,
    IndexerRequest,
)
from index_service.registry import DEFAULT_ACTOR_TYPES


class IndexRuntime:

    def __init__(
        self,
        actor_types: Sequence[type] | None = None,
        resource_overrides: dict[str, type[BaseResourceActor]] | None = None,
    ) -> None:
        types = list(actor_types or DEFAULT_ACTOR_TYPES)
        self._started: list[pykka.ActorRef] = []

        resource_types: dict[str, type[BaseResourceActor]] = {}
        indexer_types: dict[str, type[BaseIndexerActor]] = {}
        converter_types: dict[str, type[BaseConverterActor]] = {}

        for actor_cls in types:
            if issubclass(actor_cls, BaseResourceActor):
                resource_types[actor_cls.actor_id] = actor_cls
            elif issubclass(actor_cls, BaseIndexerActor):
                indexer_types[actor_cls.actor_id] = actor_cls
            elif issubclass(actor_cls, BaseConverterActor):
                converter_types[actor_cls.actor_id] = actor_cls
            else:
                raise TypeError(f"unsupported actor type: {actor_cls}")

        if resource_overrides:
            resource_types.update(resource_overrides)

        self._resource_refs: dict[str, pykka.ActorRef] = {}
        for name, cls in resource_types.items():
            ref = cls.start()
            self._resource_refs[name] = ref
            self._started.append(ref)

        self._indexer_refs: dict[str, pykka.ActorRef] = {}
        self._indexer_proxies = {}
        self._indexer_types = indexer_types
        for name, cls in indexer_types.items():
            deps = {r: self._resource_refs[r] for r in cls.required_resources}
            ref = cls.start(resources=deps)
            self._indexer_refs[name] = ref
            self._indexer_proxies[name] = ref.proxy()
            self._started.append(ref)

        self._converter_refs: dict[str, pykka.ActorRef] = {}
        self._converter_proxies = {}
        for name, cls in converter_types.items():
            deps = {r: self._resource_refs[r] for r in cls.required_resources}
            ref = cls.start(resources=deps)
            self._converter_refs[name] = ref
            self._converter_proxies[name] = ref.proxy()
            self._started.append(ref)

    def stop(self) -> None:
        for ref in reversed(self._started):
            ref.stop()

    def run_indexers(self, file_ref: FileRef,
                     names: list[str]) -> dict[str, IndexerOutput]:
        order: list[str] = []
        visited: set[str] = set()
        visiting: set[str] = set()

        def visit(name: str) -> None:
            if name in visited:
                return
            if name in visiting:
                raise RuntimeError(f"cyclic indexer dependency at {name}")
            visiting.add(name)
            cls = self._indexer_types[name]
            for dep in cls.dependencies:
                visit(dep)
            visiting.remove(name)
            visited.add(name)
            order.append(name)

        for name in names:
            visit(name)

        done: dict[str, IndexerOutput] = {}
        for name in order:
            req = IndexerRequest(file_ref=file_ref, dependency_results=done)
            out = self._indexer_proxies[name].handle(req).get()
            done[name] = (out if isinstance(out, IndexerOutput) else
                          IndexerOutput.model_validate(out))
        return {name: done[name] for name in names}

    def run_converter(
        self,
        converter_name: str,
        input_files: list[str],
        param: str = "",
    ) -> ConverterOutput:
        req = ConverterRequest(input_files=input_files, param=param)
        out = self._converter_proxies[converter_name].handle(req).get()
        return (out if isinstance(out, ConverterOutput) else
                ConverterOutput.model_validate(out))

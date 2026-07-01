from __future__ import annotations

import logging
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from graphlib import TopologicalSorter

from beartype import beartype
from beartype.typing import Sequence, overload

from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import (
    BaseConverter,
    BaseIndexer,
    BaseResource,
    RunContext,
)
from index_service.services.core.types import (
    ConverterOutput,
    ConverterRequest,
    FileHash,
    FileRef,
    IndexerOutput,
    IndexerRequest,
)
from index_service.services.utils import ExceptionContextNote

log = logging.getLogger(__name__)


@beartype
@dataclass(frozen=True)
class PlannedIndexerBatch:
    indexer_name: str
    file_refs: list[FileRef]
    sub_batches: list[list[FileRef]]
    window_id: int


@beartype
@dataclass(frozen=True)
class ExecutionPlan:
    batches: list[PlannedIndexerBatch]

    def get_indexer_names(self) -> set[str]:
        return {it.indexer_name for it in self.batches}


@beartype
class IndexRuntime:

    def __init__(
            self,
            ctx: RunContext,
            db: IndexDatabase,
            indexer_types: Sequence[BaseIndexer] = list(),
            converter_types: Sequence[BaseConverter] = list(),
            resource_types: Sequence[BaseResource] = list(),
    ) -> None:
        self.db = db
        self.ctx = ctx
        self._resource_instances: dict[str, BaseResource] = {
            inst.resource_key: inst
            for inst in resource_types
        }
        self._indexer_instances: dict[str, BaseIndexer] = {
            inst.asset_name: inst
            for inst in indexer_types
        }
        self._converter_instances: dict[str, BaseConverter] = {
            inst.converter_id: inst
            for inst in converter_types
        }

        self.db.ensure_collections(list(self._indexer_instances.keys()))
        self._indexer_order = self._compute_order()

    def _compute_order(self) -> list[str]:
        ts: TopologicalSorter[str] = TopologicalSorter()
        for name, idx in self._indexer_instances.items():
            ts.add(name, *idx.required_assets)
        return list(ts.static_order())

    def _compute_layers(self, requested: set[str]) -> list[list[str]]:
        ts: TopologicalSorter[str] = TopologicalSorter()
        for name, idx in self._indexer_instances.items():
            if name in requested:
                ts.add(
                    name,
                    *[dep for dep in idx.required_assets if dep in requested])

        layers: list[list[str]] = []
        ts.prepare()
        while ts.is_active():
            ready = list(ts.get_ready())
            if ready:
                layers.append(sorted(ready))
                ts.done(*ready)
        return layers

    def _expand_requested(self, names: list[str]) -> set[str]:
        needed: set[str] = set()
        stack = list(names)
        while stack:
            name = stack.pop()
            if name in needed:
                continue
            needed.add(name)
            stack.extend(self._indexer_instances[name].required_assets)
        return needed

    def _resource_closure(self, roots: tuple[str, ...]) -> set[str]:
        out: set[str] = set()
        stack = list(roots)
        while stack:
            name = stack.pop()
            if name in out:
                continue
            out.add(name)
            stack.extend(self._resource_instances[name].required_resources)
        return out

    def _resources_for_indexer(
            self, indexer: BaseIndexer) -> dict[str, BaseResource]:
        names = self._resource_closure(indexer.required_resources)
        return {name: self._resource_instances[name] for name in names}

    def _exclusive_signature(self, indexer_name: str) -> dict[str, str]:
        indexer = self._indexer_instances[indexer_name]
        consumers: dict[str, set[str]] = {}

        def walk(resource_name: str, consumer_name: str) -> None:
            res = self._resource_instances[resource_name]
            if res.exclusive:
                consumers.setdefault(resource_name, set()).add(consumer_name)
            for dep in res.required_resources:
                walk(dep, resource_name)

        for rname in indexer.required_resources:
            walk(rname, indexer_name)

        normalized: dict[str, str] = {}
        for exclusive_resource, direct_users in consumers.items():
            if len(direct_users) == 1:
                normalized[exclusive_resource] = next(iter(direct_users))
            else:
                normalized[exclusive_resource] = f"__multi__:{indexer_name}"
        return normalized

    def _group_layer_into_windows(self, layer: list[str]) -> list[list[str]]:
        windows: list[tuple[list[str], dict[str, str]]] = []

        for name in layer:
            signature = self._exclusive_signature(name)
            placed = False
            for names, locks in windows:
                compatible = True
                for res_name, consumer_key in signature.items():
                    if res_name in locks and locks[res_name] != consumer_key:
                        compatible = False
                        break
                if compatible:
                    names.append(name)
                    for res_name, consumer_key in signature.items():
                        locks.setdefault(res_name, consumer_key)
                    placed = True
                    break

            if not placed:
                windows.append(([name], dict(signature)))

        return [names for names, _ in windows]

    def build_windows(self, names: list[str]) -> list[list[str]]:
        requested = self._expand_requested(names)
        layers = self._compute_layers(requested)
        windows: list[list[str]] = []
        for layer in layers:
            windows.extend(self._group_layer_into_windows(layer))
        return windows

    def create_plan(self, files: list[FileRef],
                    names: list[str]) -> ExecutionPlan:
        windows = self.build_windows(names)
        planned: list[PlannedIndexerBatch] = []

        for window_id, window in enumerate(windows):
            for name in window:
                indexer = self._indexer_instances[name]
                stage_files = [
                    ref for ref in files
                    if indexer.can_run(self.db.get_path(ref))
                    and not self.db.has_indexer_result(ref, name)
                ]
                if not stage_files:
                    continue
                chunk_size = max(1, indexer.max_parallel)
                sub_batches = [
                    stage_files[i:i + chunk_size]
                    for i in range(0, len(stage_files), chunk_size)
                ]
                planned.append(
                    PlannedIndexerBatch(
                        indexer_name=name,
                        file_refs=stage_files,
                        sub_batches=sub_batches,
                        window_id=window_id,
                    ))

        return ExecutionPlan(batches=planned)

    def execute_plan(self, plan: ExecutionPlan) -> None:
        for batch in plan.batches:
            self._run_indexer_batch(batch)

    def truncate_all(self) -> None:
        self.db.truncate_all(list(self._indexer_instances.keys()))

    def run_indexers(self, files: list[FileRef], names: list[str]) -> None:
        plan = self.create_plan(files, names)
        self.execute_plan(plan)

    def get_indexer_result(self, hash: FileHash | FileRef,
                           name: str) -> IndexerOutput:
        return IndexerOutput(
            indexer_id=name,
            result=self.db.get_indexer_result_type(  # type: ignore
                hash if isinstance(hash, FileHash) else hash.hash,
                name,
                self._indexer_instances[name].result_model,
            ),
        )

    def run_indexer(self, file: FileRef, names: list[str]):
        self.run_indexers([file], names)

    def _run_indexer_batch(self, batch: PlannedIndexerBatch) -> None:
        indexer = self._indexer_instances[batch.indexer_name]
        resources = self._resources_for_indexer(indexer)

        def work(ref: FileRef) -> tuple[FileRef, IndexerOutput]:
            assets: dict[str, IndexerOutput | None] = {}
            for name in indexer.required_assets:
                if self.db.has_indexer_result(ref, name):
                    assets[name] = self.get_indexer_result(ref.hash, name)
                else:
                    assets[name] = None

            request = IndexerRequest(file_ref=ref, dependency_results=assets)

            with (
                    ExceptionContextNote(
                        f"running indexer '{indexer.asset_name}' for '{self.db.get_path(ref)} {ref.hash}'"
                    ),
                    self.ctx.trace_scope("index",
                                         file=str(self.db.get_path(ref))),
            ):
                out = indexer.run(
                    ctx=self.ctx,
                    request=request,
                    resources=resources,  # type: ignore
                    assets=assets,  # type: ignore
                )
            return ref, out

        for chunk in batch.sub_batches:
            if len(chunk) > 1 and indexer.max_parallel > 1:
                with ThreadPoolExecutor(
                        max_workers=indexer.max_parallel) as ex:
                    completed = list(ex.map(work, chunk))
            else:
                completed = [work(ref) for ref in chunk]

            for ref, out in completed:
                with ExceptionContextNote(
                        f"indexer asset: {indexer.asset_name}"):
                    self.db.store_indexer_result(ref, indexer.asset_name,
                                                 out.result)

    @overload
    def run_converter(
        self,
        converter_name: str,
        inputs: list[FileRef],
        param: str = "",
        assets: dict[str, IndexerOutput] | None = None,
    ) -> ConverterOutput:
        ...

    @overload
    def run_converter(
        self,
        converter_name: str,
        inputs: list[list[FileRef]],
        param: str = "",
        assets: dict[str, IndexerOutput] | None = None,
    ) -> list[ConverterOutput]:
        ...

    def run_converter(
        self,
        converter_name: str,
        inputs: list[FileRef] | list[list[FileRef]],
        param: str = "",
        assets: dict[str, IndexerOutput] | None = None,
    ) -> ConverterOutput | list[ConverterOutput]:
        converter = self._converter_instances[converter_name]
        assets = assets or {}

        if inputs and isinstance(inputs[0], list):
            batches: list[list[FileRef]] = inputs  # type: ignore[assignment]
            single = False
        else:
            batches = [inputs]  # type: ignore[list-item]
            single = True

        outputs = [
            self._run_converter_one(converter, b, param, assets)
            for b in batches
        ]
        return outputs[0] if single else outputs

    def _run_converter_one(
        self,
        converter: BaseConverter,
        input_files: list[FileRef],
        param: str,
        assets: dict[str, IndexerOutput],
    ) -> ConverterOutput:
        resources = {
            name: self._resource_instances[name]
            for name in converter.required_resources
        }
        request = ConverterRequest(input_files=input_files, param=param)
        out = converter.run(self.ctx,
                            request,
                            resources=resources,
                            assets=assets)
        self.db.store_derivation(
            request.input_files,
            out.output_files,
            {"param": param},
            out.return_value,
        )
        return out

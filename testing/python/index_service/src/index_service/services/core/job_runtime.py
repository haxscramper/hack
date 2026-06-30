from __future__ import annotations

import logging
from concurrent.futures import ThreadPoolExecutor
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
class IndexRuntime:
    """Stage-batched runtime for executing indexers and converters."""

    def __init__(
        self,
        ctx: RunContext,
        db: IndexDatabase,
        indexer_types: Sequence[BaseIndexer],
        converter_types: Sequence[BaseConverter],
        resource_types: Sequence[BaseResource],
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

    def truncate_all(self) -> None:
        self.db.truncate_all(list(self._indexer_instances.keys()))

    def run_indexers(self, files: list[FileRef], names: list[str]) -> None:
        """Run the requested indexers (plus their dependencies) over all files.

        Execution is batched per indexer type, in dependency order: every file
        passes through one indexer before the next indexer starts. Returns a
        mapping hash -> {indexer_id: IndexerOutput}.
        """
        requested = self._expand_requested(names)
        order = [n for n in self._indexer_order if n in requested]

        for name in order:
            indexer = self._indexer_instances[name]
            targets = [
                f for f in files if indexer.can_run(self.db.get_path(f))
            ]
            self._run_indexer_stage(indexer, targets)

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

    def _run_indexer_stage(
        self,
        indexer: BaseIndexer,
        targets: list[FileRef],
    ) -> None:
        to_run: list[FileRef] = []
        for ref in targets:
            if not self.db.has_indexer_result(ref, indexer.asset_name):
                to_run.append(ref)

        resources = {
            name: self._resource_instances[name]
            for name in indexer.required_resources
        }

        def work(ref: FileRef) -> tuple[FileRef, IndexerOutput]:
            assets: dict[str, IndexerOutput | None] = dict()
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

        # Parallelize .run() within the stage; DB writes stay sequential.
        if indexer.max_parallel > 1 and len(to_run) > 1:
            with ThreadPoolExecutor(max_workers=indexer.max_parallel) as ex:
                completed = list(ex.map(work, to_run))
        else:
            completed = [work(ref) for ref in to_run]

        for ref, out in completed:
            with ExceptionContextNote(f"indexer asset: {indexer.asset_name}"):
                self.db.store_indexer_result(
                    ref,
                    indexer.asset_name,
                    out.result,
                )

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

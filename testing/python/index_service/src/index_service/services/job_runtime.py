from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor
from graphlib import TopologicalSorter
from beartype import beartype
from beartype.typing import overload

from index_service.services.db import IndexDatabase
from index_service.services.job_types import BaseConverter, BaseIndexer, BaseResource
from index_service.services.types import (
    ConverterOutput,
    ConverterRequest,
    FileRef,
    IndexerOutput,
    IndexerRequest,
)
from index_service.services.default_job_types import (
    DEFAULT_CONVERTER_TYPES,
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)


@beartype
class IndexRuntime:
    """Stage-batched runtime for executing indexers and converters."""

    def __init__(
        self,
        db: IndexDatabase,
        indexer_types: list[BaseIndexer] | None = None,
        converter_types: list[BaseConverter] | None = None,
        resource_types: list[BaseResource] | None = None,
    ) -> None:
        self.db = db
        self._resource_instances: dict[str, BaseResource] = {
            inst.resource_key: inst
            for inst in (
                resource_types or [t() for t in DEFAULT_RESOURCE_TYPES])
        }
        self._indexer_instances: dict[str, BaseIndexer] = {
            inst.asset_name: inst
            for inst in (indexer_types or [t() for t in DEFAULT_INDEXER_TYPES])
        }
        self._converter_instances: dict[str, BaseConverter] = {
            inst.converter_id: inst
            for inst in (
                converter_types or [t() for t in DEFAULT_CONVERTER_TYPES])
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

    def run_indexers(
        self,
        files: list[FileRef],
        names: list[str],
    ) -> dict[str, dict[str, IndexerOutput]]:
        """Run the requested indexers (plus their dependencies) over all files.

        Execution is batched per indexer type, in dependency order: every file
        passes through one indexer before the next indexer starts. Returns a
        mapping md5 -> {indexer_id: IndexerOutput}.
        """
        results: dict[str, dict[str, IndexerOutput]] = {
            f.md5.md5: {}
            for f in files
        }
        requested = self._expand_requested(names)
        order = [n for n in self._indexer_order if n in requested]

        for name in order:
            indexer = self._indexer_instances[name]
            targets = [f for f in files if indexer.can_run(f.path)]
            self._run_indexer_stage(indexer, targets, results)

        return results

    def run_indexer(self, file: FileRef,
                    names: list[str]) -> dict[str, IndexerOutput]:
        return self.run_indexers([file], names)[file.md5.md5]

    def _run_indexer_stage(
        self,
        indexer: BaseIndexer,
        targets: list[FileRef],
        results: dict[str, dict[str, IndexerOutput]],
    ) -> None:
        to_run: list[FileRef] = []
        for ref in targets:
            cached = self.db.get_indexer_result_optional(
                ref.md5, indexer.asset_name)
            if cached is not None:
                results[ref.md5.md5][indexer.asset_name] = IndexerOutput(
                    indexer_id=indexer.asset_name,
                    result=indexer.result_model.model_validate(cached.result),
                )
            else:
                to_run.append(ref)

        resources = {
            name: self._resource_instances[name]
            for name in indexer.required_resources
        }

        def work(ref: FileRef) -> tuple[FileRef, IndexerOutput]:
            deps = {
                name: results[ref.md5.md5][name]
                for name in indexer.required_assets
                if name in results[ref.md5.md5]
            }
            request = IndexerRequest(file_ref=ref, dependency_results=deps)
            out = indexer.run(request, resources=resources, assets=dict(deps))
            return ref, out

        # Parallelize .run() within the stage; DB writes stay sequential.
        if indexer.max_parallel > 1 and len(to_run) > 1:
            with ThreadPoolExecutor(max_workers=indexer.max_parallel) as ex:
                completed = list(ex.map(work, to_run))
        else:
            completed = [work(ref) for ref in to_run]

        for ref, out in completed:
            self.db.store_indexer_result(ref, indexer.asset_name, out.result)
            results[ref.md5.md5][indexer.asset_name] = out

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
        out = converter.run(request, resources=resources, assets=assets)
        self.db.store_derivation(
            request.input_files,
            out.output_files,
            {"param": param},
            out.return_value,
        )
        return out

from __future__ import annotations

from typing import Any

from airflow.decorators import task

from index_service.services.harness import BaseConverter, BaseIndexer
from index_service.services.protocol import (
    ConverterOutput,
    ConverterRequest,
    FileRef,
    IndexerOutput,
    IndexerRequest,
)


class ExecutionContext:
    """Shared state for in-process Airflow execution.

    Tasks communicate through this object instead of XCom, since
    ``dag.test()`` runs everything sequentially in a single process.
    """

    def __init__(self) -> None:
        self.outputs: dict[str, Any] = {}
        self.file_ref: FileRef | None = None

    def set_file_ref(self, fr: FileRef) -> None:
        self.file_ref = fr

    def add_output(self, name: str, value: Any) -> None:
        self.outputs[name] = value

    def get_upstream_outputs(
            self, deps: tuple[str, ...]) -> dict[str, IndexerOutput]:
        return {d: self.outputs[d] for d in deps if d in self.outputs}


def make_file_ref_task(
    context: ExecutionContext,
    arango: Any,
    md5: str,
    paths: list[str],
):

    @task
    def _file_ref() -> dict:
        arango.ensure_file(md5, paths)
        fr = FileRef(md5=md5, path=list(paths))
        context.set_file_ref(fr)
        return fr.model_dump()

    return _file_ref


def make_indexer_task(
    indexer: BaseIndexer,
    context: ExecutionContext,
    arango: Any,
    resource_instances: dict[str, Any],
):

    @task(task_id=indexer.asset_name)
    def _indexer() -> dict:
        assert context.file_ref is not None
        file_ref = context.file_ref
        upstream = context.get_upstream_outputs(indexer.required_assets)

        cached = arango.get_indexer_result_optional(file_ref.md5,
                                                    indexer.asset_name)
        if cached is not None:
            out = IndexerOutput(
                indexer_id=indexer.asset_name,
                result=indexer.result_model.model_validate(cached.result),
            )
        else:
            resources = {
                name: resource_instances[name]
                for name in indexer.required_resources
            }
            request = IndexerRequest(
                file_ref=file_ref,
                dependency_results=upstream,
            )
            out = indexer.run(
                request,
                resources=resources,
                assets=dict(upstream),
            )
            arango.store_indexer_result(file_ref.md5, indexer.asset_name,
                                        out.result)

        context.add_output(indexer.asset_name, out)
        return out.model_dump()

    return _indexer


def make_converter_task(
    converter: BaseConverter,
    context: ExecutionContext,
    arango: Any,
    resource_instances: dict[str, Any],
    input_files: list[str],
    input_md5s: list[str],
    param: str,
):

    @task(task_id=converter.converter_id)
    def _converter() -> dict:
        resources = {
            name: resource_instances[name]
            for name in converter.required_resources
        }
        upstream = context.get_upstream_outputs(converter.required_assets)
        request = ConverterRequest(
            input_files=list(input_files),
            param=param,
        )
        out = converter.run(
            request,
            resources=resources,
            assets=dict(upstream),
        )
        arango.store_derivation(
            list(input_md5s),
            out.output_files,
            {"param": param},
            out.return_value,
        )
        context.add_output(converter.converter_id, out)
        return out.model_dump()

    return _converter

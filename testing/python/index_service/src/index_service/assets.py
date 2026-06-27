from typing import Any

from dagster import AssetIn, Config, asset
from pydantic import BaseModel
import dagster

from index_service.harness import BaseConverter, BaseIndexer
from index_service.protocol import (
    ConverterOutput,
    ConverterRequest,
    FileRef,
    IndexerOutput,
    IndexerRequest,
)


class OutputCollector:
    """Collects indexer/converter outputs during in-process Dagster execution."""

    def __init__(self) -> None:
        self.outputs: dict[str, Any] = {}

    def add(self, name: str, value: Any) -> None:
        self.outputs[name] = value


class FileRefConfig(dagster.Config):
    md5: str
    paths: list[str]


class ConverterConfig(Config):
    input_files: list[str]
    input_md5s: list[str]
    param: str = ""


@asset(required_resource_keys={"arango"})
def file_ref(context, config: FileRefConfig) -> FileRef:
    context.resources.arango.ensure_file(config.md5, config.paths)
    return FileRef(md5=config.md5, paths=config.paths)


def _indexer_asset_body(
    indexer: BaseIndexer,
    context,
    file_ref: FileRef,
    upstream_outputs: dict[str, IndexerOutput],
) -> IndexerOutput:
    db = context.resources.arango
    cached = db.get_indexer_result_optional(file_ref.md5, indexer.asset_name)
    if cached is not None:
        out = IndexerOutput(
            indexer_id=indexer.asset_name,
            result=indexer.result_model.model_validate(cached.result),
        )
        context.resources.output_collector.add(indexer.asset_name, out)
        return out
    resources = {
        name: getattr(context.resources, name)
        for name in indexer.required_resources
    }
    request = IndexerRequest(file_ref=file_ref,
                             dependency_results=upstream_outputs)
    out = indexer.run(request, **resources)
    db.store_indexer_result(file_ref.md5, indexer.asset_name, out.result)
    context.resources.output_collector.add(indexer.asset_name, out)
    return out


def _sanitize(name: str) -> str:
    return name.replace("-", "_")


def _build_indexer_asset_fn(indexer: BaseIndexer):
    upstream = list(indexer.dependencies)
    params = ["context", "file_ref"] + [_sanitize(u) for u in upstream]
    upstream_dict_items = ", ".join(f"'{u}': {_sanitize(u)}" for u in upstream)
    upstream_dict = "{" + upstream_dict_items + "}"
    src = (
        f"def _asset_fn({', '.join(params)}):\n"
        f"    return _indexer_asset_body(indexer, context, file_ref, {upstream_dict})\n"
    )
    ns: dict = {
        "_indexer_asset_body": _indexer_asset_body,
        "indexer": indexer,
    }
    exec(src, ns)
    return ns["_asset_fn"]


def build_indexer_asset(indexer: BaseIndexer):
    fn = _build_indexer_asset_fn(indexer)
    ins: dict[str, Any] = {"file_ref": AssetIn("file_ref")}
    for u in indexer.dependencies:
        ins[_sanitize(u)] = AssetIn(u)
    return asset(
        name=indexer.asset_name,
        ins=ins,
        required_resource_keys={
            "arango", "output_collector", *indexer.required_resources
        },
    )(fn)


def _converter_asset_body(
    converter: BaseConverter,
    context,
    config: ConverterConfig,
) -> ConverterOutput:
    db = context.resources.arango
    resources = {
        name: getattr(context.resources, name)
        for name in converter.required_resources
    }
    request = ConverterRequest(input_files=config.input_files,
                               param=config.param)
    out = converter.run(request, **resources)
    db.store_derivation(
        config.input_md5s,
        out.output_files,
        {"param": config.param},
        out.return_value,
    )
    context.resources.output_collector.add(converter.converter_id, out)
    return out


def build_converter_asset(converter: BaseConverter):

    def _asset(context, config: ConverterConfig) -> ConverterOutput:
        return _converter_asset_body(converter, context, config)

    return asset(
        name=converter.converter_id,
        required_resource_keys={
            "arango", "output_collector", *converter.required_resources
        },
    )(_asset)

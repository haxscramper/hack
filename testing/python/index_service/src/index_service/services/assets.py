from pathlib import Path
from typing import Any

from dagster import AssetIn, Config, asset
from pydantic import BaseModel
import dagster

from index_service.services.db import IndexDatabase
from index_service.services.harness import BaseConverter, BaseIndexer
from index_service.services.protocol import (
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
    path: str


class ConverterConfig(Config):
    input_files: list[str]
    input_md5s: list[str]
    param: str = ""


@asset(required_resource_keys={"arango"})
def file_ref(context, config: FileRefConfig) -> FileRef:
    md5 = context.resources.arango.get_md5(Path(config.path))
    return FileRef(md5=md5, path=Path(config.path))


def _indexer_asset_body(
    indexer: BaseIndexer,
    context,
    file_ref: FileRef,
    upstream_outputs: dict[str, IndexerOutput],
) -> IndexerOutput:

    assert indexer.can_run(
        file_ref.path), f"{indexer.asset_name} cannot be used with {file_ref}"

    db: IndexDatabase = context.resources.arango
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
    out = indexer.run(
        request,
        resources=resources,
        assets=dict(upstream_outputs),
    )
    db.store_indexer_result(file_ref.md5, indexer.asset_name, out.result)
    context.resources.output_collector.add(indexer.asset_name, out)
    return out


def _sanitize(name: str) -> str:
    return name.replace("-", "_")


def _build_indexer_asset_fn(indexer: BaseIndexer):
    upstream = list(indexer.required_assets)
    params = ["context", "file_ref"] + [_sanitize(u) for u in upstream]
    upstream_dict_items = ", ".join(f"'{u}': {_sanitize(u)}" for u in upstream)
    upstream_dict = "{" + upstream_dict_items + "}" if upstream_dict_items else "{}"
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
    for u in indexer.required_assets:
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
    upstream_assets: dict[str, Any],
) -> ConverterOutput:
    db = context.resources.arango
    resources = {
        name: getattr(context.resources, name)
        for name in converter.required_resources
    }
    request = ConverterRequest(input_files=config.input_files,
                               param=config.param)
    out = converter.run(
        request,
        resources=resources,
        assets=dict(upstream_assets),
    )
    db.store_derivation(
        config.input_md5s,
        out.output_files,
        {"param": config.param},
        out.return_value,
    )
    context.resources.output_collector.add(converter.converter_id, out)
    return out


def build_converter_asset(converter: BaseConverter):
    upstream = list(converter.required_assets)
    params = ["context", "config: ConverterConfig"
              ] + [_sanitize(u) for u in upstream]
    upstream_dict_items = ", ".join(f"'{u}': {_sanitize(u)}" for u in upstream)
    upstream_dict = "{" + upstream_dict_items + "}" if upstream_dict_items else "{}"
    src = (
        f"def _asset_fn({', '.join(params)}):\n"
        f"    return _converter_asset_body(converter, context, config, {upstream_dict})\n"
    )
    ns: dict[str, Any] = {
        "_converter_asset_body": _converter_asset_body,
        "converter": converter,
        "ConverterConfig": ConverterConfig,
    }
    exec(src, ns)
    fn = ns["_asset_fn"]

    ins: dict[str, Any] = {}
    for u in upstream:
        ins[_sanitize(u)] = AssetIn(u)

    return asset(
        name=converter.converter_id,
        ins=ins,
        required_resource_keys={
            "arango", "output_collector", *converter.required_resources
        },
    )(fn)

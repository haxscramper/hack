from __future__ import annotations

from pathlib import Path
from typing import Any

from dagster import ResourceDefinition, RunConfig, materialize, mem_io_manager, resource

from index_service.assets import (
    ConverterConfig,
    FileRefConfig,
    OutputCollector,
    build_converter_asset,
    build_indexer_asset,
    file_ref,
)
from index_service.db import IndexDatabase
from index_service.registry import (
    DEFAULT_CONVERTER_TYPES,
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)


def _lazy_resource(cls: type) -> Any:

    @resource()
    def _r(_context):
        return cls()

    return _r


def _build_resource_defs(
        resource_overrides: dict[str, Any] | None = None) -> dict[str, Any]:
    overrides = resource_overrides or {}
    defs: dict[str, Any] = {}
    for cls in DEFAULT_RESOURCE_TYPES:
        if cls.resource_key in overrides:
            defs[cls.resource_key] = ResourceDefinition.hardcoded_resource(
                overrides[cls.resource_key])
        else:
            defs[cls.resource_key] = _lazy_resource(cls)
    return defs


stfu = {
    "console": {
        "config": {
            "log_level": "WARNING",
        }
    }
}


def run_index_file_job(
    arango: IndexDatabase,
    path: Path,
    resource_overrides: dict[str, Any] | None = None,
    indexer_names: list[str] | None = None,
) -> Any:
    collector = OutputCollector()
    resource_defs: dict[str, Any] = {
        "io_manager": mem_io_manager,
        "arango": ResourceDefinition.hardcoded_resource(arango),
        "output_collector": ResourceDefinition.hardcoded_resource(collector),
    }

    resource_defs.update(_build_resource_defs(resource_overrides))

    if indexer_names is None:
        indexer_instances = [cls() for cls in DEFAULT_INDEXER_TYPES]
    else:
        by_name = {cls.asset_name: cls for cls in DEFAULT_INDEXER_TYPES}
        indexer_instances = [by_name[n]() for n in indexer_names]

    indexer_instances = [
        inst for inst in indexer_instances if inst.can_run(path)
    ]

    assets = [file_ref
              ] + [build_indexer_asset(idx) for idx in indexer_instances]

    run_config = RunConfig(
        ops={
            "file_ref": FileRefConfig(
                md5=arango.add_path(path),
                path=str(path),
            )
        },
        loggers=stfu,
    )
    return materialize(assets, resources=resource_defs, run_config=run_config)


def run_convert_files_job(
    arango: IndexDatabase,
    input_files: list[str],
    input_md5s: list[str],
    param: str = "",
    resource_overrides: dict[str, Any] | None = None,
) -> Any:
    collector = OutputCollector()
    resource_defs: dict[str, Any] = {
        "io_manager": mem_io_manager,
        "arango": ResourceDefinition.hardcoded_resource(arango),
        "output_collector": ResourceDefinition.hardcoded_resource(collector),
    }
    resource_defs.update(_build_resource_defs(resource_overrides))

    converter_instances = [cls() for cls in DEFAULT_CONVERTER_TYPES]
    assets = [build_converter_asset(c) for c in converter_instances]
    run_config = RunConfig(
        ops={
            c.converter_id:
            ConverterConfig(
                input_files=list(input_files),
                input_md5s=list(input_md5s),
                param=param,
            )
            for c in converter_instances
        },
        loggers=stfu,
    )
    return materialize(assets, resources=resource_defs, run_config=run_config)

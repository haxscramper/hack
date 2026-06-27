from __future__ import annotations

from pathlib import Path
from typing import Any

from dagster import ResourceDefinition, RunConfig, materialize, mem_io_manager

from index_service.assets import (
    ConverterConfig,
    FileRefConfig,
    OutputCollector,
    build_converter_asset,
    build_indexer_asset,
    file_ref,
)
from index_service.db import IndexDatabase
from index_service.harness import BaseConverter, BaseIndexer, BaseResource
from index_service.protocol import ConverterOutput, FileRef, IndexerOutput
from index_service.registry import (
    DEFAULT_CONVERTER_TYPES,
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)

stfu = {
    "console": {
        "config": {
            "log_level": "WARNING",
        }
    }
}


class DagsterIndexRunner:

    def __init__(
        self,
        indexer_types: list[type[BaseIndexer]] | None = None,
        converter_types: list[type[BaseConverter]] | None = None,
        resource_types: list[type[BaseResource]] | None = None,
    ) -> None:
        self._indexer_types = indexer_types or list(DEFAULT_INDEXER_TYPES)
        self._converter_types = converter_types or list(
            DEFAULT_CONVERTER_TYPES)
        self._resource_types = resource_types or list(DEFAULT_RESOURCE_TYPES)

    def _build_resource_defs(
        self,
        resource_overrides: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        overrides = resource_overrides or {}
        defs: dict[str, Any] = {}
        for cls in self._resource_types:
            if cls.resource_key in overrides:
                defs[cls.resource_key] = ResourceDefinition.hardcoded_resource(
                    overrides[cls.resource_key])
            else:
                defs[cls.resource_key] = ResourceDefinition.hardcoded_resource(
                    cls())
        return defs

    def _indexer_instances(self,
                           names: list[str] | None = None
                           ) -> list[BaseIndexer]:
        by_name = {cls.asset_name: cls for cls in self._indexer_types}
        if names is None:
            return [cls() for cls in self._indexer_types]
        return [by_name[n]() for n in names]

    def run_index_file_job(
        self,
        arango: IndexDatabase,
        path: Path,
        resource_overrides: dict[str, Any] | None = None,
        indexer_names: list[str] | None = None,
    ) -> Any:
        collector = OutputCollector()
        resource_defs: dict[str, Any] = {
            "io_manager": mem_io_manager,
            "arango": ResourceDefinition.hardcoded_resource(arango),
            "output_collector":
            ResourceDefinition.hardcoded_resource(collector),
        }

        resource_defs.update(self._build_resource_defs(resource_overrides))

        indexer_instances = [
            inst for inst in self._indexer_instances(indexer_names)
            if inst.can_run(path)
        ]

        assets = [file_ref
                  ] + [build_indexer_asset(idx) for idx in indexer_instances]

        run_config = RunConfig(
            ops={
                "file_ref":
                FileRefConfig(
                    md5=arango.get_md5(path),
                    path=str(path),
                )
            },
            loggers=stfu,
        )
        return materialize(assets,
                           resources=resource_defs,
                           run_config=run_config)

    def run_convert_files_job(
        self,
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
            "output_collector":
            ResourceDefinition.hardcoded_resource(collector),
        }
        resource_defs.update(self._build_resource_defs(resource_overrides))

        converter_instances = [cls() for cls in self._converter_types]
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
        return materialize(assets,
                           resources=resource_defs,
                           run_config=run_config)

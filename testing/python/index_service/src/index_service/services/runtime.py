from __future__ import annotations

import logging
from typing import Any

from dagster import ResourceDefinition, RunConfig, materialize, mem_io_manager

from index_service.services.assets import (
    ConverterConfig,
    FileRefConfig,
    OutputCollector,
    build_converter_asset,
    build_indexer_asset,
    file_ref,
)
from index_service.services.db import IndexDatabase
from index_service.services.harness import BaseConverter, BaseIndexer, BaseResource
from index_service.services.types import ConverterOutput, FileRef, IndexerOutput
from index_service.services.registry import (
    DEFAULT_CONVERTER_TYPES,
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)


class IndexRuntime:
    """Dagster-backed runtime for executing indexers and converters."""

    def __init__(
        self,
        db: IndexDatabase,
        indexer_types: list[BaseIndexer] | None = None,
        converter_types: list[BaseConverter] | None = None,
        resource_types: list[BaseResource] | None = None,
    ) -> None:
        self.db = db
        self._resource_instances: dict[str, BaseResource] = {
            cls.resource_key: cls
            for cls in (
                resource_types or [t() for t in DEFAULT_RESOURCE_TYPES])
        }
        self._indexer_instances = {
            cls.asset_name: cls
            for cls in (indexer_types or [t() for t in DEFAULT_INDEXER_TYPES])
        }
        self._converter_instances = {
            cls.converter_id: cls
            for cls in (
                converter_types or [t() for t in DEFAULT_CONVERTER_TYPES])
        }

    def _resource_defs(self, collector: OutputCollector) -> dict[str, Any]:
        defs: dict[str, Any] = {
            "io_manager": mem_io_manager,
            "arango": ResourceDefinition.hardcoded_resource(self.db),
            "output_collector":
            ResourceDefinition.hardcoded_resource(collector),
        }
        for key, inst in self._resource_instances.items():
            defs[key] = ResourceDefinition.hardcoded_resource(inst)
        return defs

    def run_indexers(
        self,
        file_ref_: FileRef,
        names: list[str],
    ) -> dict[str, IndexerOutput]:
        collector = OutputCollector()
        indexer_instances = [
            self._indexer_instances[n] for n in names
            if self._indexer_instances[n].can_run(file_ref_.path)
        ]

        assets = [file_ref
                  ] + [build_indexer_asset(idx) for idx in indexer_instances]
        run_config = RunConfig(
            ops={
                "file_ref":
                FileRefConfig(
                    md5=file_ref_.md5.md5,
                    path=str(file_ref_.path),
                )
            },
            loggers={
                "console": {
                    "config": {
                        "log_level": "WARNING",
                    }
                },
            },
        )
        logging.getLogger("dagster").setLevel(logging.ERROR)
        result = materialize(
            assets,
            resources=self._resource_defs(collector),
            run_config=run_config,
        )
        assert result.success
        return collector.outputs

    def run_converter(
        self,
        converter_name: str,
        input: list[FileRef],
        param: str = "",
    ) -> ConverterOutput:
        collector = OutputCollector()
        converter = self._converter_instances[converter_name]
        assets = [build_converter_asset(converter)]
        run_config = RunConfig(
            ops={
                converter.converter_id:
                ConverterConfig(
                    input=[
                        FileRefConfig(md5=f.md5.md5, path=str(f.path))
                        for f in input
                    ],
                    param=param,
                )
            })

        result = materialize(
            assets,
            resources=self._resource_defs(collector),
            run_config=run_config,
        )

        assert result.success
        return collector.outputs[converter_name]

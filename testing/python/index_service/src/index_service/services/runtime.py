from __future__ import annotations

import logging
from pathlib import Path
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

from index_service.services.harness import BaseConverter, BaseIndexer, BaseResource
from index_service.services.protocol import ConverterOutput, FileRef, IndexerOutput
from index_service.services.registry import (
    DEFAULT_CONVERTER_TYPES,
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)


class _NullIndexDatabase:
    """No-op database used when no ArangoDB connection is wired in."""

    def get_md5(self, path: Path) -> str:
        return ""

    def get_indexer_result_optional(self, md5: str, indexer_id: str) -> None:
        return None

    def store_indexer_result(self, md5: str, indexer_id: str,
                             result: Any) -> None:
        pass

    def store_derivation(
        self,
        input_md5s: list[str],
        output_files: list[str],
        config: dict[str, object],
        return_value: Any,
    ) -> str:
        return "null"


class IndexRuntime:
    """Dagster-backed runtime for executing indexers and converters."""

    def __init__(
        self,
        resource_overrides: dict[str, BaseResource | type[BaseResource]]
        | None = None,
        db: Any = None,
        indexer_types: list[type[BaseIndexer]] | None = None,
        converter_types: list[type[BaseConverter]] | None = None,
        resource_types: list[type[BaseResource]] | None = None,
    ) -> None:
        self._db = db
        overrides = resource_overrides or {}
        self._resource_instances: dict[str, BaseResource] = {}
        base_resource_types = resource_types or DEFAULT_RESOURCE_TYPES
        for cls in base_resource_types:
            self._resource_instances[cls.resource_key] = cls()
        for key, override in overrides.items():
            instance: BaseResource
            if isinstance(override, type) and issubclass(
                    override, BaseResource):
                instance = override()
            else:
                instance = override  # type: ignore[assignment]
            self._resource_instances[key] = instance

        indexer_type_list = indexer_types or DEFAULT_INDEXER_TYPES
        self._indexer_instances = {
            cls.asset_name: cls()
            for cls in indexer_type_list
        }
        converter_type_list = converter_types or DEFAULT_CONVERTER_TYPES
        self._converter_instances = {
            cls.converter_id: cls()
            for cls in converter_type_list
        }

    def _resource_defs(self, collector: OutputCollector) -> dict[str, Any]:
        defs: dict[str, Any] = {
            "io_manager":
            mem_io_manager,
            "arango":
            ResourceDefinition.hardcoded_resource(self._db
                                                  or _NullIndexDatabase()),
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
        indexer_instances = [self._indexer_instances[n] for n in names]
        assets = [file_ref
                  ] + [build_indexer_asset(idx) for idx in indexer_instances]
        run_config = RunConfig(
            ops={
                "file_ref":
                FileRefConfig(
                    md5=file_ref_.md5,
                    path=str(file_ref_.path),
                )
            })
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
        input_files: list[str],
        param: str = "",
    ) -> ConverterOutput:
        collector = OutputCollector()
        converter = self._converter_instances[converter_name]
        assets = [build_converter_asset(converter)]
        run_config = RunConfig(
            ops={
                converter.converter_id:
                ConverterConfig(
                    input_files=list(input_files),
                    input_md5s=[],
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

    def stop(self) -> None:
        pass

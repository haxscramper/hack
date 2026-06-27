from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any

from dagster import ResourceDefinition, RunConfig, materialize, mem_io_manager
from pydantic import BaseModel

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
from index_service.services.types import MD5, ConverterOutput, FileRef, IndexerOutput
from index_service.services.runtime import IndexRuntime


def _touch(path: Path) -> None:
    path.write_text("payload")


def test_chain_two_indexers(db: IndexDatabase, tmp_path: Path) -> None:
    call_log: list[str] = []

    class RootModel(BaseModel):
        value: str

    class ChildModel(BaseModel):
        value: str

    class RootIndexer(BaseIndexer):
        asset_name = "root_indexer"
        result_model = RootModel

        def run(
            self,
            request,
            resources,
            assets,
        ) -> IndexerOutput:
            call_log.append("root")
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=RootModel(value="root-value"),
            )

    class ChildIndexer(BaseIndexer):
        asset_name = "child_indexer"
        result_model = ChildModel
        required_assets = ("root_indexer", )

        def run(
            self,
            request,
            resources,
            assets,
        ) -> IndexerOutput:
            call_log.append("child")
            assert "root_indexer" in assets
            root_output = assets["root_indexer"]
            assert isinstance(root_output, IndexerOutput)
            assert root_output.result.value == "root-value"
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=ChildModel(value="child-value"),
            )

    file_path = tmp_path / "doc.txt"
    _touch(file_path)

    runtime = IndexRuntime(db=db,
                           indexer_types=[
                               RootIndexer(),
                               ChildIndexer(),
                           ])

    outputs = runtime.run_indexers(
        runtime.db.as_ref(file_path),
        ["root_indexer", "child_indexer"],
    )
    assert outputs["child_indexer"].result.value == "child-value"
    assert call_log == ["root", "child"]


def test_branching_indexers(db: IndexDatabase, tmp_path: Path) -> None:
    call_log: list[str] = []

    class ModelA(BaseModel):
        value: str

    class ModelB(BaseModel):
        value: str

    class ModelC(BaseModel):
        value: str

    class ModelD(BaseModel):
        value: str

    class IndexerA(BaseIndexer):
        asset_name = "indexer_a"
        result_model = ModelA

        def run(self, request, resources, assets) -> IndexerOutput:
            call_log.append("A")
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelA(value="A"))

    class IndexerB(BaseIndexer):
        asset_name = "indexer_b"
        result_model = ModelB
        required_assets = ("indexer_a", )

        def run(self, request, resources, assets) -> IndexerOutput:
            call_log.append("B")
            assert "indexer_a" in assets
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelB(value="B"))

    class IndexerC(BaseIndexer):
        asset_name = "indexer_c"
        result_model = ModelC
        required_assets = ("indexer_b", )

        def run(self, request, resources, assets) -> IndexerOutput:
            call_log.append("C")
            assert "indexer_b" in assets
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelC(value="C"))

    class IndexerD(BaseIndexer):
        asset_name = "indexer_d"
        result_model = ModelD
        required_assets = ("indexer_b", )

        def run(self, request, resources, assets) -> IndexerOutput:
            call_log.append("D")
            assert "indexer_b" in assets
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelD(value="D"))

    path = tmp_path / "branch.txt"
    _touch(path)

    runtime = IndexRuntime(
        db=db,
        indexer_types=[
            IndexerA(),
            IndexerB(),
            IndexerC(),
            IndexerD(),
        ],
    )

    runtime.run_indexers(
        runtime.db.as_ref(path),
        [
            "indexer_a",
            "indexer_b",
            "indexer_c",
            "indexer_d",
        ],
    )

    assert call_log[0] == "A"
    assert call_log[1] == "B"
    assert call_log.index("B") < call_log.index("C")
    assert call_log.index("B") < call_log.index("D")
    assert set(call_log) == {"A", "B", "C", "D"}


def test_indexer_receives_resource(db: IndexDatabase, tmp_path: Path) -> None:

    class ResourceRequest(BaseModel):
        value: str

    class ResourceResponse(BaseModel):
        value: str

    class EchoResource(BaseResource):
        resource_key = "echo"

        def handle(self, request: ResourceRequest) -> ResourceResponse:
            return ResourceResponse(value=request.value)

    class EchoModel(BaseModel):
        echoed: str

    class EchoIndexer(BaseIndexer):
        asset_name = "echo_indexer"
        result_model = EchoModel
        required_resources = ("echo", )

        def run(self, request, resources, assets) -> IndexerOutput:
            assert "echo" in resources
            resource = resources["echo"]
            response = resource.handle(ResourceRequest(value="ping"))
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=EchoModel(echoed=response.value),
            )

    file_path = tmp_path / "resource.txt"
    _touch(file_path)

    runtime = IndexRuntime(
        db=db,
        resource_types=[EchoResource()],
        indexer_types=[EchoIndexer()],
    )

    out = runtime.run_indexers(
        runtime.db.as_ref(file_path),
        ["echo_indexer"],
    )["echo_indexer"]

    assert out.result.echoed == "ping"


def test_converter_consumes_indexer_asset(tmp_path: Path) -> None:

    class StubDB:

        def _get_md5(self, path: Path) -> MD5:
            return MD5(md5=hashlib.md5(path.read_bytes()).hexdigest())

        def as_ref(self, path: Path) -> FileRef:
            return FileRef(md5=self._get_md5(path), path=path)

        def get_indexer_result_optional(self, md5: str, indexer_id: str):
            return None

        def store_indexer_result(self, md5: str, indexer_id: str,
                                 result: Any) -> None:
            pass

        def store_derivation(self, input_md5s, output_files, config,
                             return_value):
            return "key"

    class DepResult(BaseModel):
        token: str

    class DepIndexer(BaseIndexer):
        asset_name = "dependency_indexer"
        result_model = DepResult

        def run(self, request, resources, assets) -> IndexerOutput:
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=DepResult(token="indexed"),
            )

    class ConvResult(BaseModel):
        payload: str

    class DepConverter(BaseConverter):
        converter_id = "dependent_converter"
        result_model = ConvResult
        required_assets = ("dependency_indexer", )

        def run(self, request, resources, assets) -> ConverterOutput:
            assert "dependency_indexer" in assets
            upstream = assets["dependency_indexer"]
            assert isinstance(upstream, IndexerOutput)
            return ConverterOutput(
                converter_id=self.converter_id,
                output_files=[],
                return_value=ConvResult(
                    payload=f"{upstream.result.token}:{request.param}"),
            )

    data_path = tmp_path / "data.txt"
    _touch(data_path)
    md5 = hashlib.md5(data_path.read_bytes()).hexdigest()

    collector = OutputCollector()
    db = StubDB()
    dep_indexer = DepIndexer()
    dep_converter = DepConverter()

    resource_defs = {
        "io_manager": mem_io_manager,
        "arango": ResourceDefinition.hardcoded_resource(db),
        "output_collector": ResourceDefinition.hardcoded_resource(collector),
    }

    assets = [
        file_ref,
        build_indexer_asset(dep_indexer),
        build_converter_asset(dep_converter),
    ]

    run_config = RunConfig(
        ops={
            "file_ref":
            FileRefConfig(md5=md5, path=str(data_path)),
            "dependent_converter":
            ConverterConfig(
                input=[FileRefConfig(md5=md5, path=str(data_path))],
                param="ok",
            ),
        },
        loggers={"console": {
            "config": {
                "log_level": "WARNING"
            }
        }},
    )

    result = materialize(assets,
                         resources=resource_defs,
                         run_config=run_config)
    assert result.success
    conv_output = collector.outputs["dependent_converter"]
    assert conv_output.return_value.payload == "indexed:ok"

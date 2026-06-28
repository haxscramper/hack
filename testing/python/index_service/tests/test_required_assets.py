from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any

from beartype.typing import cast
from pydantic import BaseModel
from index_service.services.db import IndexDatabase
from index_service.services.job_types import BaseConverter, BaseIndexer, BaseResource, RunContext
from index_service.services.types import MD5, ConverterOutput, FileRef, IndexerOutput
from index_service.services.job_runtime import IndexRuntime


def _touch(path: Path) -> None:
    path.write_text("payload")


def test_chain_two_indexers(db: IndexDatabase, tmp_path: Path) -> None:
    call_log: list[str] = []

    class RootModel(BaseModel):
        value: str

    class NestedModel(BaseModel):
        value: str

    class RootIndexer(BaseIndexer):
        asset_name = "root_indexer"
        result_model = RootModel

        def run(
            self,
            ctx: RunContext,
            request,
            resources,
            assets,
        ) -> IndexerOutput:
            call_log.append("root")
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=RootModel(value="root-value"),
            )

    class NestedIndexer(BaseIndexer):
        asset_name = "nested_indexer"
        result_model = NestedModel
        required_assets = ("root_indexer", )

        def run(
            self,
            ctx: RunContext,
            request,
            resources,
            assets,
        ) -> IndexerOutput:
            call_log.append("nested")
            assert "root_indexer" in assets
            root_output = assets["root_indexer"]
            assert isinstance(root_output, IndexerOutput)
            assert root_output.result.value == "root-value"
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=NestedModel(value="nested-value"),
            )

    file_path = tmp_path / "doc.txt"
    _touch(file_path)

    ctx = RunContext(db)
    runtime = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[
            RootIndexer(),
            NestedIndexer(),
        ],
    )

    root = db.add_root("root", file_path.parent)
    ref = runtime.db.as_ref(root, file_path)
    runtime.run_indexer(ref, ["root_indexer", "nested_indexer"])

    outputs = runtime.get_indexer_result(ref, "nested_indexer")
    assert isinstance(outputs.result, NestedModel)

    assert outputs.result.value == "nested-value"
    assert call_log == ["root", "nested"]


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

        def run(self, ctx: RunContext, request, resources,
                assets) -> IndexerOutput:
            call_log.append("A")
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelA(value="A"))

    class IndexerB(BaseIndexer):
        asset_name = "indexer_b"
        result_model = ModelB
        required_assets = ("indexer_a", )

        def run(self, ctx: RunContext, request, resources,
                assets) -> IndexerOutput:
            call_log.append("B")
            assert "indexer_a" in assets
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelB(value="B"))

    class IndexerC(BaseIndexer):
        asset_name = "indexer_c"
        result_model = ModelC
        required_assets = ("indexer_b", )

        def run(self, ctx: RunContext, request, resources,
                assets) -> IndexerOutput:
            call_log.append("C")
            assert "indexer_b" in assets
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelC(value="C"))

    class IndexerD(BaseIndexer):
        asset_name = "indexer_d"
        result_model = ModelD
        required_assets = ("indexer_b", )

        def run(self, ctx: RunContext, request, resources,
                assets) -> IndexerOutput:
            call_log.append("D")
            assert "indexer_b" in assets
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=ModelD(value="D"))

    path = tmp_path / "branch.txt"
    _touch(path)

    ctx = RunContext(db)
    runtime = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[
            IndexerA(),
            IndexerB(),
            IndexerC(),
            IndexerD(),
        ],
    )

    root = db.add_root("root", tmp_path)
    runtime.run_indexer(
        runtime.db.as_ref(root, path),
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

        def run(self, ctx: RunContext, request, resources,
                assets) -> IndexerOutput:
            assert "echo" in resources
            resource = resources["echo"]
            response = resource.handle(ResourceRequest(value="ping"))
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=EchoModel(echoed=response.value),
            )

    file_path = tmp_path / "resource.txt"
    _touch(file_path)

    ctx = RunContext(db)
    runtime = IndexRuntime(
        ctx=ctx,
        db=db,
        resource_types=[EchoResource()],
        indexer_types=[EchoIndexer()],
    )

    root = db.add_root("root", tmp_path)
    ref = runtime.db.as_ref(root, file_path)
    runtime.run_indexer(ref, ["echo_indexer"])
    out = runtime.get_indexer_result(ref, "echo_indexer")

    assert isinstance(out.result, EchoModel)
    assert out.result.echoed == "ping"


def test_converter_consumes_indexer_asset(tmp_path: Path,
                                          db: IndexDatabase) -> None:

    class DepResult(BaseModel):
        token: str

    class DepIndexer(BaseIndexer):
        asset_name = "dependency_indexer"
        result_model = DepResult

        def run(self, ctx: RunContext, request, resources,
                assets) -> IndexerOutput:
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

        def run(self, ctx: RunContext, request, resources,
                assets) -> ConverterOutput:
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

    ctx = RunContext(db)
    runtime = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[DepIndexer()],
        converter_types=[DepConverter()],
        resource_types=[],
    )

    root = db.add_root("root", tmp_path)
    ref = db.as_ref(root, data_path)

    indexer_results = runtime.run_indexers([ref], ["dependency_indexer"])
    assert runtime.db.has_indexer_result(ref, "dependency_indexer")
    upstream = runtime.get_indexer_result(ref.md5, "dependency_indexer")
    assert upstream.result.token == "indexed"

    conv_output = runtime.run_converter(
        "dependent_converter",
        [ref],
        param="ok",
        assets=dict(dependency_indexer=upstream),
    )

    assert conv_output.return_value.payload == "indexed:ok"

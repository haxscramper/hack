import json
import logging
import glom
from pathlib import Path

from beartype.typing import Any, ClassVar, cast
from pydantic import BaseModel, Field, model_validator, model_serializer

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.indexers.exif_metadata import ExifMetadataIndexerResult
from index_service.services.core.types import IndexerOutput, IndexerRequest

log = logging.getLogger(__name__)


class XYObject(BaseModel, extra="allow"):
    f_0: float = Field(alias="0")
    f_1: float = Field(alias="1")


class Group(BaseModel, extra="allow"):
    title: str
    bounding: tuple[float, float, float, float]
    color: str | None = None
    font_size: float | None = None
    locked: bool | None = None


class ConfigModel(BaseModel, extra="allow"):
    links_ontop: bool | None = None
    align_to_grid: bool | None = None


class State(BaseModel, extra="allow"):
    lastGroupid: float | None = None
    lastNodeId: float | None = None
    lastLinkId: float | None = None
    lastRerouteId: float | None = None


class Flags(BaseModel, extra="allow"):
    collapsed: bool | None = None
    pinned: bool | None = None
    allow_interaction: bool | None = None
    horizontal: bool | None = None
    skip_repeated_outputs: bool | None = None


class Input(BaseModel, extra="allow"):
    name: str
    type: str | list[str] | float
    link: float | None = None
    slot_index: int | str | None = None


class Output(BaseModel, extra="allow"):
    name: str
    type: str | list[str] | float
    links: list[float] | None = None
    slot_index: int | str | None = None


class NodeProperties(BaseModel, extra="allow"):
    node_name_for_s_r: str | None = Field(default=None,
                                          alias="Node name for S&R")


class Node(BaseModel, extra="allow"):
    id: int | str
    type: str
    pos: XYObject | tuple[float, float]
    size: XYObject | tuple[float, float]
    flags: Flags
    order: float
    mode: float
    inputs: list[Input] | None = None
    outputs: list[Output] | None = None
    properties: NodeProperties
    widgets_values: list[Any] | dict[str, Any] | None = None
    color: str | None = None
    bgcolor: str | None = None


class Link(BaseModel, extra="allow"):
    id: float
    origin_id: int | str
    origin_slot: int | str
    target_id: int | str
    target_slot: int | str
    type: str | list[str] | float
    parentId: float | None = None

    _ARRAY_FIELDS: ClassVar[tuple[str, ...]] = (
        "id",
        "origin_id",
        "origin_slot",
        "target_id",
        "target_slot",
        "type",
    )

    @model_validator(mode="before")
    @classmethod
    def _from_array(cls, data):
        if isinstance(data, (list, tuple)):
            d = dict(zip(cls._ARRAY_FIELDS, data))
            if len(data) > len(cls._ARRAY_FIELDS):
                d["parentId"] = data[len(cls._ARRAY_FIELDS)]
            return d
        return data

    @model_serializer
    def _to_array(self):
        arr = [getattr(self, f) for f in self._ARRAY_FIELDS]
        if self.parentId is not None:
            arr.append(self.parentId)
        return arr


class Reroute(BaseModel, extra="allow"):
    id: float
    parentId: float | None = None
    pos: XYObject | tuple[float, float]
    linkIds: list[float] | None = None


class DS(BaseModel, extra="allow"):
    scale: float
    offset: XYObject | tuple[float, float]


class Info(BaseModel, extra="allow"):
    name: str
    author: str
    description: str
    version: str
    created: str
    modified: str
    software: str


class LinkExtension(BaseModel, extra="allow"):
    id: float
    parentId: float


class Extra(BaseModel, extra="allow"):
    ds: DS | None = None
    info: Info | None = None
    linkExtensions: list[LinkExtension] | None = None
    reroutes: list[Reroute] | None = None


class ModelItem(BaseModel, extra="allow"):
    name: str
    url: str
    hash: str | None = None
    hash_type: str | None = None
    directory: str


class ComfyWorkflow1_0(BaseModel, extra="allow"):
    version: float
    config: ConfigModel | None = None
    state: State = Field(default_factory=lambda: State())
    groups: list[Group] | None = None
    nodes: list[Node]
    links: list[Link] | None = None
    reroutes: list[Reroute] | None = None
    extra: Extra | None = None
    models: list[ModelItem] | None = None


class ComfyInputLink(BaseModel, extra="forbid"):
    origin_slot: int
    origin_id: str
    target_slot: int


class ComfyInput(BaseModel, extra="forbid"):
    node: str
    node_id: str
    title: str | None = None
    inputs: dict[str, Any] | list[Any]
    links: list[ComfyInputLink] = Field(
        description=
        "Incoming links to the node -- origin slot/id is a dependee node")


class ComfyInputIndexerResult(BaseModel, extra="forbid"):
    inputs: list[ComfyInput]
    note: str = ""


class ComfyInputIndexer(BaseIndexer):
    asset_name = "comfy_input"
    result_model = ComfyInputIndexerResult
    required_assets = ("exif_metadata", )

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def can_run(self, path: Path) -> bool:
        return path.suffix in [".png", ".jpg", ".webp", ".jpeg"]

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:

        pre_data = cast(IndexerOutput, assets["exif_metadata"]).result
        assert isinstance(pre_data, ExifMetadataIndexerResult), type(pre_data)

        if ("exif_metadata" not in pre_data.file.original_metadata_full
                or "workflow"
                not in pre_data.file.original_metadata_full["exif_metadata"]):
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=ComfyInputIndexerResult(
                    inputs=[], note="no workflow data detected"),
            )

        else:
            workflow_js = pre_data.file.original_metadata_full[
                "exif_metadata"]["workflow"]
            Path("/tmp/result.json").write_text(
                json.dumps(workflow_js, indent=2))

            workflow = ComfyWorkflow1_0(**workflow_js)
            result = ComfyInputIndexerResult(inputs=[])

            for node in workflow.nodes:
                if node.widgets_values:
                    result.inputs.append(
                        ComfyInput(
                            node=node.type,
                            node_id=str(node.id),
                            inputs=node.widgets_values,
                            title=glom.glom(node, "_meta.title", default=None),
                            links=[
                                ComfyInputLink(
                                    target_slot=int(l.target_slot),
                                    origin_slot=int(l.origin_slot),
                                    origin_id=str(l.origin_id),
                                ) for l in workflow.links
                                if l.target_id == node.id
                            ] if workflow.links else list(),
                        ))

            return IndexerOutput(indexer_id=self.asset_name, result=result)

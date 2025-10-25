#!/usr/bin/env python

from log_writer import log
from common import JSON_PATH, USE_GRAPH_CACHE
from pathlib import Path
from igraph_builder import parse_recipes_to_graph, RecipeNodeData, ItemNodeData, FluidNodeData, NodeDataUnion
import pickle
import igraph as ig
import elk_schema as elk
import typst_schema as typ
from pydantic import BaseModel
from beartype.typing import Literal, Union, List
from enum import Enum
from beartype import beartype


class PortData(BaseModel, extra="forbid"):
    direction: Literal["in", "out"]


class EdgeData(BaseModel, extra="forbid"):
    source: NodeDataUnion
    target: NodeDataUnion


class ElkExtra(BaseModel, extra="forbid"):
    data: Union[NodeDataUnion, EdgeData, PortData]
    kind: Literal["fluid_node", "item_node", "recipe_node", "port", "edge"]


def graph_to_typst(graph: elk.Graph) -> typ.Document:
    subnodes: List[typ.TypstNode] = []

    subnodes.append(
        typ.Import(
            path=
            "/home/haxscramper/workspace/repos/hack/testing/python/mi_pipeline_visualization/recipe_nodes.typ",
            items=["*"]))

    bbox = elk.compute_graph_bounding_box(graph)

    subnodes.append(
        typ.Set(target="page",
                args=dict(
                    width=typ.Literal(value=typ.PtSize(size=bbox.width)),
                    height=typ.Literal(value=typ.PtSize(size=bbox.height)),
                    margin=typ.Literal(
                        value=dict(
                            top=typ.PtSize(size=0),
                            bottom=typ.PtSize(size=0),
                            left=typ.PtSize(size=0),
                            right=typ.PtSize(size=0),
                        )),
                )))

    if graph.children:
        for node in graph.children:
            extra: ElkExtra = node.extra["data"]
            subnodes.append(
                typ.Command(
                    name=extra.kind,
                    args=[
                        typ.Literal(value=node.model_copy(update=dict(
                            extra=None)).model_dump(exclude_none=True))
                    ],
                ))

    if graph.edges:
        for edge in graph.edges:
            extra: ElkExtra = edge.extra["data"]
            subnodes.append(
                typ.Command(
                    name="edge",
                    args=[
                        typ.Literal(value=edge.model_copy(update=dict(
                            extra=None)).model_dump(exclude_none=True))
                    ],
                ))

    return typ.Document(subnodes=subnodes)


def convert_to_elk(graph: ig.Graph) -> elk.Graph:

    result = elk.Graph(
        id="root",
        children=[],
        edges=[],
        ports=[],
    )

    @beartype
    class Direction(Enum):
        IN = 0
        OUT = 1

        def to_port_side(self) -> elk.PortSide:
            if self == Direction.IN:
                return elk.PortSide.WEST

            else:
                return elk.PortSide.EAST

    @beartype
    def get_resource_port_id(id: str, side: Direction) -> str:
        return f"{id}-{side.name}"

    for recipe_idx, v in enumerate(graph.vs):
        match v["node_type"]:
            case "recipe":
                rec: RecipeNodeData = v["data"]

                node = elk.Node(
                    id=f"{rec.type}-{recipe_idx}",
                    width=200,
                    height=100,
                    ports=[],
                    extra=dict(data=ElkExtra(data=rec, kind="recipe_node")),
                )

                @beartype
                def get_port_id(kind: str, idx: int, side: Direction) -> str:
                    return f"{node.id}.{kind}_{idx}_{side.name}"

                @beartype
                def connect_resource(kind: str, idx: int, side: Direction,
                                     it: Union[ItemNodeData, FluidNodeData]):
                    port = elk.Port(
                        id=get_port_id(kind, idx, side),
                        properties=elk.PortProperties(
                            side=side.to_port_side()),
                    )

                    node.ports.append(port)

                    if side == Direction.IN:
                        port_src = get_resource_port_id(it.id, Direction.OUT)
                        port_dst = port.id
                        edge = elk.Edge(
                            source=it.id,
                            target=node.id,
                            sourcePort=port_src,
                            targetPort=port_dst,
                            id=f"{port_src}-{port_dst}",
                            extra=dict(data=ElkExtra(
                                kind="edge",
                                data=EdgeData(source=it, target=rec))),
                        )

                    else:
                        port_src = port.id
                        port_dst = get_resource_port_id(it.id, Direction.IN)
                        edge = elk.Edge(
                            source=node.id,
                            target=it.id,
                            sourcePort=port_src,
                            targetPort=port_dst,
                            id=f"{port_src}-{port_dst}",
                            extra=dict(
                                ElkExtra(kind="edge",
                                         data=EdgeData(source=rec,
                                                       target=it))),
                        )

                    result.edges.append(edge)

                for idx, it in enumerate(rec.fluid_inputs):
                    connect_resource("fluid", idx, Direction.IN, it)

                for idx, it in enumerate(rec.fluid_outputs):
                    connect_resource("fluid", idx, Direction.OUT, it)

                for idx, it in enumerate(rec.item_inputs):
                    connect_resource("item", idx, Direction.IN, it)

                for idx, it in enumerate(rec.item_outputs):
                    connect_resource("item", idx, Direction.OUT, it)

                result.children.append(node)

            case "fluid" | "item":
                if v["node_type"] == "fluid":
                    fluid: FluidNodeData = v["data"]
                    id = fluid.id

                else:
                    item: FluidNodeData = v["data"]
                    id = item.id

                node = elk.Node(
                    id=f"{id}",
                    width=50,
                    height=50,
                    extra=dict(data=ElkExtra(data=v["data"],
                                             kind=v["node_type"] + "_node")),
                    ports=[
                        elk.Port(
                            id=get_resource_port_id(id, Direction.IN),
                            properties=elk.PortProperties(
                                side=Direction.IN.to_port_side()),
                            extra=dict(data=ElkExtra(
                                kind="port", data=PortData(direction="in"))),
                        ),
                        elk.Port(
                            id=get_resource_port_id(id, Direction.OUT),
                            properties=elk.PortProperties(
                                side=Direction.OUT.to_port_side()),
                            extra=dict(data=ElkExtra(
                                kind="port", data=PortData(direction="out"))),
                        ),
                    ])

                result.children.append(node)

            case _:
                raise ValueError(f"{v['node_type']}")

    return result

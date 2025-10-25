#!/usr/bin/env python

from log_writer import log
from common import JSON_PATH, USE_GRAPH_CACHE
from pathlib import Path
from igraph_builder import parse_recipes_to_graph, RecipeNodeData, ItemNodeData, FluidNodeData
import pickle
import igraph as ig
import elk_schema as elk
from typst_schema import *
from enum import Enum


def graph_to_typst(graph: elk.Graph) -> Document:
    children: List[TypstNode] = []

    if graph.children:
        for node in graph.children:
            children.append(
                Command(
                    name="node",
                    args=[Literal(value=node.model_dump(exclude_none=True))]))

    if graph.edges:
        for edge in graph.edges:
            children.append(
                Command(
                    name="edge",
                    args=[Literal(value=edge.model_dump(exclude_none=True))]))

    return Document(children=children)


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
                    extra=dict(data=rec),
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
                        edge = elk.Edge(source=it.id,
                                        target=node.id,
                                        sourcePort=port_src,
                                        targetPort=port_dst,
                                        id=f"{port_src}-{port_dst}",
                                        extra=dict(src_data=it, dst_data=node))

                    else:
                        port_src = port.id
                        port_dst = get_resource_port_id(it.id, Direction.IN)
                        edge = elk.Edge(
                            source=node.id,
                            target=it.id,
                            sourcePort=port_src,
                            targetPort=port_dst,
                            id=f"{port_src}-{port_dst}",
                            extra=dict(src_data=node, dst_data=it),
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
                    extra=dict(data=v["data"]),
                    ports=[
                        elk.Port(
                            id=get_resource_port_id(id, Direction.IN),
                            properties=elk.PortProperties(
                                side=Direction.IN.to_port_side()),
                            extra=dict(direction="in"),
                        ),
                        elk.Port(
                            id=get_resource_port_id(id, Direction.OUT),
                            properties=elk.PortProperties(
                                side=Direction.OUT.to_port_side()),
                            extra=dict(direciton="out"),
                        ),
                    ])

                result.children.append(node)

            case _:
                raise ValueError(f"{v['node_type']}")

    return result

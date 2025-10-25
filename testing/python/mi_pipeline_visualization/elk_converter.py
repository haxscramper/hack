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
            children.extend(_convert_node(node))

    if graph.edges:
        for edge in graph.edges:
            children.extend(_convert_edge(edge))

    return Document(children=children)


def _convert_node(node: elk.Node) -> List[TypstNode]:
    result: List[TypstNode] = []

    rect_args: List[Union[Expression, NamedArg]] = [
        NamedArg(name="width", value=Literal(value=PtSize(size=node.width))),
        NamedArg(name="height", value=Literal(value=PtSize(size=node.height))),
        NamedArg(name="fill", value=RawLiteral(value="white")),
        NamedArg(name="stroke", value=RawLiteral(value="black"))
    ]

    rect_content: List[TypstNode] = []

    if node.labels:
        for label in node.labels:
            if label.text:
                rect_content.append(Text(content=label.text))

    rect_cmd = Command(name="rect",
                       args=rect_args,
                       content=rect_content if rect_content else None)

    place_args: List[Union[Expression, NamedArg]] = [
        RawLiteral(value="top + left"),
        NamedArg(name="dx", value=Literal(value=PtSize(size=node.x))),
        NamedArg(name="dy", value=Literal(value=PtSize(size=node.y)))
    ]

    place_cmd = Command(name="place", args=place_args, content=[rect_cmd])

    result.append(place_cmd)

    if node.children:
        for child in node.children:
            child_nodes = _convert_node(child)
            for child_node in child_nodes:
                if isinstance(child_node,
                              Command) and child_node.name == "place":
                    child_node.args[1].value = Literal(value=PtSize(
                        size=node.x + child.x))
                    child_node.args[2].value = Literal(value=PtSize(
                        size=node.y + child.y))
            result.extend(child_nodes)

    if node.edges:
        for edge in node.edges:
            edge_nodes = _convert_edge(edge, parent_offset=(node.x, node.y))
            result.extend(edge_nodes)

    return result


def _convert_edge(
    edge: elk.Edge, parent_offset: tuple[float, float] = (0, 0)
) -> List[TypstNode]:
    result: List[TypstNode] = []

    curve_elements: List["Command"] = []

    if edge.sections:
        for i, section in enumerate(edge.sections):
            start_x = section.startPoint.x + parent_offset[0]
            start_y = section.startPoint.y + parent_offset[1]
            end_x = section.endPoint.x + parent_offset[0]
            end_y = section.endPoint.y + parent_offset[1]

            if i == 0:
                curve_elements.append(
                    Command(name="curve.move",
                            args=[
                                Array(items=[
                                    Literal(value=PtSize(size=start_x)),
                                    Literal(value=PtSize(size=start_y))
                                ])
                            ]))

            if section.bendPoints and len(section.bendPoints) < 2:
                for bend_point in section.bendPoints:
                    bend_x = bend_point.x + parent_offset[0]
                    bend_y = bend_point.y + parent_offset[1]
                    curve_elements.append(
                        Command(name="curve.line",
                                args=[
                                    Array(items=[
                                        Literal(value=PtSize(size=bend_x)),
                                        Literal(value=PtSize(size=bend_y))
                                    ])
                                ]))
                curve_elements.append(
                    Command(name="curve.line",
                            args=[
                                Array(items=[
                                    Literal(value=PtSize(size=end_x)),
                                    Literal(value=PtSize(size=end_y))
                                ])
                            ]))
            elif section.bendPoints and len(section.bendPoints) == 2:
                ctrl1_x = section.bendPoints[0].x + parent_offset[0]
                ctrl1_y = section.bendPoints[0].y + parent_offset[1]
                ctrl2_x = section.bendPoints[1].x + parent_offset[0]
                ctrl2_y = section.bendPoints[1].y + parent_offset[1]

                curve_elements.append(
                    Command(name="curve.cubic",
                            args=[
                                Array(items=[
                                    Literal(value=PtSize(size=ctrl1_x)),
                                    Literal(value=PtSize(size=ctrl1_y))
                                ]),
                                Array(items=[
                                    Literal(value=PtSize(size=ctrl2_x)),
                                    Literal(value=PtSize(size=ctrl2_y))
                                ]),
                                Array(items=[
                                    Literal(value=PtSize(size=end_x)),
                                    Literal(value=PtSize(size=end_y))
                                ])
                            ]))
            else:
                curve_elements.append(
                    Command(name="curve.line",
                            args=[
                                Array(items=[
                                    Literal(value=PtSize(size=end_x)),
                                    Literal(value=PtSize(size=end_y))
                                ])
                            ]))
    elif edge.sourcePoint and edge.targetPoint:
        start_x = edge.sourcePoint.x + parent_offset[0]
        start_y = edge.sourcePoint.y + parent_offset[1]
        end_x = edge.targetPoint.x + parent_offset[0]
        end_y = edge.targetPoint.y + parent_offset[1]

        curve_elements.append(
            Command(name="curve.move",
                    args=[
                        Array(items=[
                            Literal(value=PtSize(size=start_x)),
                            Literal(value=PtSize(size=start_y))
                        ])
                    ]))

        if edge.bendPoints:
            for bend_point in edge.bendPoints:
                bend_x = bend_point.x + parent_offset[0]
                bend_y = bend_point.y + parent_offset[1]
                curve_elements.append(
                    Command(name="curve.line",
                            args=[
                                Array(items=[
                                    Literal(value=PtSize(size=bend_x)),
                                    Literal(value=PtSize(size=bend_y))
                                ])
                            ]))

        curve_elements.append(
            Command(name="curve.line",
                    args=[
                        Array(items=[
                            Literal(value=PtSize(size=end_x)),
                            Literal(value=PtSize(size=end_y))
                        ])
                    ]))

    if curve_elements:
        curve_cmd = Command(
            name="curve",
            args=[NamedArg(name="stroke", value=RawLiteral(value="black"))],
            content=curve_elements)
        result.append(curve_cmd)

    return result


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
                    ports=[
                        elk.Port(id=get_resource_port_id(id, Direction.IN),
                                 properties=elk.PortProperties(
                                     side=Direction.IN.to_port_side())),
                        elk.Port(id=get_resource_port_id(id, Direction.OUT),
                                 properties=elk.PortProperties(
                                     side=Direction.OUT.to_port_side())),
                    ])

                result.children.append(node)

            case _:
                raise ValueError(f"{v['node_type']}")

    return result

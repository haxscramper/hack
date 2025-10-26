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
from beartype.typing import Literal, Union, List, Optional, Set
from enum import Enum
from beartype import beartype
import recipe_gui_schema as gui


class PortData(BaseModel, extra="forbid"):
    direction: Literal["in", "out"]


class EdgeData(BaseModel, extra="forbid"):
    source: NodeDataUnion
    target: NodeDataUnion


class ElkExtra(BaseModel, extra="forbid"):
    data: Union[NodeDataUnion, EdgeData, PortData]
    machine: Optional[gui.MachineData] = None
    kind: Literal["fluid_node", "item_node", "recipe_node", "port", "edge"]


def graph_to_typst(graph: elk.Graph) -> typ.Document:
    subnodes: List[typ.TypstNode] = []

    subnodes.append(
        typ.Import(
            path=
            "/home/haxscramper/workspace/repos/hack/testing/python/mi_pipeline_visualization/recipe_nodes.typ",
            items=["*"]))

    bbox = elk.compute_graph_bounding_box(graph)

    # subnodes.append(
    #     typ.Set(target="text",
    #             args=dict(font=typ.Literal(value="Iosevka Term"))))

    subnodes.append(
        typ.Set(target="page",
                args=dict(
                    width=typ.Literal(value=typ.PtSize(size=bbox.width)),
                    height=typ.Literal(value=typ.PtSize(size=bbox.height)),
                    margin=typ.Literal(value=dict(
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
                        typ.Literal(value=node.model_dump(exclude_none=True))
                    ],
                ))

    if graph.edges:
        for edge in graph.edges:
            extra: ElkExtra = edge.extra["data"]
            subnodes.append(
                typ.Command(
                    name="edge",
                    args=[
                        typ.Literal(value=edge.model_dump(exclude_none=True))
                    ],
                ))

    return typ.Document(subnodes=subnodes)


def get_recipe_shape(id: str) -> gui.MachineData:
    if ":" in id:
        modid, machineid = id.split(":")

    else:
        machineid = id

    if machineid in gui.mi_fixed_machines:
        return gui.mi_fixed_machines[machineid]

    else:
        return gui.MachineData(
            english_name=id,
            machine=id,
            recipe_type=id,
            item_input_count=3,
            item_output_count=3,
            fluid_input_count=3,
            fluid_output_count=3,
            gui_params=gui.GuiParameters(),
            progress_bar=gui.ProgressBarParameters(x=88,
                                                   y=35,
                                                   texture="triple_arrow"),
            efficiency_bar=gui.RecipeEfficiencyBarParameters(x=50, y=66),
            energy_bar=gui.EnergyBarParameters(x=12, y=35),
            item_slots=gui.ItemSlots(positions=[
                gui.SlotPosition(x=30, y=27, cols=3, rows=1),
                gui.SlotPosition(x=116, y=27, cols=3, rows=1)
            ]),
            fluid_slots=gui.FluidSlots(positions=[
                gui.SlotPosition(x=30, y=47, cols=3, rows=1),
                gui.SlotPosition(x=116, y=47, cols=3, rows=1)
            ]),
            front_overlay=True,
            top_overlay=False,
            side_overlay=False,
            tiers=16,
            io_bucket_capacity=24,
            size=(200, 100),
        )


def convert_to_elk(graph: ig.Graph) -> elk.Graph:

    result = elk.Graph(id="root",
                       children=[],
                       edges=[],
                       ports=[],
                       layoutOptions={})

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

    known_graph_nodes: Set[str] = set()

    @beartype
    def get_recipe_id(rec: RecipeNodeData, recipe_idx: int) -> str:
        return f"{rec.type}-{recipe_idx}"

    for recipe_idx, v in enumerate(graph.vs):
        match v["node_type"]:
            case "recipe":
                known_graph_nodes.add(get_recipe_id(v["data"], recipe_idx))

            case "fluid" | "item":
                known_graph_nodes.add(v["data"].id)

            case _:
                raise RuntimeError("Unhandled node type")

    for recipe_idx, v in enumerate(graph.vs):
        match v["node_type"]:
            case "recipe":
                rec: RecipeNodeData = v["data"]

                recipe = get_recipe_shape(rec.type)

                node = elk.Node(
                    id=get_recipe_id(rec, recipe_idx),
                    width=recipe.size[0],
                    height=recipe.size[1],
                    ports=[],
                    extra=dict(data=ElkExtra(
                        data=rec,
                        kind="recipe_node",
                        machine=recipe,
                    )),
                )

                @beartype
                def get_port_id(kind: str, idx: int, side: Direction) -> str:
                    return f"{node.id}.{kind}_{idx}_{side.name}"

                @beartype
                def connect_resource(kind: str, idx: int, side: Direction,
                                     it: Union[ItemNodeData, FluidNodeData]):
                    port = elk.Port(
                        id=get_port_id(kind, idx, side),
                        width=8,
                        height=8,
                        properties=elk.PortProperties(
                            side=side.to_port_side()),
                        extra=dict(
                            data=ElkExtra(kind="port",
                                          data=PortData(
                                              direction="in" if side ==
                                              Direction.IN else "out"))),
                    )

                    node.ports.append(port)

                    if side == Direction.IN:
                        port_src = get_resource_port_id(it.id, Direction.OUT)
                        port_dst = port.id
                        if it.id in known_graph_nodes and node.id in known_graph_nodes: 
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
                            edge = None

                    else:
                        port_src = port.id
                        port_dst = get_resource_port_id(it.id, Direction.IN)
                        if it.id in known_graph_nodes and node.id in known_graph_nodes: 
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
                        
                        else:
                            edge = None

                    if edge: 
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
                            width=8,
                            height=8,
                            properties=elk.PortProperties(
                                side=Direction.IN.to_port_side()),
                            extra=dict(data=ElkExtra(
                                kind="port", data=PortData(direction="in"))),
                        ),
                        elk.Port(
                            id=get_resource_port_id(id, Direction.OUT),
                            width=8,
                            height=8,
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

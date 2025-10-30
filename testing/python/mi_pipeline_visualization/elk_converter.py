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
from beartype.typing import Literal, Union, List, Optional, Set, Tuple, Any, Dict
from numbers import Number
from enum import Enum
from beartype import beartype
import recipe_gui_schema as gui
from shapely.geometry import LineString, Polygon
from shapely.ops import unary_union
from PIL import Image
import numpy as np
from sklearn.cluster import KMeans


class PortData(BaseModel, extra="forbid"):
    direction: Literal["in", "out"]


class EdgeData(BaseModel, extra="forbid"):
    source: NodeDataUnion
    target: NodeDataUnion


class HyperEdgeData(BaseModel):
    polygon: List[Tuple[float, float]]
    pattern: Optional[Dict[str, Any]] = None


@beartype
class ElkExtra(BaseModel, extra="forbid"):
    data: Union[NodeDataUnion, EdgeData, PortData, HyperEdgeData]
    machine: Optional[gui.MachineData] = None
    kind: Literal["fluid_node", "item_node", "recipe_node", "port", "edge",
                  "hyperedge"]


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

    if graph.edges:
        for edge in graph.edges:
            # extra: ElkExtra = edge.extra["data"]
            subnodes.append(
                typ.Command(
                    name="edge",
                    args=[
                        typ.Literal(value=edge.model_dump(exclude_none=True))
                    ],
                ))


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


def _collect_all_edges(node: elk.Graph, all_edges: List[elk.Edge]) -> None:
    if hasattr(node, 'edges') and node.edges:
        all_edges.extend(node.edges)

    if hasattr(node, 'children') and node.children:
        for child in node.children:
            _collect_all_edges(child, all_edges)


from collections import defaultdict


def get_edge_groups_by_shared_ports(graph: elk.Graph) -> List[List[elk.Edge]]:
    port_to_edges = defaultdict(list)
    for edge in graph.edges:
        source_ports = []
        target_ports = []

        if edge.sourcePort:
            source_ports.append((edge.source, edge.sourcePort))
        elif edge.source:
            source_ports.append((edge.source, None))
        elif edge.sources:
            for source in edge.sources:
                source_ports.append((source, None))

        if edge.targetPort:
            target_ports.append((edge.target, edge.targetPort))
        elif edge.target:
            target_ports.append((edge.target, None))
        elif edge.targets:
            for target in edge.targets:
                target_ports.append((target, None))

        for port_key in source_ports + target_ports:
            port_to_edges[port_key].append(edge)

    edge_groups = []
    processed_edges = set()

    for edges_list in port_to_edges.values():
        if len(edges_list) == 1:
            edge_groups.append(edges_list)

    for edges_list in port_to_edges.values():
        if len(edges_list) <= 1:
            continue

        group = []
        for edge in edges_list:
            if id(edge) not in processed_edges:
                group.append(edge)
                processed_edges.add(id(edge))

        if len(group) > 1:
            edge_groups.append(group)

        else:
            log.info(group)

    return edge_groups


from pathlib import Path
from typing import List, Tuple
from dataclasses import dataclass
from beartype import beartype
import matplotlib.font_manager as fm
from fontTools.ttLib import TTFont
from fontTools.pens.boundsPen import BoundsPen
import matplotlib.pyplot as plt
import matplotlib.patches as patches

_font_cache = {}


@dataclass
class SplitResult:
    lines: List[str]
    height: Number


@beartype
def get_font_metrics(font_path: str) -> Tuple[TTFont, Number]:
    if font_path in _font_cache:
        return _font_cache[font_path]

    ttfont = TTFont(font_path)
    units_per_em = ttfont["head"].unitsPerEm
    _font_cache[font_path] = (ttfont, units_per_em)
    return ttfont, units_per_em


@beartype
def get_text_width(text: str, font_path: str, font_size: Number) -> Number:
    ttfont, units_per_em = get_font_metrics(font_path)
    cmap = ttfont.getBestCmap()
    hmtx = ttfont["hmtx"]

    total_width = 0
    for char in text:
        if ord(char) in cmap:
            glyph_name = cmap[ord(char)]
            advance_width, _ = hmtx[glyph_name]
            total_width += advance_width

    return (total_width / units_per_em) * font_size


@beartype
def get_line_height(font_path: str, font_size: Number) -> Number:
    ttfont, units_per_em = get_font_metrics(font_path)
    hhea = ttfont["hhea"]
    line_height = hhea.ascent - hhea.descent + hhea.lineGap
    return (line_height / units_per_em) * font_size


@beartype
def get_break_priority(char: str) -> int:
    if char == " ":
        return 0
    elif char in ".,;!?":
        return 1
    elif char in ":_-/\\|":
        return 2
    elif char.isalnum():
        return 4
    else:
        return 3


@beartype
def split_text_to_fit(text: str, expected_width: Number, font_path: str,
                      font_size: Number) -> SplitResult:
    if not text:
        return SplitResult(lines=[], height=0.0)

    n = len(text)
    char_widths = []
    for char in text:
        char_widths.append(get_text_width(char, font_path, font_size))

    dp = [float("inf")] * (n + 1)
    parent = [-1] * (n + 1)
    dp[0] = 0

    for i in range(n):
        if dp[i] == float("inf"):
            continue

        current_width = 0
        for j in range(i, n):
            current_width += char_widths[j]
            if current_width > expected_width:
                break

            break_penalty = 0
            if j < n - 1:
                break_penalty = get_break_priority(text[j])

            new_cost = dp[i] + break_penalty
            if new_cost < dp[j + 1]:
                dp[j + 1] = new_cost
                parent[j + 1] = i

    breaks = []
    current = n
    while parent[current] != -1:
        breaks.append(parent[current])
        current = parent[current]
    breaks.reverse()

    lines = []
    start = 0
    for break_point in breaks:
        if break_point > start:
            lines.append(text[start:break_point])
            start = break_point
    if start < n:
        lines.append(text[start:])

    line_height = get_line_height(font_path, font_size)
    total_height = len(lines) * line_height

    return SplitResult(lines=lines, height=total_height)


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


@beartype
def get_recipe_id(rec: RecipeNodeData, recipe_idx: int) -> str:
    return f"{rec.type}-{recipe_idx}"


def _add_recipe_node(
    graph: ig.Graph,
    result: elk.Graph,
    recipe_idx: int,
    v: ig.Vertex,
    known_graph_nodes: Set[str],
):
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
            properties=elk.PortProperties(side=side.to_port_side()),
            extra=dict(data=ElkExtra(
                kind="port",
                data=PortData(
                    direction="in" if side == Direction.IN else "out"),
            )),
            labels=[
                _single_line_label(
                    f"port-{get_port_id(kind, idx, side)}",
                    text=it.id.split(":")[-1].replace("_", " "),
                    font_size=4,
                    extra_extra=dict(
                        kind=kind + "-" +
                        ("in" if side == Direction.IN else "out")))
            ],
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
                        kind="edge", data=EdgeData(source=it, target=rec))),
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
                    extra=dict(data=ElkExtra(
                        kind="edge", data=EdgeData(source=rec, target=it))),
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


@beartype
def _single_line_label(
    id: str,
    text: str,
    font_size: Number,
    extra_extra: Dict[str, Any] = dict()) -> elk.Label:
    font_path = fm.findfont(fm.FontProperties(family="DejaVu Sans"))
    expected_width = get_text_width(text, font_path, font_size) + 4
    expected_height = get_line_height(font_path, font_size)

    return elk.Label(
        id=id,
        text=text,
        width=expected_width,
        height=expected_height,
        extra=dict(
            expected_width=expected_width,
            expected_height=expected_height,
            font_size=font_size,
            **extra_extra,
        ),
    )


@beartype
def _add_fluid_item_node(
    graph: ig.Graph,
    result: elk.Graph,
    v: ig.Vertex,
    known_graph_nodes: Set[str],
):
    if v["node_type"] == "fluid":
        fluid: FluidNodeData = v["data"]
        id = fluid.id

    else:
        item: FluidNodeData = v["data"]
        id = item.id

    assert 0 < graph.outdegree(v) + graph.indegree(v), f"{id}"

    node = elk.Node(
        id=f"{id}",
        width=50,
        height=50,
        extra=dict(data=ElkExtra(data=v["data"], kind=v["node_type"] +
                                 "_node")),
        properties={
            "nodeLabels.placement": "[H_CENTER, V_TOP, OUTSIDE]",
            "portLabels.placement": "NEXT_TO_PORT_OF_POSSIBLE",
        },
        ports=[
            elk.Port(
                id=get_resource_port_id(id, Direction.IN),
                width=8,
                height=8,
                properties=elk.PortProperties(
                    side=Direction.IN.to_port_side()),
                extra=dict(
                    data=ElkExtra(kind="port", data=PortData(direction="in"))),
            ),
            elk.Port(
                id=get_resource_port_id(id, Direction.OUT),
                width=8,
                height=8,
                properties=elk.PortProperties(
                    side=Direction.OUT.to_port_side()),
                extra=dict(
                    data=ElkExtra(kind="port", data=PortData(
                        direction="out"))),
            ),
        ],
        labels=[
            _single_line_label(
                id=f"resource-label-{id}",
                text=id.split(":")[-1].replace("_", " "),
                font_size=8.0,
                extra_extra=dict(type=v["node_type"]),
            )
        ])

    result.children.append(node)


@beartype
def compute_hyperedge_polygon(sections: List[elk.EdgeSection],
                              width: float) -> List[Tuple[float, float]]:
    buffered_lines = []

    for section in sections:
        points = [(section.startPoint.x, section.startPoint.y)]
        if section.bendPoints:
            points.extend([(bp.x, bp.y) for bp in section.bendPoints])
        points.append((section.endPoint.x, section.endPoint.y))

        if len(points) >= 2:
            line = LineString(points)
            buffered_line = line.buffer(width / 2, cap_style=2, join_style=2)
            buffered_lines.append(buffered_line)

    if not buffered_lines:
        return []

    combined_polygon = unary_union(buffered_lines)

    if hasattr(combined_polygon, 'exterior'):
        return list(combined_polygon.exterior.coords)
    else:
        return []


def extract_color_palette(texture_path: str, n_colors: int = 3) -> dict:
    img = Image.open(texture_path).convert('RGB')
    pixels = np.array(img).reshape(-1, 3)

    kmeans = KMeans(n_clusters=n_colors, random_state=42)
    kmeans.fit(pixels)

    colors = []
    for center in kmeans.cluster_centers_:
        colors.append({
            "r":
            int(center[0]),
            "g":
            int(center[1]),
            "b":
            int(center[2]),
            "hex":
            f"#{int(center[0]):02x}{int(center[1]):02x}{int(center[2]):02x}"
        })

    return {"palette": colors}


@beartype
def merge_edges_into_hyperedge(edges: List[elk.Edge]) -> elk.Edge:
    if not edges:
        raise ValueError("Cannot merge empty list of edges")

    if len(edges) == 1:
        return edges[0]

    all_sources = set()
    all_targets = set()
    all_source_ports = set()
    all_target_ports = set()
    all_sections = []
    all_junction_points = []
    all_labels = []

    sources_extra: List[NodeDataUnion] = []
    targets_extra: List[NodeDataUnion] = []

    for edge in edges:
        if edge.source:
            all_sources.add(edge.source)
        if edge.sources:
            all_sources.update(edge.sources)

        if edge.target:
            all_targets.add(edge.target)
        if edge.targets:
            all_targets.update(edge.targets)

        if edge.sourcePort:
            all_source_ports.add((edge.source, edge.sourcePort))
        if edge.targetPort:
            all_target_ports.add((edge.target, edge.targetPort))

        if edge.sections:
            all_sections.extend(edge.sections)

        if edge.junctionPoints:
            all_junction_points.extend(edge.junctionPoints)

        if edge.labels:
            all_labels.extend(edge.labels)

        if edge.extra and "data" in edge.extra:
            assert isinstance(edge, elk.Edge)
            assert isinstance(edge.extra, dict)
            dat: ElkExtra = edge.extra["data"]
            assert isinstance(dat, ElkExtra), f"{type(dat)}, {dat}"
            extra_edge: EdgeData = dat.data
            if isinstance(extra_edge.source, (FluidNodeData, ItemNodeData)):
                sources_extra.append(extra_edge.source)

            if isinstance(extra_edge.target, (FluidNodeData, ItemNodeData)):
                targets_extra.append(extra_edge.target)

    unique_extra_sources_ids = list(set(e.id for e in sources_extra))
    unique_extra_targets_ids = list(set(e.id for e in targets_extra))

    @beartype
    def get_texture(node: NodeDataUnion) -> Optional[str]:
        match node:
            case ItemNodeData():
                return node.image

            case FluidNodeData():
                return node.image

    if len(unique_extra_sources_ids) == 1:
        source_texture = get_texture(sources_extra[0])

    else:
        source_texture = None

    if len(unique_extra_targets_ids) == 1:
        target_texture = get_texture(targets_extra[0])

    else:
        target_texture = None

    if bool(source_texture) != bool(target_texture):
        if source_texture:
            hyperedge_texture = source_texture

        else:
            hyperedge_texture = target_texture

    else:
        hyperedge_texture = None

    merged_edge = elk.Edge(
        id=f"merged_{hash(tuple(e.id for e in edges))}",
        sources=list(all_sources) if len(all_sources) > 1 else None,
        targets=list(all_targets) if len(all_targets) > 1 else None,
        source=list(all_sources)[0] if len(all_sources) == 1 else None,
        target=list(all_targets)[0] if len(all_targets) == 1 else None,
        sections=all_sections,
        junctionPoints=all_junction_points if all_junction_points else None,
        labels=all_labels if all_labels else None,
        extra=dict(data=ElkExtra(
            data=HyperEdgeData(
                polygon=compute_hyperedge_polygon(all_sections, width=4.0),
                pattern=extract_color_palette(hyperedge_texture
                                              ) if hyperedge_texture else None,
            ),
            kind="hyperedge",
        )),
    )

    return merged_edge


@beartype
def group_multi_layout(graph: elk.Graph):
    grouped_multi_edges: List[elk.Edge] = []

    for group in get_edge_groups_by_shared_ports(graph):
        if len(group) == 1:
            grouped_multi_edges.append(group[0])

        else:
            grouped_multi_edges.append(merge_edges_into_hyperedge(group))

    log.info(f"Len prev edges {len(graph.edges)}")
    graph.edges = grouped_multi_edges
    log.info(f"Len new edges {len(graph.edges)}")


@beartype
def convert_to_elk(graph: ig.Graph) -> elk.Graph:

    result = elk.Graph(
        id="root",
        children=[],
        edges=[],
        ports=[],
        layoutOptions={
            "org.eclipse.elk.spacing.edgeEdge": 15,
            "org.eclipse.elk.spacing.edgeNode": 20,
            "org.eclipse.elk.spacing.labelNode": 10,
            "org.eclipse.elk.layered.spacing.edgeNodeBetweenLayers": 30,
            "org.eclipse.elk.layered.spacing.edgeEdgeBetweenLayers": 20,
            "org.eclipse.elk.spacing.labelPortVertical": 0,
        })

    known_graph_nodes: Set[str] = set()

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
                _add_recipe_node(graph, result, recipe_idx, v,
                                 known_graph_nodes)

            case "fluid" | "item":
                _add_fluid_item_node(graph, result, v, known_graph_nodes)

            case _:
                raise ValueError(f"{v['node_type']}")

    return result

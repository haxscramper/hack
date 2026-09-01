from __future__ import annotations

import math
import re
from dataclasses import dataclass

from beartype import beartype
from beartype.typing import Optional
import pydot

from graphviz_viewer.task_graph_types import (
    EdgeKind,
    Group,
    GroupKind,
    LayoutEdge,
    Point,
    Rect,
    SemanticViews,
    Size,
)

GROUP_PADDING = 24.0
GROUP_LABEL_HEIGHT = 30.0
GROUP_SPACING = 28.0
BAND_SPACING = 36.0
GRAPHVIZ_DPI = 72.0
CALENDAR_DAY_WIDTH = 280.0
CALENDAR_HOUR_HEIGHT = 80.0
CALENDAR_FIRST_HOUR = 7
CALENDAR_LAST_HOUR = 23


@dataclass
class GraphvizResult:
    width: float
    height: float
    nodes: dict[str, Rect]
    edges: dict[str, list[Point]]


@beartype
def unquote(value: str) -> str:
    if value.startswith("\"") and value.endswith("\""):
        return value[1:-1]
    return value


@beartype
def parse_graphviz_point(token: str) -> Optional[Point]:
    normalized = token.strip()
    if normalized.startswith("e,") or normalized.startswith("s,"):
        normalized = normalized[2:]

    parts = normalized.split(",")
    if len(parts) != 2:
        return None

    return Point(x=float(parts[0]), y=float(parts[1]))


@beartype
def parse_graphviz_spline(
    value: str,
    lower_x: float,
    upper_y: float,
) -> list[Point]:
    normalized = unquote(value).replace("\\\n", "")
    result: list[Point] = []

    for token in re.split(r"\s+", normalized):
        point = parse_graphviz_point(token)
        if point is not None:
            result.append(Point(
                x=point.x - lower_x,
                y=upper_y - point.y,
            ))

    return result


@beartype
def graphviz_bounding_box(
    parsed: pydot.Dot,
    group_id: str,
) -> tuple[float, float, float, float]:
    bounding_box = parsed.get_attributes().get("bb")

    if bounding_box is None:
        for attributes in parsed.get_graph_defaults():
            bounding_box = attributes.get("bb")
            if bounding_box is not None:
                break

    if bounding_box is None:
        graph_statements = parsed.get_node("graph")
        for statement in graph_statements:
            bounding_box = statement.get("bb")
            if bounding_box is not None:
                break

    if bounding_box is None:
        raise RuntimeError(
            f"Graphviz output for group {group_id} does not contain a bb attribute"
        )

    values = unquote(bounding_box).split(",")
    if len(values) != 4:
        raise RuntimeError(
            f"Graphviz returned malformed bounding box {bounding_box!r} "
            f"for group {group_id}")

    lower_x, lower_y, upper_x, upper_y = [float(value) for value in values]
    return lower_x, lower_y, upper_x, upper_y


@beartype
def graphviz_layout(group: Group) -> GraphvizResult:
    graph = pydot.Dot(
        graph_type="digraph",
        rankdir="LR",
        splines="spline",
        overlap="false",
        nodesep="0.45",
        ranksep="0.7",
        margin="0",
        bgcolor="transparent",
    )

    sizes = {node.unique_id: node.size for node in group.nodes}
    aliases = {
        node.unique_id: f"n{index}"
        for index, node in enumerate(group.nodes)
    }
    rectangle_ids = {
        alias: rectangle_id
        for rectangle_id, alias in aliases.items()
    }

    for node in group.nodes:
        graph.add_node(
            pydot.Node(
                aliases[node.unique_id],
                label="",
                shape="box",
                fixedsize="true",
                width=str(node.size.width / GRAPHVIZ_DPI),
                height=str(node.size.height / GRAPHVIZ_DPI),
            ))

    for edge in group.edges:
        attributes = {
            "id": edge.unique_id,
            "task_edge_id": edge.unique_id,
        }
        if edge.kind == EdgeKind.RELATED:
            attributes["constraint"] = "false"
            attributes["style"] = "dashed"

        source_alias = aliases[edge.source_rect_id]
        target_alias = aliases[edge.target_rect_id]
        graph.add_edge(pydot.Edge(
            source_alias,
            target_alias,
            **attributes,
        ))

    output = graph.create_dot(prog="dot").decode("utf-8")
    parsed_graphs = pydot.graph_from_dot_data(output)
    if len(parsed_graphs) != 1:
        raise RuntimeError(f"Graphviz returned {len(parsed_graphs)} graphs "
                           f"for group {group.unique_id}")

    parsed = parsed_graphs[0]
    lower_x, lower_y, upper_x, upper_y = graphviz_bounding_box(
        parsed,
        group.unique_id,
    )
    width = upper_x - lower_x
    height = upper_y - lower_y
    node_rects: dict[str, Rect] = {}

    for parsed_node in parsed.get_nodes():
        alias = unquote(parsed_node.get_name())
        if alias not in rectangle_ids:
            continue

        rectangle_id = rectangle_ids[alias]
        position = parsed_node.get_pos()
        if position is None:
            raise RuntimeError(
                f"Graphviz node {alias} for rectangle {rectangle_id} "
                f"in group {group.unique_id} has no position")

        center_x, center_y = [
            float(value) for value in unquote(position).split(",")
        ]
        size = sizes[rectangle_id]
        node_rects[rectangle_id] = Rect(
            x=center_x - lower_x - size.width / 2.0,
            y=upper_y - center_y - size.height / 2.0,
            width=size.width,
            height=size.height,
        )

    missing_ids = sorted(set(sizes) - set(node_rects))
    if missing_ids:
        raise RuntimeError(f"Graphviz omitted rectangles {missing_ids} "
                           f"from group {group.unique_id}")

    edge_points: dict[str, list[Point]] = {}
    for parsed_edge in parsed.get_edges():
        edge_id = parsed_edge.get("task_edge_id")
        position = parsed_edge.get_pos()
        if edge_id is None or position is None:
            continue

        edge_points[unquote(edge_id)] = parse_graphviz_spline(
            position,
            lower_x,
            upper_y,
        )

    return GraphvizResult(
        width=width,
        height=height,
        nodes=node_rects,
        edges=edge_points,
    )


@beartype
def layout_band(group: Group) -> None:
    result = graphviz_layout(group)

    for node in group.nodes:
        graphviz_rect = result.nodes[node.unique_id]
        node.geometry = graphviz_rect.translated(
            Point(GROUP_PADDING, GROUP_PADDING + GROUP_LABEL_HEIGHT))

    for edge in group.edges:
        edge.points = [
            Point(
                point.x + GROUP_PADDING,
                point.y + GROUP_PADDING + GROUP_LABEL_HEIGHT,
            ) for point in result.edges.get(edge.unique_id, [])
        ]

    group.geometry = Rect(
        x=0.0,
        y=0.0,
        width=result.width + 2.0 * GROUP_PADDING,
        height=result.height + 2.0 * GROUP_PADDING + GROUP_LABEL_HEIGHT,
    )


@beartype
def find_node_rect(
        group: Group,
        rect_id: str,
        offset: Point = Point(0.0, 0.0),
) -> Optional[Rect]:
    for node in group.nodes:
        if node.unique_id == rect_id:
            if node.geometry is None:
                raise RuntimeError(f"Node {rect_id} has no geometry")
            return node.geometry.translated(offset)

    for nested in group.nested_groups:
        if nested.geometry is None:
            raise RuntimeError(f"Group {nested.unique_id} has no geometry")
        result = find_node_rect(
            nested,
            rect_id,
            Point(offset.x + nested.geometry.x, offset.y + nested.geometry.y),
        )
        if result is not None:
            return result

    return None


@beartype
def route_orthogonal_edges(group: Group) -> None:
    for edge in group.edges:
        source = find_node_rect(group, edge.source_rect_id)
        target = find_node_rect(group, edge.target_rect_id)
        if source is None:
            raise KeyError(
                f"Edge {edge.unique_id} source rectangle {edge.source_rect_id} is missing"
            )
        if target is None:
            raise KeyError(
                f"Edge {edge.unique_id} target rectangle {edge.target_rect_id} is missing"
            )

        source_point = Point(
            source.x + source.width,
            source.y + source.height / 2.0,
        )
        target_point = Point(
            target.x,
            target.y + target.height / 2.0,
        )
        middle_x = (source_point.x + target_point.x) / 2.0
        edge.points = [
            source_point,
            Point(middle_x, source_point.y),
            Point(middle_x, target_point.y),
            target_point,
        ]


@beartype
def layout_lane(group: Group) -> None:
    x = GROUP_PADDING
    maximum_height = 0.0

    for nested in group.nested_groups:
        layout_band(nested)
        if nested.geometry is None:
            raise RuntimeError(f"Band {nested.unique_id} has no geometry")

        nested.geometry = Rect(
            x=x,
            y=GROUP_PADDING + GROUP_LABEL_HEIGHT,
            width=nested.geometry.width,
            height=nested.geometry.height,
        )
        x += nested.geometry.width + BAND_SPACING
        maximum_height = max(maximum_height, nested.geometry.height)

    width = max(320.0, x - BAND_SPACING + GROUP_PADDING)
    group.geometry = Rect(
        x=0.0,
        y=0.0,
        width=width,
        height=maximum_height + 2.0 * GROUP_PADDING + GROUP_LABEL_HEIGHT,
    )
    route_orthogonal_edges(group)


@beartype
def layout_graph_root(group: Group) -> None:
    y = GROUP_PADDING + GROUP_LABEL_HEIGHT
    maximum_width = 0.0

    for nested in group.nested_groups:
        layout_lane(nested)
        if nested.geometry is None:
            raise RuntimeError(f"Lane {nested.unique_id} has no geometry")

        nested.geometry = Rect(
            x=GROUP_PADDING,
            y=y,
            width=nested.geometry.width,
            height=nested.geometry.height,
        )
        y += nested.geometry.height + GROUP_SPACING
        maximum_width = max(maximum_width, nested.geometry.width)

    group.geometry = Rect(
        x=0.0,
        y=0.0,
        width=maximum_width + 2.0 * GROUP_PADDING,
        height=y - GROUP_SPACING + GROUP_PADDING,
    )
    route_orthogonal_edges(group)


@beartype
def overlap_column(
    intervals: list[tuple[float, float, int]],
    start: float,
    end: float,
) -> int:
    occupied: set[int] = set()
    for previous_start, previous_end, column in intervals:
        if start < previous_end and previous_start < end:
            occupied.add(column)

    column = 0
    while column in occupied:
        column += 1
    return column


@beartype
def layout_calendar_day(group: Group) -> None:
    intervals: list[tuple[float, float, int]] = []
    sorted_nodes = sorted(
        group.nodes,
        key=lambda node: (
            node.calendar_start,
            node.calendar_end,
            node.unique_id,
        ),
    )

    for node in sorted_nodes:
        if node.calendar_start is None or node.calendar_end is None:
            raise RuntimeError(
                f"Calendar node {node.unique_id} has no interval")

        start_minutes = (
            (node.calendar_start.hour - CALENDAR_FIRST_HOUR) * 60 +
            node.calendar_start.minute)
        end_minutes = ((node.calendar_end.hour - CALENDAR_FIRST_HOUR) * 60 +
                       node.calendar_end.minute)
        start_value = float(start_minutes)
        end_value = float(end_minutes)
        column = overlap_column(intervals, start_value, end_value)
        intervals.append((start_value, end_value, column))

        offset = float(column) * 18.0
        y = GROUP_LABEL_HEIGHT + start_value * CALENDAR_HOUR_HEIGHT / 60.0
        height = max(
            22.0,
            (end_value - start_value) * CALENDAR_HOUR_HEIGHT / 60.0,
        )
        node.size = Size(
            width=CALENDAR_DAY_WIDTH - 66.0 - offset,
            height=height,
        )
        node.geometry = Rect(
            x=54.0 + offset,
            y=y,
            width=node.size.width,
            height=node.size.height,
        )

    group.geometry = Rect(
        x=0.0,
        y=0.0,
        width=CALENDAR_DAY_WIDTH,
        height=(
            GROUP_LABEL_HEIGHT +
            (CALENDAR_LAST_HOUR - CALENDAR_FIRST_HOUR) * CALENDAR_HOUR_HEIGHT),
    )


@beartype
def layout_calendar_root(group: Group) -> None:
    x = GROUP_PADDING

    for nested in group.nested_groups:
        layout_calendar_day(nested)
        if nested.geometry is None:
            raise RuntimeError(
                f"Calendar day {nested.unique_id} has no geometry")

        nested.geometry = Rect(
            x=x,
            y=GROUP_PADDING + GROUP_LABEL_HEIGHT,
            width=nested.geometry.width,
            height=nested.geometry.height,
        )
        x += nested.geometry.width

    height = max(nested.geometry.height for nested in group.nested_groups
                 if nested.geometry is not None)
    group.geometry = Rect(
        x=0.0,
        y=0.0,
        width=x + GROUP_PADDING,
        height=height + 2.0 * GROUP_PADDING + GROUP_LABEL_HEIGHT,
    )


@beartype
def layout_views(views: SemanticViews) -> SemanticViews:
    layout_graph_root(views.graph)
    layout_calendar_root(views.calendar)
    return views

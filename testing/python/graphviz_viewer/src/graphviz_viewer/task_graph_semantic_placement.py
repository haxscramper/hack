from collections import defaultdict
from datetime import datetime, timedelta

from beartype import beartype

from graphviz_viewer.task_graph_types import (
    EdgeKind,
    Group,
    GroupKind,
    InputCollection,
    InputEdge,
    InputNode,
    LayoutEdge,
    NodeRect,
    SemanticViews,
    Size,
    TodoState,
)

NODE_PADDING_X = 14.0
NODE_PADDING_Y = 10.0


@beartype
def node_size(node: InputNode) -> Size:
    return Size(
        width=node.content_size.width + 2.0 * NODE_PADDING_X,
        height=node.content_size.height + 2.0 * NODE_PADDING_Y,
    )


@beartype
def has_tag(node: InputNode, value: tuple[str, ...]) -> bool:
    return value in node.tags


@beartype
def task_band(
    node: InputNode,
    blocked_ids: set[str],
) -> str:
    if node.todo_state == TodoState.DONE:
        return "Done"
    if has_tag(node, ("state", "active")) or node.todo_state == TodoState.WIP:
        return "Current scope"
    if has_tag(node, ("state", "slot_into_current")):
        return "Current scope"
    if has_tag(node, ("state", "consider_for_current")):
        return "Current scope"
    if node.unique_id in blocked_ids:
        return "Blocked"
    if has_tag(
            node,
        ("state", "can_take_next")) or node.todo_state == TodoState.NEXT:
        return "Next"
    if has_tag(node, ("state", "waiting_for_slot")):
        return "Available"
    return "Later"


@beartype
def make_layout_edge(edge: InputEdge, rect_ids: dict[str, str]) -> LayoutEdge:
    return LayoutEdge(
        unique_id=edge.unique_id,
        source_rect_id=rect_ids[edge.source_id],
        target_rect_id=rect_ids[edge.target_id],
        kind=edge.kind,
    )


@beartype
def build_graph_view(collection: InputCollection) -> Group:
    blocked_ids = {
        edge.target_id
        for edge in collection.edges if edge.kind == EdgeKind.BLOCKED
    }
    band_order = [
        "Current scope",
        "Next",
        "Available",
        "Blocked",
        "Later",
        "Done",
    ]
    nodes_by_id = {node.unique_id: node for node in collection.nodes}
    lanes: dict[str, dict[str, Group]] = defaultdict(dict)
    rect_ids: dict[str, str] = {}
    band_by_node: dict[str, Group] = {}
    lane_by_node: dict[str, Group] = {}

    graph_root = Group(
        unique_id="graph-root",
        label="Task graph",
        kind=GroupKind.GRAPH_ROOT,
    )

    for lane_name in sorted({node.lane for node in collection.nodes}):
        lane_group = Group(
            unique_id=f"lane:{lane_name}",
            label=lane_name,
            kind=GroupKind.LANE,
        )
        graph_root.nested_groups.append(lane_group)

        for band_name in band_order:
            band_group = Group(
                unique_id=f"lane:{lane_name}:band:{band_name}",
                label=band_name,
                kind=GroupKind.BAND,
            )
            lane_group.nested_groups.append(band_group)
            lanes[lane_name][band_name] = band_group

    lane_groups = {group.label: group for group in graph_root.nested_groups}

    for node in collection.nodes:
        band_name = task_band(node, blocked_ids)
        band_group = lanes[node.lane][band_name]
        rect_id = f"graph:{node.unique_id}"
        node_rect = NodeRect(
            unique_id=rect_id,
            source_id=node.unique_id,
            size=node_size(node),
        )
        band_group.nodes.append(node_rect)
        rect_ids[node.unique_id] = rect_id
        band_by_node[node.unique_id] = band_group
        lane_by_node[node.unique_id] = lane_groups[node.lane]

    for edge in collection.edges:
        if edge.kind == EdgeKind.NESTED:
            continue
        if edge.source_id not in nodes_by_id:
            raise KeyError(
                f"Edge {edge.unique_id} has missing source {edge.source_id}")
        if edge.target_id not in nodes_by_id:
            raise KeyError(
                f"Edge {edge.unique_id} has missing target {edge.target_id}")

        layout_edge = make_layout_edge(edge, rect_ids)
        source_band = band_by_node[edge.source_id]
        target_band = band_by_node[edge.target_id]
        source_lane = lane_by_node[edge.source_id]
        target_lane = lane_by_node[edge.target_id]

        if source_band is target_band:
            source_band.edges.append(layout_edge)
        elif source_lane is target_lane:
            source_lane.edges.append(layout_edge)
        else:
            graph_root.edges.append(layout_edge)

    for lane_group in graph_root.nested_groups:
        lane_group.nested_groups = [
            group for group in lane_group.nested_groups
            if group.nodes or group.edges
        ]

    return graph_root


@beartype
def calendar_range(collection: InputCollection) -> tuple[datetime, datetime]:
    starts = [
        allocation.start for node in collection.nodes
        for allocation in node.calendar_allocations
    ]
    if not starts:
        raise ValueError("Calendar visualization has no scheduled allocations")

    first = min(starts).replace(hour=0, minute=0, second=0, microsecond=0)
    return first, first + timedelta(days=5)


@beartype
def build_calendar_view(collection: InputCollection) -> Group:
    first_day, range_end = calendar_range(collection)
    root = Group(
        unique_id="calendar-root",
        label="Calendar",
        kind=GroupKind.CALENDAR_ROOT,
    )
    day_groups: dict[datetime, Group] = {}

    day = first_day
    while day < range_end:
        group = Group(
            unique_id=f"calendar-day:{day.date().isoformat()}",
            label=day.strftime("%A %Y-%m-%d"),
            kind=GroupKind.CALENDAR_DAY,
        )
        root.nested_groups.append(group)
        day_groups[day] = group
        day += timedelta(days=1)

    for node in collection.nodes:
        for index, allocation in enumerate(node.calendar_allocations):
            day_key = allocation.start.replace(
                hour=0,
                minute=0,
                second=0,
                microsecond=0,
            )
            if day_key not in day_groups:
                continue

            duration = allocation.end - allocation.start
            if duration <= timedelta(0):
                raise ValueError(
                    f"Allocation {index} for {node.unique_id} has non-positive duration"
                )

            day_groups[day_key].nodes.append(
                NodeRect(
                    unique_id=f"calendar:{node.unique_id}:{index}",
                    source_id=node.unique_id,
                    size=Size(width=220.0, height=40.0),
                    calendar_start=allocation.start,
                    calendar_end=allocation.end,
                ))

    return root


@beartype
def build_semantic_views(collection: InputCollection) -> SemanticViews:
    return SemanticViews(
        graph=build_graph_view(collection),
        calendar=build_calendar_view(collection),
    )

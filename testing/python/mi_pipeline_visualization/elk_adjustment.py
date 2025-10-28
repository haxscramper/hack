from elk_schema import *

from typing import List, Tuple, Set, Dict
from collections import defaultdict
import logging


def adjust_graph_for_non_overlapping_edges(graph: Graph,
                                           edge_width: float = 2) -> None:
    all_edges = []
    _collect_all_edges(graph, all_edges)

    segments = []
    for edge in all_edges:
        edge_segments = _extract_edge_segments(edge)
        segments.extend(edge_segments)

    parallel_groups = _group_parallel_segments(segments, edge_width)

    adjustments = {}
    for group in parallel_groups:
        if len(group) <= 1:
            continue

        group_adjustments = _calculate_adjustments_for_group(group, edge_width)
        adjustments.update(group_adjustments)

    _apply_adjustments(graph, adjustments)


def _collect_all_edges(node: Graph, all_edges: List[Edge]) -> None:
    if hasattr(node, 'edges') and node.edges:
        all_edges.extend(node.edges)

    if hasattr(node, 'children') and node.children:
        for child in node.children:
            _collect_all_edges(child, all_edges)


def _extract_edge_segments(edge: Edge) -> List[Tuple[Point, Point, Edge, int]]:
    segments = []

    if edge.sections:
        for section in edge.sections:
            points = [section.startPoint]
            if section.bendPoints:
                points.extend(section.bendPoints)
            points.append(section.endPoint)

            for i in range(len(points) - 1):
                segments.append((points[i], points[i + 1], edge, i))
    else:
        if edge.sourcePoint and edge.targetPoint:
            points = [edge.sourcePoint]
            if edge.bendPoints:
                points.extend(edge.bendPoints)
            points.append(edge.targetPoint)

            for i in range(len(points) - 1):
                segments.append((points[i], points[i + 1], edge, i))

    return segments


def _group_parallel_segments(
        segments: List[Tuple[Point, Point, Edge, int]],
        edge_width: float) -> List[List[Tuple[Point, Point, Edge, int]]]:
    parallel_groups = []
    used_segments = set()

    for i, segment1 in enumerate(segments):
        if i in used_segments:
            continue

        group = [segment1]
        used_segments.add(i)

        for j, segment2 in enumerate(segments):
            if j in used_segments or j <= i:
                continue

            if _are_segments_parallel_and_overlapping(segment1, segment2,
                                                      edge_width):
                group.append(segment2)
                used_segments.add(j)

        parallel_groups.append(group)

    return parallel_groups


def _are_segments_parallel_and_overlapping(seg1: Tuple[Point, Point, Edge,
                                                       int],
                                           seg2: Tuple[Point, Point, Edge,
                                                       int],
                                           edge_width: float) -> bool:
    p1_start, p1_end, _, _ = seg1
    p2_start, p2_end, _, _ = seg2

    if _is_horizontal(p1_start, p1_end) and _is_horizontal(p2_start, p2_end):
        if abs(p1_start.y - p2_start.y) < edge_width:
            return _ranges_overlap(p1_start.x, p1_end.x, p2_start.x, p2_end.x)
    elif _is_vertical(p1_start, p1_end) and _is_vertical(p2_start, p2_end):
        if abs(p1_start.x - p2_start.x) < edge_width:
            return _ranges_overlap(p1_start.y, p1_end.y, p2_start.y, p2_end.y)

    return False


def _is_horizontal(p1: Point, p2: Point) -> bool:
    return abs(p1.y - p2.y) < 0.001


def _is_vertical(p1: Point, p2: Point) -> bool:
    return abs(p1.x - p2.x) < 0.001


def _ranges_overlap(a1: float, a2: float, b1: float, b2: float) -> bool:
    min_a, max_a = min(a1, a2), max(a1, a2)
    min_b, max_b = min(b1, b2), max(b1, b2)
    return max_a > min_b and max_b > min_a


def _calculate_adjustments_for_group(
        group: List[Tuple[Point, Point, Edge, int]],
        edge_width: float) -> Dict[Tuple[Edge, int], Tuple[float, float]]:
    adjustments = {}

    if not group:
        return adjustments

    first_segment = group[0]
    p1_start, p1_end, _, _ = first_segment

    if _is_horizontal(p1_start, p1_end):
        base_y = p1_start.y
        for i, (p_start, p_end, edge, segment_idx) in enumerate(group):
            if i == 0:
                continue
            offset = i * edge_width
            new_y = base_y + offset
            adjustments[(edge, segment_idx)] = (0, new_y - p_start.y)
    elif _is_vertical(p1_start, p1_end):
        base_x = p1_start.x
        for i, (p_start, p_end, edge, segment_idx) in enumerate(group):
            if i == 0:
                continue
            offset = i * edge_width
            new_x = base_x + offset
            adjustments[(edge, segment_idx)] = (new_x - p_start.x, 0)

    return adjustments


def _apply_adjustments(
        graph: Graph, adjustments: Dict[Tuple[Edge, int],
                                        Tuple[float, float]]) -> None:
    all_edges = []
    _collect_all_edges(graph, all_edges)

    for edge in all_edges:
        _apply_edge_adjustments(edge, adjustments)


def _apply_edge_adjustments(
        edge: Edge, adjustments: Dict[Tuple[Edge, int], Tuple[float,
                                                              float]]) -> None:
    if edge.sections:
        for section in edge.sections:
            points = [section.startPoint]
            if section.bendPoints:
                points.extend(section.bendPoints)
            points.append(section.endPoint)

            for i in range(len(points) - 1):
                adjustment_key = (edge, i)
                if adjustment_key in adjustments:
                    dx, dy = adjustments[adjustment_key]
                    points[i].x += dx
                    points[i].y += dy
                    points[i + 1].x += dx
                    points[i + 1].y += dy
    else:
        points = [edge.sourcePoint] if edge.sourcePoint else []
        if edge.bendPoints:
            points.extend(edge.bendPoints)
        if edge.targetPoint:
            points.append(edge.targetPoint)

        for i in range(len(points) - 1):
            adjustment_key = (edge, i)
            if adjustment_key in adjustments:
                dx, dy = adjustments[adjustment_key]
                points[i].x += dx
                points[i].y += dy
                points[i + 1].x += dx
                points[i + 1].y += dy


def _update_node_positions_for_edge_adjustments(
        graph: Graph, adjustments: Dict[Tuple[Edge, int],
                                        Tuple[float, float]]) -> None:
    all_nodes = []
    _collect_all_nodes(graph, all_nodes)

    node_adjustments = defaultdict(lambda: {
        "min_x": 0,
        "max_x": 0,
        "min_y": 0,
        "max_y": 0
    })

    for (edge, segment_idx), (dx, dy) in adjustments.items():
        source_node = _find_node_by_id(all_nodes, edge.source)
        target_node = _find_node_by_id(all_nodes, edge.target)

        if source_node:
            node_adjustments[source_node.id]["min_x"] = min(
                node_adjustments[source_node.id]["min_x"], dx)
            node_adjustments[source_node.id]["max_x"] = max(
                node_adjustments[source_node.id]["max_x"], dx)
            node_adjustments[source_node.id]["min_y"] = min(
                node_adjustments[source_node.id]["min_y"], dy)
            node_adjustments[source_node.id]["max_y"] = max(
                node_adjustments[source_node.id]["max_y"], dy)

        if target_node:
            node_adjustments[target_node.id]["min_x"] = min(
                node_adjustments[target_node.id]["min_x"], dx)
            node_adjustments[target_node.id]["max_x"] = max(
                node_adjustments[target_node.id]["max_x"], dx)
            node_adjustments[target_node.id]["min_y"] = min(
                node_adjustments[target_node.id]["min_y"], dy)
            node_adjustments[target_node.id]["max_y"] = max(
                node_adjustments[target_node.id]["max_y"], dy)

    for node in all_nodes:
        if node.id in node_adjustments:
            adj = node_adjustments[node.id]

            if adj["min_x"] < 0 and node.x is not None:
                node.x += abs(adj["min_x"])
            if adj["min_y"] < 0 and node.y is not None:
                node.y += abs(adj["min_y"])

            width_increase = abs(adj["min_x"]) + adj["max_x"]
            height_increase = abs(adj["min_y"]) + adj["max_y"]

            if width_increase > 0 and node.width is not None:
                node.width += width_increase
            if height_increase > 0 and node.height is not None:
                node.height += height_increase


def _collect_all_nodes(container: Graph, all_nodes: List[Node]) -> None:
    if hasattr(container, 'children') and container.children:
        for child in container.children:
            all_nodes.append(child)
            _collect_all_nodes(child, all_nodes)


def _find_node_by_id(nodes: List[Node], node_id: str) -> Node:
    for node in nodes:
        if node.id == node_id:
            return node
    return None


def _adjust_port_positions(
        graph: Graph, adjustments: Dict[Tuple[Edge, int],
                                        Tuple[float, float]]) -> None:
    all_edges = []
    _collect_all_edges(graph, all_edges)

    all_nodes = []
    _collect_all_nodes(graph, all_nodes)

    for edge in all_edges:
        if not edge.sourcePort or not edge.targetPort:
            continue

        source_node = _find_node_by_id(all_nodes, edge.source)
        target_node = _find_node_by_id(all_nodes, edge.target)

        if source_node and source_node.ports:
            source_port = _find_port_by_id(source_node.ports, edge.sourcePort)
            if source_port:
                _adjust_port_for_edge(source_port, edge, adjustments, True)

        if target_node and target_node.ports:
            target_port = _find_port_by_id(target_node.ports, edge.targetPort)
            if target_port:
                _adjust_port_for_edge(target_port, edge, adjustments, False)


def _find_port_by_id(ports: List[Port], port_id: str) -> Port:
    for port in ports:
        if port.id == port_id:
            return port
    return None


def _adjust_port_for_edge(port: Port, edge: Edge,
                          adjustments: Dict[Tuple[Edge, int], Tuple[float,
                                                                    float]],
                          is_source: bool) -> None:
    for segment_idx in range(10):
        adjustment_key = (edge, segment_idx)
        if adjustment_key in adjustments:
            dx, dy = adjustments[adjustment_key]

            if segment_idx == 0 and is_source:
                if port.x is not None:
                    port.x += dx
                if port.y is not None:
                    port.y += dy
                break
            elif not is_source:
                max_segment_idx = _get_max_segment_index_for_edge(edge)
                if segment_idx == max_segment_idx:
                    if port.x is not None:
                        port.x += dx
                    if port.y is not None:
                        port.y += dy
                    break


def _get_max_segment_index_for_edge(edge: Edge) -> int:
    if edge.sections:
        max_idx = 0
        for section in edge.sections:
            points_count = 2
            if section.bendPoints:
                points_count += len(section.bendPoints)
            max_idx = max(max_idx, points_count - 2)
        return max_idx
    else:
        points_count = 2
        if edge.bendPoints:
            points_count += len(edge.bendPoints)
        return points_count - 2


def adjust_graph_for_non_overlapping_edges(graph: Graph,
                                           edge_width: float = 4) -> None:
    all_edges = []
    _collect_all_edges(graph, all_edges)

    segments = []
    for edge in all_edges:
        edge_segments = _extract_edge_segments(edge)
        segments.extend(edge_segments)

    parallel_groups = _group_parallel_segments(segments, edge_width)

    adjustments = {}
    for group in parallel_groups:
        if len(group) <= 1:
            continue

        group_adjustments = _calculate_adjustments_for_group(group, edge_width)
        adjustments.update(group_adjustments)

    _apply_adjustments(graph, adjustments)
    _update_node_positions_for_edge_adjustments(graph, adjustments)
    _adjust_port_positions(graph, adjustments)

    logging.info(
        f"Applied {len(adjustments)} edge segment adjustments to prevent overlapping"
    )

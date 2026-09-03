#!/usr/bin/env python

from __future__ import annotations

import shlex
from abc import ABC, abstractmethod
from collections.abc import Iterator

import pydot
from PyQt6.QtCore import QPointF, QRectF

from graphviz_viewer.graph_viewer_v2.layout_mapper import LayoutCluster, LayoutNode, LayoutEdge

LAYOUT_DPI = 96.0
CLUSTER_PADDING = 40.0


class LayoutExecutor(ABC):

    @abstractmethod
    def execute(
        self,
        root: LayoutCluster,
        rank_direction: str,
    ) -> None:
        raise NotImplementedError


class GraphvizLayoutExecutor(LayoutExecutor):

    def execute(
        self,
        root: LayoutCluster,
        rank_direction: str,
    ) -> None:
        graph = pydot.Dot(
            graph_type="digraph",
            strict=False,
        )
        graph.set_rankdir(rank_direction)
        graph.set("outputorder", "edgesfirst")

        nodes = list(self._walk_nodes(root))
        edges = list(self._walk_edges(root))

        dot_id_by_node_id = {
            node.unique_id: f"n{index}"
            for index, node in enumerate(nodes)
        }
        nodes_by_dot_id = {
            dot_id_by_node_id[node.unique_id]: node
            for node in nodes
        }

        for node in nodes:
            graph.add_node(
                pydot.Node(
                    dot_id_by_node_id[node.unique_id],
                    label="",
                    shape="box",
                    fixedsize="true",
                    width=f"{node.rectangle.width() / LAYOUT_DPI:.6f}",
                    height=f"{node.rectangle.height() / LAYOUT_DPI:.6f}",
                ))

        for edge in edges:
            if edge.source is None or edge.target is None:
                continue

            layout_properties = {
                key: str(value)
                for key, value in edge.properties.items()
                if key in {"constraint", "minlen", "weight"}
            }

            graph.add_edge(
                pydot.Edge(
                    dot_id_by_node_id[edge.source.unique_id],
                    dot_id_by_node_id[edge.target.unique_id],
                    **layout_properties,
                ))

        plain = graph.create(
            format="plain",
            prog="dot",
        ).decode("utf-8")

        positions, routes = self._parse_plain(plain)

        global_rectangles: dict[str, QRectF] = {}

        for dot_id, center in positions.items():
            node = nodes_by_dot_id.get(dot_id)

            if node is None:
                continue

            global_rectangles[node.unique_id] = QRectF(
                center.x() - node.rectangle.width() / 2.0,
                center.y() - node.rectangle.height() / 2.0,
                node.rectangle.width(),
                node.rectangle.height(),
            )

        edge_indices: dict[tuple[str, str], int] = {}

        for edge in edges:
            if edge.source is None or edge.target is None:
                continue

            key = (
                dot_id_by_node_id[edge.source.unique_id],
                dot_id_by_node_id[edge.target.unique_id],
            )
            index = edge_indices.get(key, 0)
            pair_routes = routes.get(key, [])

            edge.points = (list(pair_routes[index])
                           if index < len(pair_routes) else [])
            edge_indices[key] = index + 1

        cluster_bounds: dict[str, QRectF] = {}

        def compute_cluster_bounds(cluster: LayoutCluster, ) -> QRectF:
            rectangles: list[QRectF] = []

            for node in cluster.nodes:
                rectangle = global_rectangles.get(node.unique_id)

                if rectangle is not None:
                    rectangles.append(rectangle)

            for child in cluster.clusters:
                rectangles.append(compute_cluster_bounds(child))

            for edge in cluster.edges:
                if not edge.points:
                    continue

                edge_bounds = QRectF(
                    edge.points[0],
                    edge.points[0],
                )

                for point in edge.points[1:]:
                    edge_bounds = edge_bounds.united(QRectF(point, point))

                rectangles.append(edge_bounds)

            if rectangles:
                bounds = QRectF(rectangles[0])

                for rectangle in rectangles[1:]:
                    bounds = bounds.united(rectangle)
            else:
                bounds = QRectF(0.0, 0.0, 1.0, 1.0)

            bounds = bounds.adjusted(
                -CLUSTER_PADDING,
                -CLUSTER_PADDING,
                CLUSTER_PADDING,
                CLUSTER_PADDING,
            )
            cluster_bounds[cluster.unique_id] = bounds
            return bounds

        root_bounds = compute_cluster_bounds(root)

        def assign_relative_geometry(
            cluster: LayoutCluster,
            parent_origin: QPointF,
        ) -> None:
            global_bounds = cluster_bounds[cluster.unique_id]

            cluster.rectangle = QRectF(
                global_bounds.topLeft() - parent_origin,
                global_bounds.size(),
            )

            cluster_origin = global_bounds.topLeft()

            for node in cluster.nodes:
                global_rectangle = global_rectangles.get(node.unique_id)

                if global_rectangle is None:
                    continue

                node.rectangle = QRectF(
                    global_rectangle.topLeft() - cluster_origin,
                    global_rectangle.size(),
                )

            for edge in cluster.edges:
                edge.points = [point - cluster_origin for point in edge.points]

            for child in cluster.clusters:
                assign_relative_geometry(
                    child,
                    cluster_origin,
                )

        assign_relative_geometry(
            root,
            root_bounds.topLeft(),
        )

    @staticmethod
    def _walk_nodes(cluster: LayoutCluster, ) -> Iterator[LayoutNode]:
        yield from cluster.nodes

        for child in cluster.clusters:
            yield from GraphvizLayoutExecutor._walk_nodes(child)

    @staticmethod
    def _walk_edges(cluster: LayoutCluster, ) -> Iterator[LayoutEdge]:
        yield from cluster.edges

        for child in cluster.clusters:
            yield from GraphvizLayoutExecutor._walk_edges(child)

    @staticmethod
    def _parse_plain(
        plain: str,
    ) -> tuple[
            dict[str, QPointF],
            dict[tuple[str, str], list[list[QPointF]]],
    ]:
        positions: dict[str, QPointF] = {}
        routes: dict[
            tuple[str, str],
            list[list[QPointF]],
        ] = {}

        for line in plain.splitlines():
            tokens = shlex.split(line)

            if not tokens:
                continue

            if tokens[0] == "node" and len(tokens) >= 4:
                positions[tokens[1]] = QPointF(
                    float(tokens[2]) * LAYOUT_DPI,
                    -float(tokens[3]) * LAYOUT_DPI,
                )
                continue

            if tokens[0] != "edge" or len(tokens) < 5:
                continue

            point_count = int(tokens[3])
            coordinate_tokens = tokens[4:4 + point_count * 2]

            if len(coordinate_tokens) != point_count * 2:
                raise ValueError("Graphviz returned an incomplete edge route")

            points = [
                QPointF(
                    float(coordinate_tokens[index]) * LAYOUT_DPI,
                    -float(coordinate_tokens[index + 1]) * LAYOUT_DPI,
                ) for index in range(
                    0,
                    len(coordinate_tokens),
                    2,
                )
            ]

            routes.setdefault(
                (tokens[1], tokens[2]),
                [],
            ).append(points)

        return positions, routes

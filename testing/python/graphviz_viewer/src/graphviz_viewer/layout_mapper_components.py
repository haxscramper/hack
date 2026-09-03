#!/usr/bin/env python

from __future__ import annotations

from collections.abc import Mapping, Sequence

from graphviz_viewer.graph_viewer_v2.graph import (
    GraphCluster,
    GraphElement,
)
from graphviz_viewer.graph_viewer_v2.layout_mapper import LayoutHierarchyMapper, LayoutCluster, LayoutEdge, LayoutNode
from graphviz_viewer.graph_viewer_v2.layout_mapper_direct import DirectLayoutHierarchyMapper


class SyntheticGraphElement(GraphElement):

    def __init__(
        self,
        unique_id: str,
        name: str,
        properties: Mapping[str, str],
    ) -> None:
        self._unique_id = unique_id
        self.name = name
        self._properties = dict(properties)

    @property
    def unique_id(self) -> str:
        return self._unique_id

    @property
    def properties(self) -> Mapping[str, str]:
        return self._properties


class ConnectedComponentLayoutHierarchyMapper(LayoutHierarchyMapper):

    def __init__(self) -> None:
        self.direct_mapper = DirectLayoutHierarchyMapper()

    def map(self, root: GraphCluster) -> LayoutCluster:
        layout_root = self.direct_mapper.map(root)
        nodes, edges = self._flatten(layout_root)

        components = self._connected_components(nodes, edges)
        component_by_node_id = {
            node.unique_id: component_index
            for component_index, component in enumerate(components)
            for node in component
        }

        component_edges: list[list[LayoutEdge]] = [[] for _ in components]

        for edge in edges:
            if edge.source is None or edge.target is None:
                continue

            source_component = component_by_node_id.get(edge.source.unique_id)
            target_component = component_by_node_id.get(edge.target.unique_id)

            if source_component is None or target_component is None:
                continue

            if source_component != target_component:
                raise ValueError(
                    "An edge connects nodes assigned to different "
                    "connected components")

            component_edges[source_component].append(edge)

        layout_root.nodes.clear()
        layout_root.edges.clear()
        layout_root.clusters.clear()

        small_component_nodes: list[LayoutNode] = []
        small_component_edges: list[LayoutEdge] = []
        large_components: list[tuple[list[LayoutNode], list[LayoutEdge]]] = []

        for index, component_nodes in enumerate(components):
            edges_for_component = component_edges[index]

            if len(component_nodes) <= 2:
                small_component_nodes.extend(component_nodes)
                small_component_edges.extend(edges_for_component)
            else:
                large_components.append((component_nodes, edges_for_component))

        for index, (
                component_nodes,
                edges_for_component,
        ) in enumerate(large_components, start=1):
            cluster = self._create_component_cluster(
                unique_id=f"layout:connected-component:{index}",
                name=f"Connected component {index}",
                nodes=component_nodes,
                edges=edges_for_component,
            )
            cluster.parent = layout_root
            layout_root.clusters.append(cluster)

        if small_component_nodes:
            cluster = self._create_component_cluster(
                unique_id="layout:small-connected-components",
                name="One- and two-node components",
                nodes=small_component_nodes,
                edges=small_component_edges,
            )
            cluster.parent = layout_root
            layout_root.clusters.append(cluster)

        return layout_root

    @staticmethod
    def _flatten(
        root: LayoutCluster, ) -> tuple[list[LayoutNode], list[LayoutEdge]]:
        nodes: list[LayoutNode] = []
        edges: list[LayoutEdge] = []
        seen_node_ids: set[str] = set()
        seen_edge_ids: set[str] = set()

        def visit(cluster: LayoutCluster) -> None:
            for node in cluster.nodes:
                if node.unique_id in seen_node_ids:
                    continue

                seen_node_ids.add(node.unique_id)
                nodes.append(node)

            for edge in cluster.edges:
                if edge.unique_id in seen_edge_ids:
                    continue

                seen_edge_ids.add(edge.unique_id)
                edges.append(edge)

            for child in cluster.clusters:
                visit(child)

        visit(root)

        for edge in edges:
            for endpoint in (edge.source, edge.target):
                if endpoint is None:
                    continue

                if endpoint.unique_id in seen_node_ids:
                    continue

                seen_node_ids.add(endpoint.unique_id)
                nodes.append(endpoint)

        return nodes, edges

    @staticmethod
    def _connected_components(
        nodes: Sequence[LayoutNode],
        edges: Sequence[LayoutEdge],
    ) -> list[list[LayoutNode]]:
        node_by_id = {node.unique_id: node for node in nodes}
        adjacency: dict[str, set[str]] = {
            node.unique_id: set()
            for node in nodes
        }

        for edge in edges:
            if edge.source is None or edge.target is None:
                continue

            source_id = edge.source.unique_id
            target_id = edge.target.unique_id

            if source_id not in node_by_id:
                raise ValueError(f"Edge {edge.unique_id!r} references unknown "
                                 f"source node {source_id!r}")

            if target_id not in node_by_id:
                raise ValueError(f"Edge {edge.unique_id!r} references unknown "
                                 f"target node {target_id!r}")

            adjacency[source_id].add(target_id)
            adjacency[target_id].add(source_id)

        components: list[list[LayoutNode]] = []
        visited: set[str] = set()

        for node in nodes:
            if node.unique_id in visited:
                continue

            component: list[LayoutNode] = []
            pending = [node.unique_id]
            visited.add(node.unique_id)

            while pending:
                current_id = pending.pop()
                component.append(node_by_id[current_id])

                for adjacent_id in adjacency[current_id]:
                    if adjacent_id in visited:
                        continue

                    visited.add(adjacent_id)
                    pending.append(adjacent_id)

            components.append(component)

        return components

    @staticmethod
    def _create_component_cluster(
        unique_id: str,
        name: str,
        nodes: list[LayoutNode],
        edges: list[LayoutEdge],
    ) -> LayoutCluster:
        underlying_ids = {
            element_id
            for element in [*nodes, *edges]
            for element_id in element.related_underlying_ids
        }
        synthetic_element = SyntheticGraphElement(
            unique_id=f"source:{unique_id}",
            name=name,
            properties={
                "kind": "connected-component-group",
                "node_count": str(len(nodes)),
                "edge_count": str(len(edges)),
            },
        )
        cluster = LayoutCluster(
            unique_id=unique_id,
            underlying=synthetic_element,
            related_underlying_ids=frozenset(underlying_ids),
            properties=synthetic_element.properties,
            nodes=list(nodes),
            edges=list(edges),
        )

        for node in cluster.nodes:
            node.parent = cluster

        for edge in cluster.edges:
            edge.parent = cluster

        return cluster

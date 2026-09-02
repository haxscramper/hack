#!/usr/bin/env python

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Mapping

from PyQt6.QtCore import QPointF, QRectF
from PyQt6.QtGui import QTextDocument, QTextOption

from graph_viewer_graph import (
    GraphCluster,
    GraphEdge,
    GraphElement,
    GraphNode,
)

NODE_WIDTH = 380.0
NODE_PADDING = 8.0
NODE_MINIMUM_HEIGHT = 42.0


def create_text_document(
    rich_text: str,
    width: float,
) -> QTextDocument:
    document = QTextDocument()
    document.setDocumentMargin(0.0)

    option = document.defaultTextOption()
    option.setWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)
    document.setDefaultTextOption(option)

    document.setHtml(rich_text)
    document.setTextWidth(width)
    document.adjustSize()
    document.setTextWidth(width)
    return document


@dataclass
class LayoutElement:
    unique_id: str
    underlying: GraphElement
    related_underlying_ids: frozenset[str]
    properties: Mapping[str, str]
    parent: LayoutCluster | None = field(default=None, repr=False)


@dataclass
class LayoutNode(LayoutElement):
    rich_text: str = ""
    rectangle: QRectF = field(default_factory=QRectF)


@dataclass
class LayoutEdge(LayoutElement):
    source: LayoutNode | None = None
    target: LayoutNode | None = None
    rich_text: str = ""
    points: list[QPointF] = field(default_factory=list)


@dataclass
class LayoutCluster(LayoutElement):
    nodes: list[LayoutNode] = field(default_factory=list)
    edges: list[LayoutEdge] = field(default_factory=list)
    clusters: list[LayoutCluster] = field(default_factory=list)
    rectangle: QRectF = field(default_factory=QRectF)


class LayoutHierarchyMapper(ABC):

    @abstractmethod
    def map(self, root: GraphCluster) -> LayoutCluster:
        raise NotImplementedError


class DirectLayoutHierarchyMapper(LayoutHierarchyMapper):

    def map(self, root: GraphCluster) -> LayoutCluster:
        nodes_by_underlying_id: dict[str, LayoutNode] = {}

        def make_node(source: GraphNode) -> LayoutNode:
            document = create_text_document(
                source.rich_text,
                NODE_WIDTH - NODE_PADDING * 2.0,
            )
            height = max(
                NODE_MINIMUM_HEIGHT,
                document.size().height() + NODE_PADDING * 2.0,
            )

            node = LayoutNode(
                unique_id=f"layout:node:{source.unique_id}",
                underlying=source,
                related_underlying_ids=frozenset({source.unique_id}),
                properties=source.properties,
                rich_text=source.rich_text,
                rectangle=QRectF(0.0, 0.0, NODE_WIDTH, height),
            )
            nodes_by_underlying_id[source.unique_id] = node
            return node

        def convert(source: GraphCluster) -> LayoutCluster:
            result = LayoutCluster(
                unique_id=f"layout:cluster:{source.unique_id}",
                underlying=source,
                related_underlying_ids=frozenset({source.unique_id}),
                properties=source.properties,
            )

            for source_node in source.nodes:
                node = nodes_by_underlying_id.get(source_node.unique_id)

                if node is None:
                    node = make_node(source_node)

                node.parent = result
                result.nodes.append(node)

            for source_cluster in source.clusters:
                child = convert(source_cluster)
                child.parent = result
                result.clusters.append(child)

            for source_edge in source.edges:
                source_node = nodes_by_underlying_id.get(
                    source_edge.source.unique_id)
                target_node = nodes_by_underlying_id.get(
                    source_edge.target.unique_id)

                if source_node is None:
                    source_node = make_node(source_edge.source)
                    source_node.parent = result
                    result.nodes.append(source_node)

                if target_node is None:
                    target_node = make_node(source_edge.target)
                    target_node.parent = result
                    result.nodes.append(target_node)

                edge = LayoutEdge(
                    unique_id=f"layout:edge:{source_edge.unique_id}",
                    underlying=source_edge,
                    related_underlying_ids=frozenset({source_edge.unique_id}),
                    properties=source_edge.properties,
                    source=source_node,
                    target=target_node,
                    rich_text=source_edge.rich_text,
                )
                edge.parent = result
                result.edges.append(edge)

            return result

        return convert(root)


class EdgeLabelLayoutHierarchyMapper(LayoutHierarchyMapper):

    def __init__(self) -> None:
        self.direct_mapper = DirectLayoutHierarchyMapper()

    def map(self, root: GraphCluster) -> LayoutCluster:
        layout_root = self.direct_mapper.map(root)

        def split_labels(cluster: LayoutCluster) -> None:
            replacement_edges: list[LayoutEdge] = []

            for edge in cluster.edges:
                if not edge.rich_text.strip():
                    replacement_edges.append(edge)
                    continue

                document = create_text_document(
                    edge.rich_text,
                    NODE_WIDTH - NODE_PADDING * 2.0,
                )
                height = max(
                    NODE_MINIMUM_HEIGHT,
                    document.size().height() + NODE_PADDING * 2.0,
                )
                related = frozenset({edge.underlying.unique_id})

                label_node = LayoutNode(
                    unique_id=f"{edge.unique_id}:label",
                    underlying=edge.underlying,
                    related_underlying_ids=related,
                    properties=edge.properties,
                    rich_text=edge.rich_text,
                    rectangle=QRectF(
                        0.0,
                        0.0,
                        NODE_WIDTH,
                        height,
                    ),
                    parent=cluster,
                )
                cluster.nodes.append(label_node)

                tail = LayoutEdge(
                    unique_id=f"{edge.unique_id}:tail",
                    underlying=edge.underlying,
                    related_underlying_ids=related,
                    properties=edge.properties,
                    source=edge.source,
                    target=label_node,
                    parent=cluster,
                )
                head = LayoutEdge(
                    unique_id=f"{edge.unique_id}:head",
                    underlying=edge.underlying,
                    related_underlying_ids=related,
                    properties=edge.properties,
                    source=label_node,
                    target=edge.target,
                    parent=cluster,
                )
                replacement_edges.extend((tail, head))

            cluster.edges = replacement_edges

            for child in cluster.clusters:
                split_labels(child)

        split_labels(layout_root)
        return layout_root

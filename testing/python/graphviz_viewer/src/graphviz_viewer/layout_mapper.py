#!/usr/bin/env python

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Mapping

from PyQt6.QtCore import QPointF, QRectF

from graphviz_viewer.graph_viewer_v2.graph import GraphCluster, GraphElement, GraphNode


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

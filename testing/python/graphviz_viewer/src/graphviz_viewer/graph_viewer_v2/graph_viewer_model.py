#!/usr/bin/env python

from __future__ import annotations

from dataclasses import dataclass, field
from enum import IntEnum
from typing import Any

from PyQt6.QtCore import (
    QAbstractItemModel,
    QModelIndex,
    Qt,
)

from graphviz_viewer.graph_viewer_v2.graph_viewer_layout import LayoutCluster, LayoutEdge, LayoutElement, LayoutNode


class GraphRole(IntEnum):
    Element = Qt.ItemDataRole.UserRole + 1
    UnderlyingElement = Qt.ItemDataRole.UserRole + 2
    ElementKind = Qt.ItemDataRole.UserRole + 3
    Geometry = Qt.ItemDataRole.UserRole + 4
    RichText = Qt.ItemDataRole.UserRole + 5
    Properties = Qt.ItemDataRole.UserRole + 6
    RelatedUnderlyingIds = Qt.ItemDataRole.UserRole + 7
    EdgePoints = Qt.ItemDataRole.UserRole + 8
    EdgeSource = Qt.ItemDataRole.UserRole + 9
    EdgeTarget = Qt.ItemDataRole.UserRole + 10
    Style = Qt.ItemDataRole.UserRole + 11


@dataclass
class ModelTreeItem:
    element: LayoutElement
    parent_item: ModelTreeItem | None = field(
        default=None,
        repr=False,
    )
    children: list[ModelTreeItem] = field(
        default_factory=list,
        repr=False,
    )

    def row(self) -> int:
        if self.parent_item is None:
            return 0

        return self.parent_item.children.index(self)


class GraphLayoutModel(QAbstractItemModel):

    def __init__(self, root: LayoutCluster) -> None:
        super().__init__()

        self.root = root
        self.root_item = self._build_tree_item(root, None)

    def _build_tree_item(
        self,
        element: LayoutElement,
        parent_item: ModelTreeItem | None,
    ) -> ModelTreeItem:
        item = ModelTreeItem(
            element=element,
            parent_item=parent_item,
        )

        if isinstance(element, LayoutCluster):
            child_elements: list[LayoutElement] = [
                *element.clusters,
                *element.nodes,
                *element.edges,
            ]

            item.children = [
                self._build_tree_item(child, item) for child in child_elements
            ]

        return item

    def rebuild(self) -> None:
        self.beginResetModel()
        self.root_item = self._build_tree_item(
            self.root,
            None,
        )
        self.endResetModel()

    def index(
            self,
            row: int,
            column: int,
            parent: QModelIndex = QModelIndex(),
    ) -> QModelIndex:
        if row < 0 or column != 0:
            return QModelIndex()

        if not parent.isValid():
            if row != 0:
                return QModelIndex()

            return self.createIndex(
                0,
                0,
                self.root_item,
            )

        parent_item = parent.internalPointer()

        if not isinstance(parent_item, ModelTreeItem):
            return QModelIndex()

        if row >= len(parent_item.children):
            return QModelIndex()

        return self.createIndex(
            row,
            0,
            parent_item.children[row],
        )

    def parent(
        self,
        child: QModelIndex,
    ) -> QModelIndex:
        if not child.isValid():
            return QModelIndex()

        child_item = child.internalPointer()

        if not isinstance(child_item, ModelTreeItem):
            return QModelIndex()

        parent_item = child_item.parent_item

        if parent_item is None:
            return QModelIndex()

        return self.createIndex(
            parent_item.row(),
            0,
            parent_item,
        )

    def rowCount(
            self,
            parent: QModelIndex = QModelIndex(),
    ) -> int:
        if parent.isValid() and parent.column() != 0:
            return 0

        if not parent.isValid():
            return 1

        parent_item = parent.internalPointer()

        if not isinstance(parent_item, ModelTreeItem):
            return 0

        return len(parent_item.children)

    def columnCount(
            self,
            parent: QModelIndex = QModelIndex(),
    ) -> int:
        return 1

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        if not index.isValid():
            return None

        item = index.internalPointer()

        if not isinstance(item, ModelTreeItem):
            return None

        element = item.element

        if role == Qt.ItemDataRole.DisplayRole:
            return self._display_name(element)

        if role == GraphRole.Element:
            return element

        if role == GraphRole.UnderlyingElement:
            return element.underlying

        if role == GraphRole.ElementKind:
            return self._element_kind(element)

        if role == GraphRole.Geometry:
            if isinstance(element, (LayoutCluster, LayoutNode)):
                return element.rectangle

            return None

        if role == GraphRole.RichText:
            if isinstance(element, (LayoutNode, LayoutEdge)):
                return element.rich_text

            return ""

        if role == GraphRole.Properties:
            return dict(element.properties)

        if role == GraphRole.RelatedUnderlyingIds:
            return tuple(sorted(element.related_underlying_ids))

        if role == GraphRole.EdgePoints:
            if isinstance(element, LayoutEdge):
                return list(element.points)

            return []

        if role == GraphRole.EdgeSource:
            if isinstance(element, LayoutEdge):
                if element.source is not None:
                    return element.source.unique_id

            return None

        if role == GraphRole.EdgeTarget:
            if isinstance(element, LayoutEdge):
                if element.target is not None:
                    return element.target.unique_id

            return None

        if role == GraphRole.Style:
            return self._style(element)

        return None

    def flags(
        self,
        index: QModelIndex,
    ) -> Qt.ItemFlag:
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags

        return (Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable)

    def roleNames(self) -> dict[int, bytes]:
        return {
            int(GraphRole.Element): b"element",
            int(GraphRole.UnderlyingElement): b"underlyingElement",
            int(GraphRole.ElementKind): b"elementKind",
            int(GraphRole.Geometry): b"geometry",
            int(GraphRole.RichText): b"richText",
            int(GraphRole.Properties): b"properties",
            int(GraphRole.RelatedUnderlyingIds): b"relatedUnderlyingIds",
            int(GraphRole.EdgePoints): b"edgePoints",
            int(GraphRole.EdgeSource): b"edgeSource",
            int(GraphRole.EdgeTarget): b"edgeTarget",
            int(GraphRole.Style): b"style",
        }

    @staticmethod
    def _element_kind(element: LayoutElement, ) -> str:
        if isinstance(element, LayoutCluster):
            return "cluster"

        if isinstance(element, LayoutNode):
            return "node"

        if isinstance(element, LayoutEdge):
            return "edge"

        raise TypeError(
            f"Unsupported layout element: {type(element).__name__}")

    @staticmethod
    def _display_name(element: LayoutElement, ) -> str:
        underlying_name = getattr(
            element.underlying,
            "name",
            "",
        )

        if underlying_name:
            return str(underlying_name)

        if isinstance(element, LayoutEdge):
            source_name = (getattr(element.source.underlying, "name", "")
                           if element.source is not None else "?")
            target_name = (getattr(element.target.underlying, "name", "")
                           if element.target is not None else "?")
            return f"{source_name} → {target_name}"

        return element.unique_id

    @staticmethod
    def _style(element: LayoutElement, ) -> dict[str, str]:
        if isinstance(element, LayoutCluster):
            return {
                "background": "#eef1f5",
                "border": "#9aa0a6",
            }

        if isinstance(element, LayoutNode):
            return {
                "background": "#ffffff",
                "border": "#606770",
                "selected-border": "#1976d2",
            }

        if isinstance(element, LayoutEdge):
            return {
                "color": "#68707a",
            }

        raise TypeError(
            f"Unsupported layout element: {type(element).__name__}")

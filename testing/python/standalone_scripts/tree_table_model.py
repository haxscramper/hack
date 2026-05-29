#!/usr/bin/env python

import math
import os
import sys
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

from PySide6.QtCore import QAbstractItemModel, QModelIndex, Qt
from PySide6.QtWidgets import QApplication, QHeaderView, QTreeView


@dataclass
class FileNode:
    name: str
    path: Path
    is_dir: bool
    size: int = 0
    created_ts: float = 0.0
    parent: "FileNode | None" = None
    children: list["FileNode"] = field(default_factory=list)
    proportion: float = 0.0

    def row(self) -> int:
        if self.parent is None:
            return 0
        return self.parent.children.index(self)


def format_size(size: int) -> str:
    if size == 0:
        return "0 B"
    units = ["B", "KiB", "MiB", "GiB", "TiB"]
    value = float(size)
    for unit in units:
        if value < 1024.0 or unit == units[-1]:
            if unit == "B":
                return f"{int(value)} {unit}"
            return f"{value:.2f} {unit}"
        value /= 1024.0
    return f"{size} B"


def format_datetime(timestamp: float) -> str:
    if timestamp <= 0:
        return ""
    return datetime.fromtimestamp(timestamp).strftime("%Y-%m-%d %H:%M:%S")


def build_tree(path: Path, parent: FileNode | None = None) -> FileNode:
    stat = path.stat()
    node = FileNode(
        name=path.name if parent is not None else str(path),
        path=path,
        is_dir=path.is_dir(),
        created_ts=stat.st_ctime,
        parent=parent,
    )

    if node.is_dir:
        children = []
        total_size = 0
        latest_ctime = node.created_ts

        entries = sorted(path.iterdir(),
                         key=lambda p: (not p.is_dir(), p.name.lower()))
        for entry in entries:
            child = build_tree(entry, node)
            children.append(child)
            total_size += child.size
            latest_ctime = max(latest_ctime, child.created_ts)

        node.children = children
        node.size = total_size
        node.created_ts = latest_ctime
    else:
        node.size = stat.st_size

    return node


def assign_proportions(node: FileNode, total_size: int) -> None:
    node.proportion = (node.size / total_size) if total_size > 0 else 0.0
    for child in node.children:
        assign_proportions(child, total_size)


class FileTreeModel(QAbstractItemModel):
    HEADERS = ["Name", "Size", "Created", "Share"]

    def __init__(self, root: FileNode) -> None:
        super().__init__()
        self._root = root

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return len(self.HEADERS)

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        node = self._node_from_index(parent)
        return len(node.children)

    def index(self, row: int, column: int,
              parent: QModelIndex = QModelIndex()) -> QModelIndex:
        parent_node = self._node_from_index(parent)
        if row < 0 or row >= len(parent_node.children):
            return QModelIndex()
        child = parent_node.children[row]
        return self.createIndex(row, column, child)

    def parent(self, index: QModelIndex) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()

        node: FileNode = index.internalPointer()
        parent = node.parent

        if parent is None or parent == self._root:
            return QModelIndex()

        return self.createIndex(parent.row(), 0, parent)

    def data(self, index: QModelIndex, role: int = Qt.DisplayRole):
        if not index.isValid():
            return None

        node: FileNode = index.internalPointer()
        column = index.column()

        if role == Qt.DisplayRole:
            if column == 0:
                return node.name
            if column == 1:
                return format_size(node.size)
            if column == 2:
                return format_datetime(node.created_ts)
            if column == 3:
                return f"{node.proportion * 100:.2f}%"

        if role == Qt.TextAlignmentRole and column in (1, 3):
            return int(Qt.AlignRight | Qt.AlignVCenter)

        if role == Qt.UserRole:
            if column == 1:
                return node.size
            if column == 2:
                return node.created_ts
            if column == 3:
                return node.proportion

        return None

    def headerData(self,
                   section: int,
                   orientation: Qt.Orientation,
                   role: int = Qt.DisplayRole):
        if orientation == Qt.Horizontal and role == Qt.DisplayRole:
            return self.HEADERS[section]
        return None

    def _node_from_index(self, index: QModelIndex) -> FileNode:
        if index.isValid():
            return index.internalPointer()
        return self._root


def main() -> int:
    root_path = Path(sys.argv[1]).resolve() if len(
        sys.argv) > 1 else Path.cwd().resolve()
    root = build_tree(root_path)
    assign_proportions(root, root.size)

    app = QApplication(sys.argv)

    view = QTreeView()
    model = FileTreeModel(root)
    view.setModel(model)
    view.setWindowTitle(f"Directory tree: {root_path}")
    view.setRootIsDecorated(True)
    view.setAlternatingRowColors(True)
    view.setUniformRowHeights(True)
    view.setSortingEnabled(False)

    header = view.header()
    header.setSectionResizeMode(0, QHeaderView.Stretch)
    header.setSectionResizeMode(1, QHeaderView.ResizeToContents)
    header.setSectionResizeMode(2, QHeaderView.ResizeToContents)
    header.setSectionResizeMode(3, QHeaderView.ResizeToContents)

    view.resize(1100, 700)
    view.expandToDepth(0)
    view.show()

    return app.exec()


if __name__ == "__main__":
    raise SystemExit(main())

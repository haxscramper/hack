from __future__ import annotations

from dataclasses import dataclass, field
from enum import IntEnum

from beartype.typing import Any, Optional
from PyQt6.QtCore import (
    QAbstractItemModel,
    QAbstractTableModel,
    QModelIndex,
    QObject,
    Qt,
)
from PyQt6.QtGui import QTextDocument

from graphviz_viewer.task_graph_types import (
    ElementKind,
    Group,
    InputCollection,
    LayoutEdge,
    NodeRect,
    Point,
    Rect,
)


class TaskGraphRole(IntEnum):
    ELEMENT_KIND = int(Qt.ItemDataRole.UserRole) + 1
    RELATIVE_GEOMETRY = int(Qt.ItemDataRole.UserRole) + 2
    ABSOLUTE_GEOMETRY = int(Qt.ItemDataRole.UserRole) + 3
    RICH_TEXT = int(Qt.ItemDataRole.UserRole) + 4
    SOURCE_ID = int(Qt.ItemDataRole.UserRole) + 5
    EDGE_POINTS = int(Qt.ItemDataRole.UserRole) + 6
    COLOR = int(Qt.ItemDataRole.UserRole) + 7
    UNIQUE_ID = int(Qt.ItemDataRole.UserRole) + 8


@dataclass
class ModelEntry:
    kind: ElementKind
    value: Group | NodeRect | LayoutEdge
    parent: Optional[ModelEntry] = None
    nested: list[ModelEntry] = field(default_factory=list)


def entry_geometry(entry: ModelEntry) -> Rect:
    match entry.value:
        case Group(geometry=geometry):
            if geometry is None:
                raise RuntimeError(
                    f"Group {entry.value.unique_id} has no geometry")
            return geometry
        case NodeRect(geometry=geometry):
            if geometry is None:
                raise RuntimeError(
                    f"Node {entry.value.unique_id} has no geometry")
            return geometry
        case LayoutEdge():
            return Rect(0.0, 0.0, 0.0, 0.0)


def absolute_offset(entry: ModelEntry) -> Point:
    x = 0.0
    y = 0.0
    current = entry.parent

    while current is not None:
        if current.kind == ElementKind.GROUP:
            geometry = entry_geometry(current)
            x += geometry.x
            y += geometry.y
        current = current.parent

    return Point(x=x, y=y)


def absolute_geometry(entry: ModelEntry) -> Rect:
    geometry = entry_geometry(entry)
    offset = absolute_offset(entry)
    return geometry.translated(offset)


def build_entry(group: Group,
                parent: Optional[ModelEntry] = None) -> ModelEntry:
    entry = ModelEntry(
        kind=ElementKind.GROUP,
        value=group,
        parent=parent,
    )

    for nested_group in group.nested_groups:
        entry.nested.append(build_entry(nested_group, entry))

    for node in group.nodes:
        entry.nested.append(
            ModelEntry(
                kind=ElementKind.NODE,
                value=node,
                parent=entry,
            ))

    for edge in group.edges:
        entry.nested.append(
            ModelEntry(
                kind=ElementKind.EDGE,
                value=edge,
                parent=entry,
            ))

    return entry


class TaskGraphModel(QAbstractItemModel):

    def __init__(
        self,
        root_group: Group,
        collection: InputCollection,
        parent: Optional[QObject] = None,
    ) -> None:
        super().__init__(parent)
        self.root_entry = build_entry(root_group)
        self.nodes = {node.unique_id: node for node in collection.nodes}

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return 1

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        entry = self.entry_from_index(parent)
        return len(entry.nested)

    def index(
            self,
            row: int,
            column: int,
            parent: QModelIndex = QModelIndex(),
    ) -> QModelIndex:
        parent_entry = self.entry_from_index(parent)
        if row < 0 or len(parent_entry.nested) <= row:
            return QModelIndex()
        return self.createIndex(row, column, parent_entry.nested[row])

    def parent(self, index: QModelIndex) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()

        entry = index.internalPointer()
        if not isinstance(entry, ModelEntry):
            return QModelIndex()

        parent_entry = entry.parent
        if parent_entry is None or parent_entry is self.root_entry:
            return QModelIndex()

        grandparent = parent_entry.parent
        if grandparent is None:
            return QModelIndex()

        row = grandparent.nested.index(parent_entry)
        return self.createIndex(row, 0, parent_entry)

    def data(
            self,
            index: QModelIndex,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if not index.isValid():
            return None

        entry = index.internalPointer()
        if not isinstance(entry, ModelEntry):
            return None

        if role == int(Qt.ItemDataRole.DisplayRole):
            return self.display_name(entry)
        if role == int(TaskGraphRole.ELEMENT_KIND):
            return entry.kind
        if role == int(TaskGraphRole.RELATIVE_GEOMETRY):
            return entry_geometry(entry)
        if role == int(TaskGraphRole.ABSOLUTE_GEOMETRY):
            return absolute_geometry(entry)
        if role == int(TaskGraphRole.RICH_TEXT):
            return self.rich_text(entry)
        if role == int(TaskGraphRole.SOURCE_ID):
            return self.source_id(entry)
        if role == int(TaskGraphRole.EDGE_POINTS):
            return self.absolute_edge_points(entry)
        if role == int(TaskGraphRole.COLOR):
            return self.color(entry)
        if role == int(TaskGraphRole.UNIQUE_ID):
            return self.unique_id(entry)
        return None

    def entry_from_index(self, index: QModelIndex) -> ModelEntry:
        if not index.isValid():
            return self.root_entry

        entry = index.internalPointer()
        if not isinstance(entry, ModelEntry):
            raise TypeError(
                "Model index does not reference a task graph entry")
        return entry

    def display_name(self, entry: ModelEntry) -> str:
        match entry.value:
            case Group(label=label):
                return label
            case NodeRect(source_id=source_id):
                document = QTextDocument()
                document.setHtml(self.nodes[source_id].rich_text)
                return document.toPlainText()
            case LayoutEdge(unique_id=unique_id):
                return unique_id

    def rich_text(self, entry: ModelEntry) -> str:
        match entry.value:
            case NodeRect(source_id=source_id):
                return self.nodes[source_id].rich_text
            case Group(label=label):
                return label
            case LayoutEdge():
                return ""

    def source_id(self, entry: ModelEntry) -> str:
        match entry.value:
            case NodeRect(source_id=source_id):
                return source_id
            case Group(unique_id=unique_id):
                return unique_id
            case LayoutEdge(unique_id=unique_id):
                return unique_id

    def unique_id(self, entry: ModelEntry) -> str:
        return self.source_id(entry)

    def absolute_edge_points(self, entry: ModelEntry) -> list[Point]:
        if not isinstance(entry.value, LayoutEdge):
            return []

        offset = absolute_offset(entry)
        return [
            Point(point.x + offset.x, point.y + offset.y)
            for point in entry.value.points
        ]

    def color(self, entry: ModelEntry) -> str:
        match entry.value:
            case Group():
                return "#8b8f97"
            case LayoutEdge(kind=kind):
                return "#c778dd" if kind.name == "RELATED" else "#5f9eea"
            case NodeRect(source_id=source_id):
                node = self.nodes[source_id]
                if node.todo_state is None:
                    return "#73808f"
                return {
                    "TODO": "#d19a66",
                    "NEXT": "#61afef",
                    "WIP": "#98c379",
                    "DONE": "#7f848e",
                }[node.todo_state.name]


class PropertyModel(QAbstractTableModel):

    def __init__(
        self,
        source_model: TaskGraphModel,
        parent: Optional[QObject] = None,
    ) -> None:
        super().__init__(parent)
        self.source_model = source_model
        self.selected = QModelIndex()
        self.rows: list[tuple[str, str]] = []

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return len(self.rows)

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return 2

    def data(
            self,
            index: QModelIndex,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if not index.isValid() or role != int(Qt.ItemDataRole.DisplayRole):
            return None
        return self.rows[index.row()][index.column()]

    def headerData(
            self,
            section: int,
            orientation: Qt.Orientation,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if role != int(Qt.ItemDataRole.DisplayRole):
            return None
        if orientation == Qt.Orientation.Horizontal:
            return ["Property", "Value"][section]
        return str(section + 1)

    def set_selected(self, index: QModelIndex) -> None:
        self.beginResetModel()
        self.selected = index
        self.rows = self.make_rows(index)
        self.endResetModel()

    def make_rows(self, index: QModelIndex) -> list[tuple[str, str]]:
        if not index.isValid():
            return []

        kind = index.data(int(TaskGraphRole.ELEMENT_KIND))
        relative = index.data(int(TaskGraphRole.RELATIVE_GEOMETRY))
        absolute = index.data(int(TaskGraphRole.ABSOLUTE_GEOMETRY))
        rich_text = index.data(int(TaskGraphRole.RICH_TEXT))
        document = QTextDocument()
        document.setHtml(rich_text)

        rows = [
            ("ID", str(index.data(int(TaskGraphRole.UNIQUE_ID)))),
            ("Type", kind.name),
            ("Content", document.toPlainText()),
        ]

        if isinstance(relative, Rect):
            rows.extend([
                ("Relative X", f"{relative.x:.1f}"),
                ("Relative Y", f"{relative.y:.1f}"),
                ("Width", f"{relative.width:.1f}"),
                ("Height", f"{relative.height:.1f}"),
            ])

        if isinstance(absolute, Rect):
            rows.extend([
                ("Absolute X", f"{absolute.x:.1f}"),
                ("Absolute Y", f"{absolute.y:.1f}"),
            ])

        points = index.data(int(TaskGraphRole.EDGE_POINTS))
        if points:
            rows.append((
                "Spline",
                " ".join(f"({point.x:.1f}, {point.y:.1f})"
                         for point in points),
            ))

        return rows

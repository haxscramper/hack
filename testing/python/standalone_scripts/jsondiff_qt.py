#!/usr/bin/env python
from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass, field
from enum import Enum, auto
from itertools import groupby
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, Iterator
from deepdiff import DeepDiff
from PyQt6.QtCore import (
    QAbstractItemModel,
    QModelIndex,
    QObject,
    QSize,
    Qt,
)
from PyQt6.QtGui import (
    QBrush,
    QColor,
    QFont,
    QFontDatabase,
    QKeySequence,
    QPainter,
    QShortcut,
)
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QPushButton,
    QStyledItemDelegate,
    QStyleOptionViewItem,
    QTreeView,
    QVBoxLayout,
    QWidget,
)


class Missing(Enum):
    VALUE = auto()


class Status(Enum):
    SAME = auto()
    CHANGED = auto()
    ADDED = auto()
    REMOVED = auto()


@beartype
@dataclass(eq=False, slots=True)
class Entry:
    key: str | int
    value: Any


@beartype
@dataclass(eq=False, slots=True)
class Row:
    path: tuple[str | int, ...]
    old: Any
    new: Any
    status: Status
    parent: Row | None = field(default=None, repr=False)
    position: int = 0
    nested: list[Row] = field(default_factory=list, repr=False)
    group: bool = False

    @property
    def depth(self) -> int:
        result = 0
        current = self.parent
        while current is not None:
            result += 1
            current = current.parent
        return result


@beartype
def nested_entries(value: Any) -> list[Entry]:
    match value:
        case dict():
            return [Entry(key, item) for key, item in value.items()]
        case list():
            return [Entry(index, item) for index, item in enumerate(value)]
        case _:
            return []


@beartype
def walk_rows(row: Row) -> Iterator[Row]:
    yield row
    for nested in row.nested:
        yield from walk_rows(nested)


@beartype
def build_row(
    old: Any,
    new: Any,
    path: tuple[str | int, ...],
    affected: set[tuple[str | int, ...]],
    direct: set[tuple[str | int, ...]],
    parent: Row | None = None,
    position: int = 0,
    inherited_change: bool = False,
) -> Row:
    if old is Missing.VALUE:
        status = Status.ADDED
    elif new is Missing.VALUE:
        status = Status.REMOVED
    elif inherited_change or path in affected:
        status = Status.CHANGED
    else:
        status = Status.SAME

    row = Row(
        path=path,
        old=old,
        new=new,
        status=status,
        parent=parent,
        position=position,
    )

    old_entries = {entry.key: entry.value for entry in nested_entries(old)}
    new_entries = {entry.key: entry.value for entry in nested_entries(new)}
    keys = list(old_entries)
    keys.extend(key for key in new_entries if key not in old_entries)

    for index, key in enumerate(keys):
        row.nested.append(
            build_row(
                old=old_entries.get(key, Missing.VALUE),
                new=new_entries.get(key, Missing.VALUE),
                path=(*path, key),
                affected=affected,
                direct=direct,
                parent=row,
                position=index,
                inherited_change=inherited_change or path in direct,
            )
        )

    return row


@beartype
def fold_equal_ranges(row: Row) -> None:
    for nested in row.nested:
        fold_equal_ranges(nested)

    if row.status is Status.SAME:
        return

    result: list[Row] = []
    for identical, sequence in groupby(
        row.nested,
        key=lambda item: item.status is Status.SAME,
    ):
        run = list(sequence)
        if identical and 1 < len(run):
            group = Row(
                path=row.path,
                old=Missing.VALUE,
                new=Missing.VALUE,
                status=Status.SAME,
                parent=row,
                nested=run,
                group=True,
            )
            for position, nested in enumerate(run):
                nested.parent = group
                nested.position = position
            result.append(group)
        else:
            result.extend(run)

    row.nested = result
    for position, nested in enumerate(result):
        nested.parent = row
        nested.position = position


@beartype
def make_diff(old: Any, new: Any) -> Row:
    differences = DeepDiff(
        old,
        new,
        view="tree",
        ignore_order=False,
        zip_ordered_iterables=True,
        threshold_to_diff_deeper=0,
        verbose_level=2,
    )

    direct: set[tuple[str | int, ...]] = set()
    for changes in differences.values():
        for change in changes:
            direct.add(tuple(change.path(output_format="list")))

    affected: set[tuple[str | int, ...]] = set()
    for path in direct:
        for length in range(len(path) + 1):
            affected.add(path[:length])

    root = build_row(old, new, (), affected, direct)
    fold_equal_ranges(root)
    return root


@beartype
def value_text(value: Any) -> str:
    match value:
        case dict():
            return f"{{{len(value)} fields}}"
        case list():
            return f"[{len(value)} items]"
        case _:
            return json.dumps(value, ensure_ascii=False)


@beartype
def row_text(row: Row, column: int) -> str:
    if row.group:
        return f"… {len(row.nested)} unchanged entries"

    value = row.old if column == 0 else row.new
    if value is Missing.VALUE:
        return "—"

    if not row.path:
        name = "$"
    else:
        key = row.path[-1]
        match key:
            case str():
                name = json.dumps(key, ensure_ascii=False)
            case int():
                name = f"[{key}]"

    return f"{name}: {value_text(value)}"


@beartype
class DiffModel(QAbstractItemModel):
    def __init__(
        self,
        root: Row,
        old_name: str,
        new_name: str,
        parent: QObject | None = None,
    ) -> None:
        super().__init__(parent)
        self.root = root
        self.nodes = list(walk_rows(root))
        self.headers = (old_name, new_name)
        self.matches: set[Row] = set()
        self.search_text = {
            row: self.searchable_text(row) for row in self.nodes if not row.group
        }

    def searchable_text(self, row: Row) -> str:
        parts = [
            json.dumps(list(row.path), ensure_ascii=False),
            row_text(row, 0),
            row_text(row, 1),
        ]
        for value in (row.old, row.new):
            if isinstance(value, str):
                parts.append(value)
        return "\n".join(parts).casefold()

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return 2

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if not parent.isValid():
            return 1
        if parent.column() != 0:
            return 0
        row: Row = parent.internalPointer()
        return len(row.nested)

    def index(
        self,
        row: int,
        column: int,
        parent: QModelIndex = QModelIndex(),
    ) -> QModelIndex:
        if not self.hasIndex(row, column, parent):
            return QModelIndex()

        if not parent.isValid():
            return self.createIndex(row, column, self.root)

        container: Row = parent.internalPointer()
        return self.createIndex(row, column, container.nested[row])

    def parent(self, index: QModelIndex = QModelIndex()) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()

        row: Row = index.internalPointer()
        if row.parent is None:
            return QModelIndex()
        return self.index_for(row.parent)

    def index_for(self, row: Row, column: int = 0) -> QModelIndex:
        return self.createIndex(row.position, column, row)

    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole.value,
    ) -> Any:
        if (
            orientation == Qt.Orientation.Horizontal
            and role == Qt.ItemDataRole.DisplayRole.value
            and 0 <= section < len(self.headers)
        ):
            return self.headers[section]
        return None

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags
        return Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole.value,
    ) -> Any:
        if not index.isValid():
            return None

        row: Row = index.internalPointer()

        match role:
            case Qt.ItemDataRole.DisplayRole.value:
                return row_text(row, index.column())

            case Qt.ItemDataRole.ToolTipRole.value:
                if row.group:
                    return f"{len(row.nested)} unchanged entries; expand to inspect."

                value = row.old if index.column() == 0 else row.new
                rendered = (
                    "Not present"
                    if value is Missing.VALUE
                    else json.dumps(value, ensure_ascii=False, indent=2)
                )
                path = json.dumps(list(row.path), ensure_ascii=False)
                return f"Path: {path}\nStatus: {row.status.name.lower()}\n\n{rendered}"

            case Qt.ItemDataRole.BackgroundRole.value:
                if row in self.matches:
                    return QBrush(QColor(235, 185, 35, 100))

                match row.status:
                    case Status.CHANGED:
                        return QBrush(QColor(210, 155, 45, 45))
                    case Status.ADDED:
                        if index.column() == 1:
                            return QBrush(QColor(50, 175, 95, 55))
                    case Status.REMOVED:
                        if index.column() == 0:
                            return QBrush(QColor(220, 75, 75, 55))

            case Qt.ItemDataRole.FontRole.value:
                if row.group:
                    font = QFont()
                    font.setItalic(True)
                    return font

        return None

    def set_search(self, text: str) -> list[Row]:
        query = text.casefold()
        results = (
            [
                row
                for row in self.nodes
                if not row.group and query in self.search_text[row]
            ]
            if query
            else []
        )

        previous = self.matches
        self.matches = set(results)

        for row in previous | self.matches:
            self.dataChanged.emit(
                self.index_for(row, 0),
                self.index_for(row, 1),
                [Qt.ItemDataRole.BackgroundRole.value],
            )

        return results


@beartype
class DiffDelegate(QStyledItemDelegate):
    def __init__(self, view: QTreeView) -> None:
        super().__init__(view)
        self.view = view

    def indentation(self, index: QModelIndex) -> int:
        row: Row = index.internalPointer()
        return (row.depth + 1) * self.view.indentation()

    def paint(
        self,
        painter: QPainter,
        option: QStyleOptionViewItem,
        index: QModelIndex,
    ) -> None:
        adjusted = QStyleOptionViewItem(option)

        if index.column() == 1:
            inset = min(
                self.indentation(index),
                max(0, adjusted.rect.width() - 1),
            )
            adjusted.rect = adjusted.rect.adjusted(inset, 0, 0, 0)

        super().paint(painter, adjusted, index)

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex,
    ) -> QSize:
        size = super().sizeHint(option, index)
        if index.column() == 1:
            size.setWidth(size.width() + self.indentation(index))
        return size


@beartype
class DiffWindow(QWidget):
    def __init__(self, root: Row, old_path: Path, new_path: Path) -> None:
        super().__init__()
        self.setWindowTitle(f"JSON diff — {old_path.name} → {new_path.name}")
        self.resize(1400, 900)

        self.model = DiffModel(root, str(old_path), str(new_path), self)
        self.results: list[Row] = []
        self.result_position = -1

        self.search = QLineEdit(self)
        self.search.setPlaceholderText("Search keys, values, or paths — Ctrl+F")
        self.search.setClearButtonEnabled(True)

        self.previous_button = QPushButton("Previous", self)
        self.next_button = QPushButton("Next", self)
        self.counter = QLabel("0 matches", self)

        search_layout = QHBoxLayout()
        search_layout.addWidget(self.search, 1)
        search_layout.addWidget(self.previous_button)
        search_layout.addWidget(self.next_button)
        search_layout.addWidget(self.counter)

        self.view = QTreeView(self)
        self.view.setModel(self.model)
        self.view.setItemDelegate(DiffDelegate(self.view))
        self.view.setFont(QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont))
        self.view.setUniformRowHeights(True)
        self.view.setAnimated(False)
        self.view.setIndentation(18)
        self.view.setAllColumnsShowFocus(True)
        self.view.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.view.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self.view.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.view.setVerticalScrollMode(QAbstractItemView.ScrollMode.ScrollPerPixel)
        self.view.setHorizontalScrollMode(QAbstractItemView.ScrollMode.ScrollPerPixel)
        self.view.setTextElideMode(Qt.TextElideMode.ElideNone)

        header = self.view.header()
        header.setSectionResizeMode(QHeaderView.ResizeMode.Interactive)
        header.setStretchLastSection(True)
        self.view.setColumnWidth(0, 670)
        self.view.setColumnWidth(1, 670)

        layout = QVBoxLayout(self)
        layout.addLayout(search_layout)
        layout.addWidget(self.view, 1)

        self.search.textChanged.connect(self.search_changed)
        self.search.returnPressed.connect(self.next_match)
        self.previous_button.clicked.connect(self.previous_clicked)
        self.next_button.clicked.connect(self.next_clicked)

        self.find_shortcut = QShortcut(
            QKeySequence.StandardKey.Find,
            self,
        )
        self.find_shortcut.activated.connect(self.focus_search)

        self.next_shortcut = QShortcut(QKeySequence("F3"), self)
        self.next_shortcut.activated.connect(self.next_match)

        self.previous_shortcut = QShortcut(
            QKeySequence("Shift+F3"),
            self,
        )
        self.previous_shortcut.activated.connect(self.previous_match)

        for row in self.model.nodes:
            if row.status is not Status.SAME and row.nested:
                self.view.setExpanded(self.model.index_for(row), True)

        self.update_search_controls()

    def focus_search(self) -> None:
        self.search.setFocus(Qt.FocusReason.ShortcutFocusReason)
        self.search.selectAll()

    def search_changed(self, text: str) -> None:
        self.results = self.model.set_search(text)
        self.result_position = -1
        if self.results:
            self.move_match(1)
        else:
            self.update_search_controls()

    def previous_clicked(self, checked: bool) -> None:
        self.previous_match()

    def next_clicked(self, checked: bool) -> None:
        self.next_match()

    def previous_match(self) -> None:
        self.move_match(-1)

    def next_match(self) -> None:
        self.move_match(1)

    def move_match(self, direction: int) -> None:
        if not self.results:
            return

        self.result_position = (self.result_position + direction) % len(self.results)
        row = self.results[self.result_position]

        ancestors: list[Row] = []
        current = row.parent
        while current is not None:
            ancestors.append(current)
            current = current.parent

        for ancestor in reversed(ancestors):
            self.view.setExpanded(self.model.index_for(ancestor), True)

        index = self.model.index_for(row)
        self.view.setCurrentIndex(index)
        self.view.scrollTo(
            index,
            QAbstractItemView.ScrollHint.PositionAtCenter,
        )
        self.update_search_controls()

    def update_search_controls(self) -> None:
        available = bool(self.results)
        self.previous_button.setEnabled(available)
        self.next_button.setEnabled(available)

        if available:
            self.counter.setText(f"{self.result_position + 1} / {len(self.results)}")
        else:
            self.counter.setText("0 matches")


@beartype
def load_json(path: Path) -> Any:
    with path.open("r", encoding="utf-8") as stream:
        return json.load(stream)


@beartype
def main() -> int:
    parser = argparse.ArgumentParser(
        description="View a synchronized, foldable JSON diff."
    )
    parser.add_argument("old", type=Path)
    parser.add_argument("new", type=Path)
    arguments = parser.parse_args()

    root = make_diff(
        load_json(arguments.old),
        load_json(arguments.new),
    )

    application = QApplication([sys.argv[0]])
    window = DiffWindow(root, arguments.old, arguments.new)
    window.show()
    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main())

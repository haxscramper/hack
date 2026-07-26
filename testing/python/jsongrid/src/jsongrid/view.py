# jsongrid/view.py
from __future__ import annotations

import logging
from dataclasses import dataclass
from enum import Enum

from beartype import beartype
from beartype.typing import List, Optional
from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import (
    QFrame,
    QGridLayout,
    QLabel,
    QMainWindow,
    QScrollArea,
    QToolButton,
    QVBoxLayout,
    QWidget,
)

from jsongrid.structure import (
    ContainerNode,
    JsonNode,
    MatrixNode,
    MixedListNode,
    ObjectNode,
    ObjectTableNode,
    ScalarListNode,
    ScalarNode,
    ScalarType,
    container_item_count,
    is_empty_container,
    node_summary,
)

log = logging.getLogger(__name__)

STYLE_SHEET = """
QScrollArea { border: none; }
QWidget#documentHolder { background: #ffffff; }
QFrame#gridContainer { background: #c2c7cc; }
QFrame#keyCell, QFrame#headerCell { background: #eaeef2; }
QFrame#indexCell { background: #f2f4f6; }
QFrame#valueCell { background: #ffffff; }
QFrame#emptyCell { background: #fafbfc; }
QFrame#noteCell { background: #f6f8fa; }
QFrame#keyCell QLabel, QFrame#headerCell QLabel {
    color: #1f2328;
    font-weight: 600;
}
QFrame#indexCell QLabel { color: #8c959f; }
QFrame#noteCell QLabel { color: #6e7781; font-style: italic; }
QLabel#scalarNull { color: #8c959f; font-style: italic; }
QLabel#scalarBool { color: #953800; }
QLabel#scalarInt, QLabel#scalarFloat { color: #0550ae; }
QLabel#scalarString { color: #1a7f37; }
QToolButton#gridToggle {
    color: #57606a;
    border: none;
    padding: 1px 3px;
}
"""

SCALAR_STYLE_NAMES = {
    ScalarType.NULL: "scalarNull",
    ScalarType.BOOL: "scalarBool",
    ScalarType.INT: "scalarInt",
    ScalarType.FLOAT: "scalarFloat",
    ScalarType.STRING: "scalarString",
}


class CellRole(Enum):
    """Styling role of a rendered grid cell."""

    KEY = "keyCell"
    HEADER = "headerCell"
    INDEX = "indexCell"
    VALUE = "valueCell"
    EMPTY = "emptyCell"
    NOTE = "noteCell"


@beartype
@dataclass
class RenderConfig:
    """Limits applied while materializing the widget tree."""

    auto_expand_depth: int = 2
    """Deepest nesting level that is expanded without a user click."""

    max_rows: int = 200
    """Largest number of rows built per container before a truncation note."""

    max_scalar_chars: int = 160
    """Longest scalar text kept in a cell before eliding into the tooltip."""


@beartype
def scalar_text(node: ScalarNode) -> str:
    match node.scalar_type:
        case ScalarType.NULL:
            return "null"
        case ScalarType.BOOL:
            return "true" if node.value else "false"
        case ScalarType.STRING:
            return str(node.value)
        case _:
            return str(node.value)


@beartype
class GridContainer(QFrame):
    """Grid whose background paints the one pixel separators between cells."""

    def __init__(self) -> None:
        super().__init__()
        self.setObjectName("gridContainer")
        self.grid = QGridLayout(self)
        self.grid.setContentsMargins(1, 1, 1, 1)
        self.grid.setSpacing(1)


@beartype
class CollapsibleGrid(QWidget):
    """Container node header whose body is built on first expansion."""

    def __init__(self, node: ContainerNode, builder: GridBuilder, depth: int,
                 expanded: bool) -> None:
        super().__init__()
        self.node = node
        self.builder = builder
        self.depth = depth
        self.body: Optional[QWidget] = None

        box = QVBoxLayout(self)
        box.setContentsMargins(0, 0, 0, 0)
        box.setSpacing(1)

        self.toggle = QToolButton(self)
        self.toggle.setObjectName("gridToggle")
        self.toggle.setCheckable(True)
        self.toggle.setAutoRaise(True)
        self.toggle.setArrowType(Qt.ArrowType.RightArrow)
        self.toggle.setToolButtonStyle(
            Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        self.toggle.setText(node_summary(node))
        self.toggle.setToolTip(node.path)
        self.toggle.toggled.connect(self.on_toggled)
        box.addWidget(self.toggle, alignment=Qt.AlignmentFlag.AlignLeft)
        self.toggle.setChecked(expanded)

    def on_toggled(self, checked: bool) -> None:
        self.toggle.setArrowType(
            Qt.ArrowType.DownArrow if checked else Qt.ArrowType.RightArrow)
        if checked and self.body is None:
            self.body = self.builder.build_body(self.node, self.depth)
            self.layout().addWidget(self.body)

        if self.body is not None:
            self.body.setVisible(checked)


@beartype
class GridBuilder:
    """Factory turning inferred structure nodes into nested grid widgets."""

    def __init__(self, config: RenderConfig) -> None:
        self.config = config

    def build(self, node: JsonNode, depth: int) -> QWidget:
        match node:
            case ScalarNode():
                return self.scalar_cell(node)
            case _:
                if is_empty_container(node):
                    return self.note_cell(node_summary(node))
                return CollapsibleGrid(node, self, depth, depth
                                       <= self.config.auto_expand_depth)

    def build_body(self, node: ContainerNode, depth: int) -> QWidget:
        match node:
            case ObjectNode():
                return self.build_object(node, depth)
            case ObjectTableNode():
                return self.build_table(node, depth)
            case ScalarListNode():
                return self.build_scalar_list(node)
            case MatrixNode():
                return self.build_matrix(node, depth)
            case MixedListNode():
                return self.build_mixed(node, depth)

    def build_object(self, node: ObjectNode, depth: int) -> QWidget:
        container = GridContainer()
        for row, entry in enumerate(self.limit(node.entries)):
            container.grid.addWidget(self.text_cell(CellRole.KEY, entry.key),
                                     row, 0)
            container.grid.addWidget(
                self.wrap_cell(CellRole.VALUE,
                               self.build(entry.value, depth + 1)),
                row,
                1,
            )

        self.append_truncation(container, node, len(node.entries), 2)
        container.grid.setColumnStretch(1, 1)
        return container

    def build_table(self, node: ObjectTableNode, depth: int) -> QWidget:
        container = GridContainer()
        container.grid.addWidget(self.text_cell(CellRole.HEADER, "#"), 0, 0)
        for column, definition in enumerate(node.columns):
            header = self.text_cell(CellRole.HEADER, definition.name)
            header.setToolTip(
                f"{definition.occurrence_count} of {len(node.rows)} records "
                f"({definition.fill_ratio:.0%})")
            container.grid.addWidget(header, 0, column + 1)
            container.grid.setColumnStretch(column + 1, 1)

        for row, record in enumerate(self.limit(node.rows), start=1):
            container.grid.addWidget(
                self.text_cell(CellRole.INDEX, str(record.index)), row, 0)
            for column, definition in enumerate(node.columns):
                cell = record.cells.get(definition.name)
                widget = (self.wrap_cell(CellRole.EMPTY, QLabel(""))
                          if cell is None else self.wrap_cell(
                              CellRole.VALUE, self.build(cell, depth + 1)))
                container.grid.addWidget(widget, row, column + 1)

        self.append_truncation(container, node, len(node.rows),
                               len(node.columns) + 1)
        return container

    def build_matrix(self, node: MatrixNode, depth: int) -> QWidget:
        container = GridContainer()
        container.grid.addWidget(self.text_cell(CellRole.HEADER, "#"), 0, 0)
        for column in range(node.column_count):
            container.grid.addWidget(
                self.text_cell(CellRole.HEADER, str(column)), 0, column + 1)
            container.grid.setColumnStretch(column + 1, 1)

        for row, cells in enumerate(self.limit(node.rows)):
            container.grid.addWidget(self.text_cell(CellRole.INDEX, str(row)),
                                     row + 1, 0)
            for column in range(node.column_count):
                widget = (self.wrap_cell(CellRole.VALUE,
                                         self.build(cells[column], depth +
                                                    1)) if column < len(cells)
                          else self.wrap_cell(CellRole.EMPTY, QLabel("")))
                container.grid.addWidget(widget, row + 1, column + 1)

        self.append_truncation(container, node, len(node.rows),
                               node.column_count + 1)
        return container

    def build_scalar_list(self, node: ScalarListNode) -> QWidget:
        container = GridContainer()
        for row, item in enumerate(self.limit(node.items)):
            container.grid.addWidget(self.text_cell(CellRole.INDEX, str(row)),
                                     row, 0)
            container.grid.addWidget(
                self.wrap_cell(CellRole.VALUE, self.scalar_label(item)), row,
                1)

        self.append_truncation(container, node, len(node.items), 2)
        container.grid.setColumnStretch(1, 1)
        return container

    def build_mixed(self, node: MixedListNode, depth: int) -> QWidget:
        container = GridContainer()
        for row, item in enumerate(self.limit(node.items)):
            container.grid.addWidget(self.text_cell(CellRole.INDEX, str(row)),
                                     row, 0)
            container.grid.addWidget(
                self.wrap_cell(CellRole.VALUE, self.build(item, depth + 1)),
                row, 1)

        self.append_truncation(container, node, len(node.items), 2)
        container.grid.setColumnStretch(1, 1)
        return container

    def limit(self, items: List) -> List:
        return items[:self.config.max_rows]

    def append_truncation(
        self,
        container: GridContainer,
        node: ContainerNode,
        total: int,
        span: int,
    ) -> None:
        if total <= self.config.max_rows:
            return

        hidden = total - self.config.max_rows
        log.debug(f"{node.path}: hiding {hidden} of {total} rows")
        container.grid.addWidget(
            self.text_cell(CellRole.NOTE, f"{hidden} more items not shown"),
            container.grid.rowCount(),
            0,
            1,
            span,
        )

    def wrap_cell(self, role: CellRole, inner: QWidget) -> QFrame:
        frame = QFrame()
        frame.setObjectName(role.value)
        box = QVBoxLayout(frame)
        box.setContentsMargins(5, 2, 5, 2)
        box.setSpacing(0)
        box.addWidget(inner, alignment=Qt.AlignmentFlag.AlignTop)
        return frame

    def text_cell(self, role: CellRole, text: str) -> QFrame:
        return self.wrap_cell(role, self.make_label(text))

    def note_cell(self, text: str) -> QFrame:
        return self.text_cell(CellRole.NOTE, text)

    def scalar_cell(self, node: ScalarNode) -> QWidget:
        return self.scalar_label(node)

    def scalar_label(self, node: ScalarNode) -> QLabel:
        label = self.make_label(scalar_text(node))
        label.setObjectName(SCALAR_STYLE_NAMES[node.scalar_type])
        return label

    def make_label(self, text: str) -> QLabel:
        flat = text.replace("\n", " ").replace("\t", " ")
        if self.config.max_scalar_chars < len(flat):
            flat = flat[:self.config.max_scalar_chars] + "\u2026"

        label = QLabel(flat)
        label.setWordWrap(False)
        label.setAlignment(Qt.AlignmentFlag.AlignLeft
                           | Qt.AlignmentFlag.AlignTop)
        label.setTextInteractionFlags(
            Qt.TextInteractionFlag.TextSelectableByMouse)
        if flat != text:
            label.setToolTip(text)

        return label


@beartype
class MainWindow(QMainWindow):
    """Scrollable window hosting the grid for one document."""

    def __init__(self, title: str, node: JsonNode,
                 config: RenderConfig) -> None:
        super().__init__()
        self.setWindowTitle(title)

        holder = QWidget()
        holder.setObjectName("documentHolder")
        box = QVBoxLayout(holder)
        box.setContentsMargins(8, 8, 8, 8)
        box.addWidget(GridBuilder(config).build(node, 0),
                      alignment=Qt.AlignmentFlag.AlignTop)
        box.addStretch(1)

        area = QScrollArea()
        area.setWidgetResizable(True)
        area.setWidget(holder)
        self.setCentralWidget(area)
        self.resize(1280, 860)

#!/usr/bin/env python

import argparse
import sys
import xml.etree.ElementTree as ElementTree
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

import pydot
from beartype import beartype
from beartype.typing import Any, Callable
from plumbum import local
from PyQt6.QtCore import (
    QAbstractTableModel,
    QByteArray,
    QModelIndex,
    QRectF,
    Qt,
    QTimer,
)
from PyQt6.QtGui import (
    QAction,
    QBrush,
    QColor,
    QMouseEvent,
    QPainter,
    QPaintEvent,
    QPen,
    QResizeEvent,
    QWheelEvent,
)
from PyQt6.QtSvg import QSvgRenderer
from PyQt6.QtSvgWidgets import QGraphicsSvgItem
from PyQt6.QtWidgets import (
    QApplication,
    QComboBox,
    QFileDialog,
    QFrame,
    QGraphicsRectItem,
    QGraphicsScene,
    QGraphicsSceneHoverEvent,
    QGraphicsSceneMouseEvent,
    QGraphicsView,
    QHeaderView,
    QLabel,
    QMainWindow,
    QSizePolicy,
    QSplitter,
    QTableView,
    QVBoxLayout,
    QWidget,
)


class RankDirection(Enum):
    TOP_TO_BOTTOM = "TB"
    LEFT_TO_RIGHT = "LR"
    BOTTOM_TO_TOP = "BT"
    RIGHT_TO_LEFT = "RL"


@dataclass(frozen=True)
class PropertyRecord:
    name: str
    value: str


@dataclass(frozen=True)
class NodeRecord:
    name: str
    element_id: str
    properties: list[PropertyRecord]


@dataclass(frozen=True)
class RenderedGraph:
    svg: bytes
    nodes: list[NodeRecord]


@beartype
def clean_dot_value(value: str) -> str:
    if 1 < len(value) and value.startswith("\"") and value.endswith("\""):
        return value[1:-1].replace("\\\"", "\"").replace("\\n", "\n")
    return value


@beartype
def parse_graph(source: str) -> pydot.Dot:
    graphs = pydot.graph_from_dot_data(source)
    if graphs is None:
        raise ValueError("Graphviz input could not be parsed into a graph")

    if len(graphs) != 1:
        raise ValueError(
            f"Expected exactly one Graphviz graph, but parsed {len(graphs)} graphs"
        )

    return graphs[0]


@beartype
def svg_node_elements(svg: bytes) -> list[PropertyRecord]:
    root = ElementTree.fromstring(svg)
    result: list[PropertyRecord] = []

    for element in root.iter():
        classes = element.attrib.get("class", "").split()
        if "node" not in classes:
            continue

        element_id = element.attrib.get("id")
        if element_id is None:
            raise ValueError(
                "Graphviz SVG node group does not have an id attribute")

        title = next(
            (nested for nested in element
             if nested.tag.rsplit("}", maxsplit=1)[-1] == "title"),
            None,
        )
        if title is None or title.text is None:
            raise ValueError(
                f"Graphviz SVG node group {element_id!r} does not have a title"
            )

        result.append(PropertyRecord(title.text, element_id))

    return result


@beartype
def render_graph(graph: Any) -> RenderedGraph:
    source = graph.to_string()
    command = local["dot"]["-Tsvg"] << source
    return_code, stdout, stderr = command.run(retcode=None)

    if return_code != 0:
        raise RuntimeError(
            f"Graphviz exited with code {return_code}: {stderr.strip()}")

    svg = stdout.encode("utf-8")
    element_records = svg_node_elements(svg)
    attributes_by_name: dict[str, list[PropertyRecord]] = {}

    for node in graph.get_nodes():
        name = clean_dot_value(node.get_name())
        attributes = [
            PropertyRecord(attribute_name, clean_dot_value(attribute_value))
            for attribute_name, attribute_value in
            node.get_attributes().items()
        ]
        attributes_by_name[name] = attributes

    records: list[NodeRecord] = []
    for element_record in element_records:
        name = element_record.name
        properties = [PropertyRecord("name", name)]
        properties.extend(attributes_by_name.get(name, []))
        records.append(
            NodeRecord(
                name=name,
                element_id=element_record.value,
                properties=properties,
            ))

    return RenderedGraph(svg=svg, nodes=records)


class PropertyModel(QAbstractTableModel):

    @beartype
    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.properties: list[PropertyRecord] = []

    @beartype
    def set_properties(self, properties: list[PropertyRecord]) -> None:
        self.beginResetModel()
        self.properties = properties
        self.endResetModel()

    @beartype
    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self.properties)

    @beartype
    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return 2

    @beartype
    def data(
            self,
            index: QModelIndex,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if not index.isValid() or role != int(Qt.ItemDataRole.DisplayRole):
            return None

        record = self.properties[index.row()]
        match index.column():
            case 0:
                return record.name
            case 1:
                return record.value
            case _:
                return None

    @beartype
    def headerData(
            self,
            section: int,
            orientation: Qt.Orientation,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if (orientation != Qt.Orientation.Horizontal
                or role != int(Qt.ItemDataRole.DisplayRole)):
            return None

        match section:
            case 0:
                return "Property"
            case 1:
                return "Value"
            case _:
                return None

    @beartype
    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags

        return Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable


class NodeHitItem(QGraphicsRectItem):

    @beartype
    def __init__(
        self,
        rectangle: QRectF,
        node: NodeRecord,
        selected: Callable[[NodeRecord, "NodeHitItem"], None],
    ) -> None:
        super().__init__(rectangle)
        self.node = node
        self.selected = selected
        self.active = False
        self.setAcceptHoverEvents(True)
        self.setPen(QPen(Qt.PenStyle.NoPen))
        self.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        self.setZValue(10.0)

    @beartype
    def set_active(self, active: bool) -> None:
        self.active = active
        if active:
            self.setPen(QPen(QColor(40, 130, 220), 2.0))
            self.setBrush(QBrush(QColor(40, 130, 220, 35)))
        else:
            self.setPen(QPen(Qt.PenStyle.NoPen))
            self.setBrush(QBrush(Qt.BrushStyle.NoBrush))

    @beartype
    def hoverEnterEvent(self, event: QGraphicsSceneHoverEvent) -> None:
        if not self.active:
            self.setPen(QPen(QColor(80, 160, 240), 1.5))
            self.setBrush(QBrush(QColor(80, 160, 240, 20)))
        super().hoverEnterEvent(event)

    @beartype
    def hoverLeaveEvent(self, event: QGraphicsSceneHoverEvent) -> None:
        if not self.active:
            self.setPen(QPen(Qt.PenStyle.NoPen))
            self.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        super().hoverLeaveEvent(event)

    @beartype
    def mousePressEvent(self, event: QGraphicsSceneMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            self.selected(self.node, self)
            event.accept()
            return

        super().mousePressEvent(event)


class MinimapView(QGraphicsView):

    @beartype
    def __init__(
        self,
        scene: QGraphicsScene,
        canvas: "GraphView",
        parent: QWidget,
    ) -> None:
        super().__init__(scene, parent)
        self.canvas = canvas
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFrameShape(QFrame.Shape.Box)
        self.setBackgroundBrush(QBrush(QColor(250, 250, 250)))
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.timer = QTimer(self)
        self.timer.timeout.connect(self.refresh)
        self.timer.start(50)

    @beartype
    def refresh(self) -> None:
        scene = self.scene()
        if scene is not None and not scene.sceneRect().isEmpty():
            self.fitInView(
                scene.sceneRect(),
                Qt.AspectRatioMode.KeepAspectRatio,
            )
        self.viewport().update()

    @beartype
    def paintEvent(self, event: QPaintEvent) -> None:
        super().paintEvent(event)

        scene = self.scene()
        if scene is None or scene.sceneRect().isEmpty():
            return

        visible_scene = self.canvas.mapToScene(
            self.canvas.viewport().rect()).boundingRect()
        visible_polygon = self.mapFromScene(visible_scene)

        painter = QPainter(self.viewport())
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setPen(QPen(self.palette().highlight().color(), 2.0))
        painter.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        painter.drawPolygon(visible_polygon)
        painter.end()


class GraphView(QGraphicsView):

    @beartype
    def __init__(
        self,
        node_selected: Callable[[NodeRecord], None],
        parent: QWidget | None = None,
    ) -> None:
        self.graph_scene = QGraphicsScene()
        super().__init__(self.graph_scene, parent)
        self.node_selected = node_selected
        self.renderer: QSvgRenderer | None = None
        self.selected_item: NodeHitItem | None = None
        self.pan_start: QPoint | None = None
        self.pan_last: QPoint | None = None
        self.pan_moved = False
        self.setRenderHints(QPainter.RenderHint.Antialiasing
                            | QPainter.RenderHint.TextAntialiasing
                            | QPainter.RenderHint.SmoothPixmapTransform)
        self.setDragMode(QGraphicsView.DragMode.NoDrag)
        self.setTransformationAnchor(
            QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.ViewportAnchor.AnchorViewCenter)
        self.setBackgroundBrush(QBrush(QColor(245, 245, 245)))
        self.minimap = MinimapView(self.graph_scene, self, self)
        self.position_minimap()

    @beartype
    def position_minimap(self) -> None:
        width = 240
        height = 170
        margin = 12
        y = self.height() - height - margin

        if y < margin:
            y = margin

        self.minimap.setGeometry(margin, y, width, height)
        self.minimap.raise_()

    @beartype
    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self.position_minimap()

    @beartype
    def wheelEvent(self, event: QWheelEvent) -> None:
        delta = event.angleDelta().y()

        if delta == 0:
            event.accept()
            return

        factor = 1.2 if 0 < delta else 1.0 / 1.2
        next_scale = self.transform().m11() * factor

        if 0.01 < next_scale and next_scale < 12.0:
            self.scale(factor, factor)

        event.accept()

    @beartype
    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        position = event.position().toPoint()
        self.pan_start = position
        self.pan_last = position
        self.pan_moved = False
        self.viewport().setCursor(Qt.CursorShape.ClosedHandCursor)
        event.accept()

    @beartype
    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        if self.pan_start is None or self.pan_last is None:
            super().mouseMoveEvent(event)
            return

        position = event.position().toPoint()
        movement = position - self.pan_last
        total_movement = position - self.pan_start

        if 4 < total_movement.manhattanLength():
            self.pan_moved = True

        self.horizontalScrollBar().setValue(
            self.horizontalScrollBar().value() - movement.x())
        self.verticalScrollBar().setValue(self.verticalScrollBar().value() -
                                          movement.y())
        self.pan_last = position
        event.accept()

    @beartype
    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        if (event.button() != Qt.MouseButton.LeftButton
                or self.pan_start is None):
            super().mouseReleaseEvent(event)
            return

        position = event.position().toPoint()
        was_moved = self.pan_moved
        self.pan_start = None
        self.pan_last = None
        self.pan_moved = False
        self.viewport().unsetCursor()

        if not was_moved:
            item = self.itemAt(position)

            match item:
                case NodeHitItem():
                    self.select_node(item.node, item)

        event.accept()

    @beartype
    def mouseDoubleClickEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            self.fit_graph()
            event.accept()
            return

        super().mouseDoubleClickEvent(event)

    @beartype
    def display_graph(self, graph: RenderedGraph) -> None:
        previous_renderer = self.renderer
        self.graph_scene.clear()
        self.selected_item = None

        if previous_renderer is not None:
            previous_renderer.deleteLater()

        self.renderer = QSvgRenderer(QByteArray(graph.svg), self)
        if not self.renderer.isValid():
            raise ValueError("Graphviz produced SVG that Qt could not render")

        svg_item = QGraphicsSvgItem()
        svg_item.setSharedRenderer(self.renderer)
        svg_item.setZValue(0.0)
        self.graph_scene.addItem(svg_item)

        scene_rectangle = self.renderer.viewBoxF()
        if scene_rectangle.isEmpty():
            raise ValueError("Graphviz SVG has an empty view box")

        self.graph_scene.setSceneRect(scene_rectangle)

        for node in graph.nodes:
            bounds = self.renderer.boundsOnElement(node.element_id)
            transform = self.renderer.transformForElement(node.element_id)
            rectangle = transform.mapRect(bounds)

            if rectangle.isEmpty():
                raise ValueError(
                    f"Graphviz SVG node {node.name!r} has empty rendered bounds"
                )

            hit_item = NodeHitItem(rectangle, node, self.select_node)
            self.graph_scene.addItem(hit_item)

        QTimer.singleShot(0, self.fit_graph)
        self.minimap.refresh()

    @beartype
    def select_node(self, node: NodeRecord, item: NodeHitItem) -> None:
        if self.selected_item is not None:
            self.selected_item.set_active(False)

        self.selected_item = item
        item.set_active(True)
        self.node_selected(node)

    @beartype
    def fit_graph(self) -> None:
        if not self.graph_scene.sceneRect().isEmpty():
            self.fitInView(
                self.graph_scene.sceneRect(),
                Qt.AspectRatioMode.KeepAspectRatio,
            )


class MainWindow(QMainWindow):

    @beartype
    def __init__(self, input_path: Path | None) -> None:
        super().__init__()
        self.graph: Any | None = None
        self.input_path = input_path
        self.directions = list(RankDirection)
        self.property_model = PropertyModel(self)
        self.property_table = QTableView()
        self.property_table.setModel(self.property_model)
        self.property_table.setWordWrap(True)
        self.property_table.setAlternatingRowColors(True)
        self.property_table.verticalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.ResizeToContents)
        self.property_table.horizontalHeader().setSectionResizeMode(
            0,
            QHeaderView.ResizeMode.ResizeToContents,
        )
        self.property_table.horizontalHeader().setSectionResizeMode(
            1,
            QHeaderView.ResizeMode.Stretch,
        )

        self.node_title = QLabel("No node selected")
        self.node_title.setTextInteractionFlags(
            Qt.TextInteractionFlag.TextSelectableByMouse)

        property_panel = QWidget()
        property_layout = QVBoxLayout(property_panel)
        property_layout.addWidget(self.node_title)
        property_layout.addWidget(self.property_table)

        self.graph_view = GraphView(self.node_selected)
        self.rank_combo = QComboBox()
        for direction in self.directions:
            self.rank_combo.addItem(
                f"{direction.name.replace('_', ' ').title()} ({direction.value})"
            )
        self.rank_combo.currentIndexChanged.connect(self.rank_changed)

        controls = QWidget()
        controls_layout = QVBoxLayout(controls)
        controls_layout.setContentsMargins(0, 0, 0, 0)
        controls_layout.addWidget(QLabel("Rank direction"))
        controls_layout.addWidget(self.rank_combo)
        controls.setSizePolicy(
            QSizePolicy.Policy.Preferred,
            QSizePolicy.Policy.Fixed,
        )

        graph_panel = QWidget()
        graph_layout = QVBoxLayout(graph_panel)
        graph_layout.addWidget(controls)
        graph_layout.addWidget(self.graph_view)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(property_panel)
        splitter.addWidget(graph_panel)
        splitter.setSizes([380, 1200])

        self.setCentralWidget(splitter)
        self.create_actions()
        self.setWindowTitle("Graphviz Explorer")
        self.resize(1500, 900)

        if input_path is not None:
            self.load_path(input_path)

    @beartype
    def create_actions(self) -> None:
        open_action = QAction("Open", self)
        open_action.setShortcut("Ctrl+O")
        open_action.triggered.connect(self.open_graph)
        self.menuBar().addMenu("File").addAction(open_action)

    @beartype
    def open_graph(self, checked: bool = False) -> None:
        selected_path, selected_filter = QFileDialog.getOpenFileName(
            self,
            "Open Graphviz document",
            str(self.input_path.parent if self.
                input_path is not None else Path.cwd()),
            "Graphviz documents (*.dot *.gv);;All files (*)",
        )
        if selected_path == "":
            return

        self.load_path(Path(selected_path))

    @beartype
    def load_path(self, path: Path) -> None:
        source = path.read_text(encoding="utf-8")
        self.graph = parse_graph(source)
        self.input_path = path
        self.setWindowTitle(f"Graphviz Explorer — {path.name}")

        rank_value = self.graph.get_rankdir()
        if rank_value is not None:
            cleaned_rank = clean_dot_value(rank_value)
            for index, direction in enumerate(self.directions):
                if direction.value == cleaned_rank:
                    self.rank_combo.blockSignals(True)
                    self.rank_combo.setCurrentIndex(index)
                    self.rank_combo.blockSignals(False)
                    break

        self.render_current_graph()

    @beartype
    def rank_changed(self, index: int) -> None:
        if self.graph is None:
            return

        direction = self.directions[index]
        self.graph.set("rankdir", direction.value)
        self.render_current_graph()

    @beartype
    def render_current_graph(self) -> None:
        if self.graph is None:
            raise RuntimeError(
                "Cannot render a graph because no Graphviz document is loaded")

        self.property_model.set_properties([])
        self.node_title.setText("No node selected")
        self.graph_view.display_graph(render_graph(self.graph))

    @beartype
    def node_selected(self, node: NodeRecord) -> None:
        self.node_title.setText(node.name)
        self.property_model.set_properties(node.properties)


@beartype
def parse_arguments(arguments: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Interactively visualize a Graphviz document")
    parser.add_argument(
        "input",
        nargs="?",
        type=Path,
        help="Graphviz DOT document to open",
    )
    return parser.parse_args(arguments)


@beartype
def main(arguments: list[str]) -> int:
    options = parse_arguments(arguments)
    application = QApplication(sys.argv)
    window = MainWindow(options.input)
    window.show()
    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))

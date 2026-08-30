#!/usr/bin/env python

import argparse
import html
import math
import shlex
import sys
from dataclasses import dataclass, field
from html.parser import HTMLParser
from pathlib import Path

import pydot
from PyQt6.QtCore import QPointF, QRect, QRectF, Qt, pyqtSignal
from PyQt6.QtGui import (
    QAction,
    QColor,
    QBrush,
    QFont,
    QPainter,
    QPainterPath,
    QPen,
    QPolygonF,
    QTextDocument,
    QTextOption,
    QTransform,
)
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
    QComboBox,
    QFileDialog,
    QFormLayout,
    QGraphicsItem,
    QGraphicsPathItem,
    QGraphicsPolygonItem,
    QGraphicsScene,
    QGraphicsView,
    QHeaderView,
    QLabel,
    QMainWindow,
    QMessageBox,
    QSplitter,
    QStyle,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

NODE_WIDTH = 380.0
NODE_PADDING = 8.0
NODE_MINIMUM_HEIGHT = 42.0
LAYOUT_DPI = 96.0

NODE_BACKGROUND = QColor("#ffffff")
NODE_BORDER = QColor("#606770")
NODE_SELECTED_BORDER = QColor("#1976d2")
EDGE_COLOR = QColor("#68707a")
MINIMAP_BACKGROUND = QColor(245, 247, 250, 235)
MINIMAP_BORDER = QColor("#646b73")
MINIMAP_NODE = QColor("#59636e")
MINIMAP_EDGE = QColor("#9aa0a6")
MINIMAP_VIEWPORT = QColor("#d32f2f")


def unquote_dot_value(value: str | None) -> str:
    if value is None:
        return ""

    value = str(value).strip()

    if len(value) >= 2 and value[0] == '"' and value[-1] == '"':
        value = value[1:-1]
        value = value.replace(r"\"", '"')
        value = value.replace(r"\\", "\\")

    return value


def normalize_node_name(name: str) -> str:
    return unquote_dot_value(name).strip()


def display_dot_value(value: str | None) -> str:
    value = unquote_dot_value(value)
    return (value.replace(r"\l", "\n").replace(r"\r",
                                               "\n").replace(r"\n", "\n"))


class GraphvizHtmlConverter(HTMLParser):
    allowed_inline_tags = {
        "b",
        "i",
        "u",
        "o",
        "s",
        "sub",
        "sup",
    }

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.output: list[str] = []
        self.table_stack: list[dict[str, str]] = []
        self.open_tags: list[str | None] = []

    @staticmethod
    def attributes_dict(
        attributes: list[tuple[str, str | None]], ) -> dict[str, str]:
        return {key.lower(): value or "" for key, value in attributes}

    @staticmethod
    def css_color(value: str) -> str:
        return html.escape(value, quote=True)

    def handle_starttag(
        self,
        tag: str,
        attributes: list[tuple[str, str | None]],
    ) -> None:
        tag = tag.lower()
        attrs = self.attributes_dict(attributes)

        if tag == "table":
            border = attrs.get("border", "0")
            cellborder = attrs.get("cellborder", "0")
            cellspacing = attrs.get("cellspacing", "0")
            cellpadding = attrs.get("cellpadding", "3")

            styles = [
                "border-collapse: separate",
                f"border-spacing: {html.escape(cellspacing)}px",
            ]

            if border not in {"", "0"}:
                styles.append(f"border: {html.escape(border)}px solid #606770")

            bgcolor = attrs.get("bgcolor")
            if bgcolor:
                styles.append(f"background-color: {self.css_color(bgcolor)}")

            self.output.append(
                '<table width="100%" '
                f'cellspacing="{html.escape(cellspacing, quote=True)}" '
                f'cellpadding="{html.escape(cellpadding, quote=True)}" '
                f'style="{"; ".join(styles)}">')
            self.table_stack.append({
                "cellborder": cellborder,
            })
            self.open_tags.append("table")
            return

        if tag in {"tr", "td"}:
            if tag == "tr":
                self.output.append("<tr>")
                self.open_tags.append("tr")
                return

            styles: list[str] = []
            table = self.table_stack[-1] if self.table_stack else {}
            cellborder = table.get("cellborder", "0")

            if cellborder not in {"", "0"}:
                styles.append(
                    f"border: {html.escape(cellborder)}px solid #606770")

            align = attrs.get("align", "").lower()
            if align in {"left", "right", "center", "justify"}:
                styles.append(f"text-align: {align}")

            valign = attrs.get("valign", "").lower()
            if valign in {"top", "middle", "bottom"}:
                styles.append(f"vertical-align: {valign}")

            bgcolor = attrs.get("bgcolor")
            if bgcolor:
                styles.append(f"background-color: {self.css_color(bgcolor)}")

            tag_attributes: list[str] = []
            for name in ("colspan", "rowspan"):
                value = attrs.get(name)
                if value:
                    tag_attributes.append(
                        f'{name}="{html.escape(value, quote=True)}"')

            if styles:
                tag_attributes.append(
                    f'style="{html.escape("; ".join(styles), quote=True)}"')

            suffix = ""
            if tag_attributes:
                suffix = " " + " ".join(tag_attributes)

            self.output.append(f"<td{suffix}>")
            self.open_tags.append("td")
            return

        if tag == "br":
            self.output.append("<br/>")
            self.open_tags.append(None)
            return

        if tag == "font":
            styles: list[str] = []

            color = attrs.get("color")
            if color:
                styles.append(f"color: {self.css_color(color)}")

            face = attrs.get("face")
            if face:
                styles.append(
                    f"font-family: '{html.escape(face, quote=True)}'")

            point_size = attrs.get("point-size")
            if point_size:
                styles.append(
                    f"font-size: {html.escape(point_size, quote=True)}pt")

            style = html.escape("; ".join(styles), quote=True)
            self.output.append(f'<span style="{style}">')
            self.open_tags.append("span")
            return

        if tag in self.allowed_inline_tags:
            qt_tag = "s" if tag == "o" else tag
            self.output.append(f"<{qt_tag}>")
            self.open_tags.append(qt_tag)
            return

        self.open_tags.append(None)

    def handle_startendtag(
        self,
        tag: str,
        attributes: list[tuple[str, str | None]],
    ) -> None:
        if tag.lower() == "br":
            self.output.append("<br/>")
            return

        self.handle_starttag(tag, attributes)
        self.handle_endtag(tag)

    def handle_endtag(self, tag: str) -> None:
        if not self.open_tags:
            return

        output_tag = self.open_tags.pop()

        if tag.lower() == "table" and self.table_stack:
            self.table_stack.pop()

        if output_tag is not None:
            self.output.append(f"</{output_tag}>")

    def handle_data(self, data: str) -> None:
        self.output.append(html.escape(data))

    def result(self) -> str:
        while self.open_tags:
            output_tag = self.open_tags.pop()
            if output_tag is not None:
                self.output.append(f"</{output_tag}>")

        return "".join(self.output)


def graphviz_label_to_qt_html(label: str | None) -> str:
    raw_label = str(label or "").strip()

    if raw_label.startswith("<<") and raw_label.endswith(">>"):
        graphviz_html = raw_label[1:-1]
        converter = GraphvizHtmlConverter()
        converter.feed(graphviz_html)
        converter.close()
        body = converter.result()
    else:
        plain_text = display_dot_value(raw_label)
        body = html.escape(plain_text).replace("\n", "<br/>")

    return ("<html>"
            "<head>"
            "<style>"
            "body { color: #202124; font-family: sans-serif; }"
            "table { width: 100%; }"
            "td { padding: 3px; }"
            "</style>"
            "</head>"
            f"<body>{body}</body>"
            "</html>")


def create_text_document(qt_html: str, width: float) -> QTextDocument:
    document = QTextDocument()
    document.setDocumentMargin(0.0)

    text_option = document.defaultTextOption()
    text_option.setWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)
    document.setDefaultTextOption(text_option)

    document.setHtml(qt_html)
    document.setTextWidth(width)
    document.adjustSize()
    document.setTextWidth(width)
    return document


@dataclass
class NodeModel:
    layout_id: str
    name: str
    attributes: dict[str, str]
    qt_html: str
    width: float
    height: float
    center: QPointF = field(default_factory=lambda: QPointF())


@dataclass
class EdgeModel:
    source_id: str
    destination_id: str
    attributes: dict[str, str]
    points: list[QPointF]


def collect_graph_nodes(graph: pydot.Graph) -> list[pydot.Node]:
    result = list(graph.get_nodes())

    for subgraph in graph.get_subgraphs():
        result.extend(collect_graph_nodes(subgraph))

    return result


def collect_graph_edges(graph: pydot.Graph) -> list[pydot.Edge]:
    result = list(graph.get_edges())

    for subgraph in graph.get_subgraphs():
        result.extend(collect_graph_edges(subgraph))

    return result


def parse_plain_layout(
    plain_text: str,
) -> tuple[dict[str, QPointF], list[tuple[str, str, list[QPointF]]]]:
    positions: dict[str, QPointF] = {}
    edges: list[tuple[str, str, list[QPointF]]] = []

    for line in plain_text.splitlines():
        tokens = shlex.split(line)

        if not tokens:
            continue

        if tokens[0] == "node" and len(tokens) >= 4:
            node_id = tokens[1]
            x = float(tokens[2]) * LAYOUT_DPI
            y = -float(tokens[3]) * LAYOUT_DPI
            positions[node_id] = QPointF(x, y)
            continue

        if tokens[0] == "edge" and len(tokens) >= 5:
            source_id = tokens[1]
            destination_id = tokens[2]
            point_count = int(tokens[3])
            coordinates = tokens[4:4 + point_count * 2]

            points = [
                QPointF(
                    float(coordinates[index]) * LAYOUT_DPI,
                    -float(coordinates[index + 1]) * LAYOUT_DPI,
                ) for index in range(0, len(coordinates), 2)
            ]

            edges.append((source_id, destination_id, points))

    return positions, edges


class NodeItem(QGraphicsItem):
    clicked = pyqtSignal(object)

    def __init__(self, model: NodeModel) -> None:
        super().__init__()
        self.model = model

        content_width = model.width - NODE_PADDING * 2.0
        self.document = create_text_document(model.qt_html, content_width)

        self.setFlag(
            QGraphicsItem.GraphicsItemFlag.ItemIsSelectable,
            True,
        )
        self.setAcceptedMouseButtons(Qt.MouseButton.LeftButton)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setZValue(10.0)

    def boundingRect(self) -> QRectF:
        return QRectF(0.0, 0.0, self.model.width, self.model.height)

    def paint(self, painter, option, widget=None) -> None:
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        selected = bool(option.state & QStyle.StateFlag.State_Selected)
        border_color = (NODE_SELECTED_BORDER if selected else NODE_BORDER)
        border_width = 2.5 if selected else 1.0

        painter.setBrush(QBrush(NODE_BACKGROUND))
        painter.setPen(QPen(border_color, border_width))
        painter.drawRoundedRect(
            self.boundingRect(),
            3.0,
            3.0,
        )

        painter.save()
        painter.translate(NODE_PADDING, NODE_PADDING)

        clip_rect = QRectF(
            0.0,
            0.0,
            self.model.width - NODE_PADDING * 2.0,
            self.model.height - NODE_PADDING * 2.0,
        )
        painter.setClipRect(clip_rect)
        self.document.drawContents(painter, clip_rect)
        painter.restore()

    def mousePressEvent(self, event) -> None:
        scene = self.scene()
        if scene is not None:
            scene.clearSelection()

        self.setSelected(True)

        view = self.scene().views()[0] if self.scene().views() else None
        if isinstance(view, GraphView):
            view.nodeClicked.emit(self.model)

        event.accept()


class EdgeItem(QGraphicsPathItem):

    def __init__(
        self,
        points: list[QPointF],
        attributes: dict[str, str],
    ) -> None:
        super().__init__()

        path = QPainterPath()

        if points:
            path.moveTo(points[0])

            if len(points) >= 4 and (len(points) - 1) % 3 == 0:
                index = 1
                while index + 2 < len(points):
                    path.cubicTo(
                        points[index],
                        points[index + 1],
                        points[index + 2],
                    )
                    index += 3
            else:
                for point in points[1:]:
                    path.lineTo(point)

        color = QColor(unquote_dot_value(attributes.get("color", "")))
        if not color.isValid():
            color = EDGE_COLOR

        pen = QPen(color, 1.5)
        pen.setCosmetic(True)
        self.setPen(pen)
        self.setBrush(QBrush(Qt.BrushStyle.NoBrush))
        self.setPath(path)
        self.setZValue(-10.0)

        if len(points) >= 2:
            self.add_arrowhead(points, color)

    def add_arrowhead(
        self,
        points: list[QPointF],
        color: QColor,
    ) -> None:
        end = points[-1]
        previous = points[-2]

        direction = end - previous
        length = math.hypot(direction.x(), direction.y())

        if length == 0.0:
            return

        unit_x = direction.x() / length
        unit_y = direction.y() / length

        arrow_length = 12.0
        arrow_half_width = 5.0

        base = QPointF(
            end.x() - unit_x * arrow_length,
            end.y() - unit_y * arrow_length,
        )
        perpendicular = QPointF(
            -unit_y * arrow_half_width,
            unit_x * arrow_half_width,
        )

        polygon = QPolygonF([
            end,
            base + perpendicular,
            base - perpendicular,
        ])

        arrow = QGraphicsPolygonItem(polygon, self)
        arrow.setPen(QPen(color, 1.0))
        arrow.setBrush(QBrush(color))


class MinimapWidget(QWidget):

    def __init__(self, graph_view: "GraphView") -> None:
        super().__init__(graph_view)
        self.graph_view = graph_view
        self.setFixedSize(220, 150)
        self.setCursor(Qt.CursorShape.CrossCursor)
        self.setAttribute(
            Qt.WidgetAttribute.WA_OpaquePaintEvent,
            False,
        )
        self.raise_()

    def content_rect(self) -> QRectF:
        return QRectF(
            8.0,
            8.0,
            self.width() - 16.0,
            self.height() - 16.0,
        )

    def scene_bounds(self) -> QRectF:
        bounds = self.graph_view.scene().itemsBoundingRect()

        if bounds.isNull() or bounds.width() <= 0.0 or bounds.height() <= 0.0:
            return QRectF(-1.0, -1.0, 2.0, 2.0)

        return bounds.adjusted(-20.0, -20.0, 20.0, 20.0)

    def scene_to_widget_transform(self) -> QTransform:
        source = self.scene_bounds()
        destination = self.content_rect()

        scale = min(
            destination.width() / source.width(),
            destination.height() / source.height(),
        )

        rendered_width = source.width() * scale
        rendered_height = source.height() * scale
        offset_x = destination.left() + (destination.width() -
                                         rendered_width) / 2.0
        offset_y = destination.top() + (destination.height() -
                                        rendered_height) / 2.0

        transform = QTransform()
        transform.translate(offset_x, offset_y)
        transform.scale(scale, scale)
        transform.translate(-source.left(), -source.top())
        return transform

    def paintEvent(self, event) -> None:
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        painter.setPen(QPen(MINIMAP_BORDER, 1.0))
        painter.setBrush(QBrush(MINIMAP_BACKGROUND))
        painter.drawRoundedRect(
            self.rect().adjusted(0, 0, -1, -1),
            4.0,
            4.0,
        )

        transform = self.scene_to_widget_transform()
        painter.setTransform(transform)

        scene_pen_width = max(
            self.scene_bounds().width() / self.width(),
            self.scene_bounds().height() / self.height(),
        )

        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(QPen(MINIMAP_EDGE, scene_pen_width))

        for edge_item in self.graph_view.edge_items:
            painter.drawPath(edge_item.path())

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(MINIMAP_NODE))

        for node_item in self.graph_view.node_items:
            painter.drawRect(node_item.sceneBoundingRect())

        viewport_polygon = self.graph_view.mapToScene(
            self.graph_view.viewport().rect())
        viewport_rect = viewport_polygon.boundingRect()

        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(QPen(
            MINIMAP_VIEWPORT,
            scene_pen_width * 2.0,
        ))
        painter.drawRect(viewport_rect)

    def mousePressEvent(self, event) -> None:
        inverse, invertible = self.scene_to_widget_transform().inverted()

        if invertible:
            scene_position = inverse.map(event.position())
            self.graph_view.centerOn(scene_position)
            self.update()

        event.accept()

    def mouseMoveEvent(self, event) -> None:
        if event.buttons() & Qt.MouseButton.LeftButton:
            inverse, invertible = (self.scene_to_widget_transform().inverted())

            if invertible:
                scene_position = inverse.map(event.position())
                self.graph_view.centerOn(scene_position)
                self.update()

        event.accept()


class GraphView(QGraphicsView):
    nodeClicked = pyqtSignal(object)

    def __init__(self) -> None:
        scene = QGraphicsScene()
        super().__init__(scene)

        self.node_items: list[NodeItem] = []
        self.edge_items: list[EdgeItem] = []

        self.setRenderHints(QPainter.RenderHint.Antialiasing
                            | QPainter.RenderHint.TextAntialiasing
                            | QPainter.RenderHint.SmoothPixmapTransform)
        self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
        self.setTransformationAnchor(
            QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.ViewportAnchor.AnchorViewCenter)
        self.setBackgroundBrush(QBrush(QColor("#f3f4f6")))

        self.minimap = MinimapWidget(self)

        self.horizontalScrollBar().valueChanged.connect(self.minimap.update)
        self.verticalScrollBar().valueChanged.connect(self.minimap.update)

    def set_graph_items(
        self,
        node_items: list[NodeItem],
        edge_items: list[EdgeItem],
    ) -> None:
        self.scene().clear()

        self.node_items = node_items
        self.edge_items = edge_items

        for edge_item in edge_items:
            self.scene().addItem(edge_item)

        for node_item in node_items:
            self.scene().addItem(node_item)

        bounds = self.scene().itemsBoundingRect()
        self.scene().setSceneRect(bounds.adjusted(-100.0, -100.0, 100.0,
                                                  100.0))

        if not bounds.isNull():
            self.fitInView(
                bounds.adjusted(-30.0, -30.0, 30.0, 30.0),
                Qt.AspectRatioMode.KeepAspectRatio,
            )

        self.position_minimap()
        self.minimap.update()

    def position_minimap(self) -> None:
        margin = 12
        self.minimap.move(
            margin,
            self.height() - self.minimap.height() - margin,
        )
        self.minimap.raise_()

    def resizeEvent(self, event) -> None:
        super().resizeEvent(event)
        self.position_minimap()
        self.minimap.update()

    def scrollContentsBy(self, dx: int, dy: int) -> None:
        super().scrollContentsBy(dx, dy)
        self.minimap.update()

    def wheelEvent(self, event) -> None:
        if event.angleDelta().y() == 0:
            super().wheelEvent(event)
            return

        zoom_factor = 1.15
        if event.angleDelta().y() < 0:
            zoom_factor = 1.0 / zoom_factor

        current_scale = self.transform().m11()
        target_scale = current_scale * zoom_factor

        if 0.03 <= target_scale <= 20.0:
            self.scale(zoom_factor, zoom_factor)

        self.minimap.update()
        event.accept()


class PropertyPanel(QWidget):
    rankDirectionChanged = pyqtSignal(str)

    def __init__(self) -> None:
        super().__init__()

        layout = QVBoxLayout(self)

        title = QLabel("Graph configuration")
        title_font = QFont(title.font())
        title_font.setBold(True)
        title.setFont(title_font)
        layout.addWidget(title)

        form_layout = QFormLayout()
        self.rank_direction = QComboBox()
        self.rank_direction.addItems(["TB", "BT", "LR", "RL"])
        self.rank_direction.currentTextChanged.connect(
            self.rankDirectionChanged)
        form_layout.addRow("Rank direction", self.rank_direction)
        layout.addLayout(form_layout)

        properties_title = QLabel("Node properties")
        properties_font = QFont(properties_title.font())
        properties_font.setBold(True)
        properties_title.setFont(properties_font)
        layout.addWidget(properties_title)

        self.properties = QTableWidget(0, 2)
        self.properties.setHorizontalHeaderLabels(["Property", "Value"])
        self.properties.setEditTriggers(
            QAbstractItemView.EditTrigger.NoEditTriggers)
        self.properties.setSelectionMode(
            QAbstractItemView.SelectionMode.SingleSelection)
        self.properties.setWordWrap(True)
        self.properties.verticalHeader().setVisible(False)
        self.properties.horizontalHeader().setSectionResizeMode(
            0,
            QHeaderView.ResizeMode.ResizeToContents,
        )
        self.properties.horizontalHeader().setSectionResizeMode(
            1,
            QHeaderView.ResizeMode.Stretch,
        )
        layout.addWidget(self.properties, 1)

    def set_rank_direction(self, direction: str) -> None:
        index = self.rank_direction.findText(direction)

        if index < 0:
            index = self.rank_direction.findText("TB")

        self.rank_direction.blockSignals(True)
        self.rank_direction.setCurrentIndex(index)
        self.rank_direction.blockSignals(False)

    def show_node(self, model: NodeModel) -> None:
        rows = [("name", model.name)]
        rows.extend((
            key,
            display_dot_value(value),
        ) for key, value in sorted(model.attributes.items()))

        self.properties.setRowCount(len(rows))

        for row, (key, value) in enumerate(rows):
            self.properties.setItem(
                row,
                0,
                QTableWidgetItem(key),
            )
            self.properties.setItem(
                row,
                1,
                QTableWidgetItem(value),
            )

        self.properties.resizeRowsToContents()

    def clear_node(self) -> None:
        self.properties.setRowCount(0)


class GraphDocument:

    def __init__(self, graph: pydot.Dot) -> None:
        self.graph = graph
        self.nodes: list[NodeModel] = []
        self.edges: list[EdgeModel] = []
        self.rank_direction = unquote_dot_value(graph.get_attributes().get(
            "rankdir", "TB")).upper()

        if self.rank_direction not in {"TB", "BT", "LR", "RL"}:
            self.rank_direction = "TB"

        self.build_node_models()

    def build_node_models(self) -> None:
        raw_nodes = collect_graph_nodes(self.graph)
        raw_edges = collect_graph_edges(self.graph)

        node_attributes: dict[str, dict[str, str]] = {}
        node_order: list[str] = []

        for raw_node in raw_nodes:
            name = normalize_node_name(raw_node.get_name())

            if name in {"", "graph", "node", "edge", r"\n"}:
                continue

            if name not in node_attributes:
                node_order.append(name)
                node_attributes[name] = {}

            node_attributes[name].update({
                key: str(value)
                for key, value in raw_node.get_attributes().items()
            })

        for raw_edge in raw_edges:
            source = normalize_node_name(raw_edge.get_source())
            destination = normalize_node_name(raw_edge.get_destination())

            for name in (source, destination):
                if name and name not in node_attributes:
                    node_order.append(name)
                    node_attributes[name] = {}

        self.nodes.clear()

        for index, name in enumerate(node_order):
            attributes = node_attributes[name]
            qt_html = graphviz_label_to_qt_html(attributes.get("label", name))
            document = create_text_document(
                qt_html,
                NODE_WIDTH - NODE_PADDING * 2.0,
            )
            height = max(
                NODE_MINIMUM_HEIGHT,
                document.size().height() + NODE_PADDING * 2.0,
            )

            self.nodes.append(
                NodeModel(
                    layout_id=f"n{index}",
                    name=name,
                    attributes=attributes,
                    qt_html=qt_html,
                    width=NODE_WIDTH,
                    height=height,
                ))

    def compute_layout(self, rank_direction: str) -> None:
        node_by_name = {node.name: node for node in self.nodes}

        temporary = pydot.Dot(
            graph_type=self.graph.get_type(),
            strict=False,
        )
        temporary.set_rankdir(rank_direction)
        temporary.set("outputorder", "edgesfirst")

        source_graph_attributes = self.graph.get_attributes()
        for key in ("ranksep", "nodesep", "concentrate"):
            if key in source_graph_attributes:
                temporary.set(key, source_graph_attributes[key])

        for node in self.nodes:
            temporary.add_node(
                pydot.Node(
                    node.layout_id,
                    label="",
                    shape="box",
                    fixedsize="true",
                    width=f"{node.width / LAYOUT_DPI:.6f}",
                    height=f"{node.height / LAYOUT_DPI:.6f}",
                ))

        raw_edges = collect_graph_edges(self.graph)
        edge_attributes_by_pair: dict[tuple[str, str], list[dict[str,
                                                                 str]]] = {}

        for raw_edge in raw_edges:
            source_name = normalize_node_name(raw_edge.get_source())
            destination_name = normalize_node_name(raw_edge.get_destination())

            source = node_by_name.get(source_name)
            destination = node_by_name.get(destination_name)

            if source is None or destination is None:
                continue

            source_attributes = {
                key: str(value)
                for key, value in raw_edge.get_attributes().items()
            }
            layout_attributes = {
                key: value
                for key, value in source_attributes.items()
                if key in {"constraint", "minlen", "weight"}
            }

            temporary.add_edge(
                pydot.Edge(
                    source.layout_id,
                    destination.layout_id,
                    **layout_attributes,
                ))

            key = (source.layout_id, destination.layout_id)
            edge_attributes_by_pair.setdefault(key,
                                               []).append(source_attributes)

        plain_data = temporary.create(
            format="plain",
            prog="dot",
        )
        plain_text = plain_data.decode("utf-8")
        positions, layout_edges = parse_plain_layout(plain_text)

        for node in self.nodes:
            node.center = positions.get(
                node.layout_id,
                QPointF(),
            )

        edge_pair_indices: dict[tuple[str, str], int] = {}
        self.edges.clear()

        for source_id, destination_id, points in layout_edges:
            key = (source_id, destination_id)
            pair_index = edge_pair_indices.get(key, 0)
            pair_attributes = edge_attributes_by_pair.get(key, [])
            attributes = (pair_attributes[pair_index]
                          if pair_index < len(pair_attributes) else {})
            edge_pair_indices[key] = pair_index + 1

            self.edges.append(
                EdgeModel(
                    source_id=source_id,
                    destination_id=destination_id,
                    attributes=attributes,
                    points=points,
                ))


class MainWindow(QMainWindow):

    def __init__(self, initial_path: Path | None) -> None:
        super().__init__()

        self.setWindowTitle("Graphviz Qt Viewer")
        self.resize(1500, 900)

        self.document: GraphDocument | None = None
        self.current_path: Path | None = None

        self.property_panel = PropertyPanel()
        self.graph_view = GraphView()

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self.property_panel)
        splitter.addWidget(self.graph_view)
        splitter.setSizes([350, 1150])
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)
        self.setCentralWidget(splitter)

        self.property_panel.rankDirectionChanged.connect(
            self.set_rank_direction)
        self.graph_view.nodeClicked.connect(self.property_panel.show_node)

        open_action = QAction("&Open…", self)
        open_action.setShortcut("Ctrl+O")
        open_action.triggered.connect(self.open_graph_dialog)

        quit_action = QAction("&Quit", self)
        quit_action.setShortcut("Ctrl+Q")
        quit_action.triggered.connect(self.close)

        file_menu = self.menuBar().addMenu("&File")
        file_menu.addAction(open_action)
        file_menu.addSeparator()
        file_menu.addAction(quit_action)

        if initial_path is not None:
            self.load_graph(initial_path)

    def open_graph_dialog(self) -> None:
        path, _ = QFileDialog.getOpenFileName(
            self,
            "Open Graphviz graph",
            str(self.current_path.parent if self.current_path else Path.cwd()),
            "Graphviz DOT files (*.dot *.gv);;All files (*)",
        )

        if path:
            self.load_graph(Path(path))

    def load_graph(self, path: Path) -> None:
        try:
            dot_data = path.read_text(encoding="utf-8")
            parsed_graphs = pydot.graph_from_dot_data(dot_data)

            if not parsed_graphs:
                raise ValueError("The input did not contain a graph")

            graph = parsed_graphs[0]

            if not isinstance(graph, pydot.Dot):
                raise TypeError("pydot did not return a pydot.Dot graph")

            document = GraphDocument(graph)
            document.compute_layout(document.rank_direction)
        except Exception as error:
            QMessageBox.critical(
                self,
                "Unable to load graph",
                str(error),
            )
            return

        self.document = document
        self.current_path = path
        self.property_panel.set_rank_direction(document.rank_direction)
        self.property_panel.clear_node()
        self.render_document()

        self.setWindowTitle(f"Graphviz Qt Viewer — {path.name}")

    def set_rank_direction(self, direction: str) -> None:
        if self.document is None:
            return

        QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)

        try:
            self.document.compute_layout(direction)
            self.document.rank_direction = direction
            self.property_panel.clear_node()
            self.render_document()
        except Exception as error:
            QMessageBox.critical(
                self,
                "Unable to compute layout",
                str(error),
            )
        finally:
            QApplication.restoreOverrideCursor()

    def render_document(self) -> None:
        if self.document is None:
            return

        node_items: list[NodeItem] = []
        edge_items: list[EdgeItem] = []

        for edge in self.document.edges:
            edge_items.append(EdgeItem(
                edge.points,
                edge.attributes,
            ))

        for node in self.document.nodes:
            item = NodeItem(node)
            item.setPos(
                node.center.x() - node.width / 2.0,
                node.center.y() - node.height / 2.0,
            )
            node_items.append(item)

        self.graph_view.set_graph_items(
            node_items,
            edge_items,
        )


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Interactive PyQt6 Graphviz graph viewer")
    parser.add_argument(
        "graph",
        nargs="?",
        type=Path,
        help="Graphviz DOT input file",
    )
    return parser.parse_args()


def main() -> int:
    arguments = parse_arguments()

    application = QApplication(sys.argv)
    application.setApplicationName("Graphviz Qt Viewer")

    window = MainWindow(arguments.graph)
    window.show()

    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main())

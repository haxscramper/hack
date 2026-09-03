#!/usr/bin/env python

from __future__ import annotations

import math
from collections.abc import Callable

from PyQt6.QtCore import QModelIndex, QPointF, QRectF, Qt, pyqtSignal
from PyQt6.QtGui import (
    QBrush,
    QColor,
    QPainter,
    QPainterPath,
    QPen,
    QPolygonF,
    QTextDocument,
    QTextOption,
    QTransform,
)
from PyQt6.QtWidgets import (
    QGraphicsItem,
    QGraphicsPathItem,
    QGraphicsPolygonItem,
    QGraphicsRectItem,
    QGraphicsScene,
    QGraphicsView,
    QStyle,
    QWidget,
)

from graphviz_viewer.graph_viewer_v2.constants import NODE_PADDING
from graphviz_viewer.graph_viewer_v2.model import GraphLayoutModel, GraphRole


def create_document(
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
    return document


class NodeItem(QGraphicsItem):

    def __init__(
        self,
        index: QModelIndex,
        selected: Callable[[QModelIndex], None],
    ) -> None:
        super().__init__()
        self.index = QModelIndex(index)
        self.selected_callback = selected
        self.rectangle = index.data(GraphRole.Geometry)
        self.style = index.data(GraphRole.Style)
        self.document = create_document(
            index.data(GraphRole.RichText),
            self.rectangle.width() - NODE_PADDING * 2.0,
        )

        self.setFlag(
            QGraphicsItem.GraphicsItemFlag.ItemIsSelectable,
            True,
        )
        self.setAcceptedMouseButtons(Qt.MouseButton.LeftButton)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setZValue(10.0)

    def boundingRect(self) -> QRectF:
        return QRectF(
            0.0,
            0.0,
            self.rectangle.width(),
            self.rectangle.height(),
        )

    def paint(self, painter, option, widget=None) -> None:
        selected = bool(option.state & QStyle.StateFlag.State_Selected)
        border = (self.style["selected-border"]
                  if selected else self.style["border"])

        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setBrush(QColor(self.style["background"]))
        painter.setPen(QPen(QColor(border), 2.5 if selected else 1.0))
        painter.drawRoundedRect(self.boundingRect(), 3.0, 3.0)

        painter.save()
        painter.translate(NODE_PADDING, NODE_PADDING)
        clip = QRectF(
            0.0,
            0.0,
            self.rectangle.width() - NODE_PADDING * 2.0,
            self.rectangle.height() - NODE_PADDING * 2.0,
        )
        painter.setClipRect(clip)
        self.document.drawContents(painter, clip)
        painter.restore()

    def mousePressEvent(self, event) -> None:
        if self.scene() is not None:
            self.scene().clearSelection()

        self.setSelected(True)
        self.selected_callback(self.index)
        event.accept()


class EdgeItem(QGraphicsPathItem):

    def __init__(
        self,
        points: list[QPointF],
        color: QColor,
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

        pen = QPen(color, 1.5)
        pen.setCosmetic(True)
        self.setPen(pen)
        self.setPath(path)
        self.setZValue(-10.0)

        if len(points) >= 2:
            self._add_arrowhead(points, color)

    def _add_arrowhead(
        self,
        points: list[QPointF],
        color: QColor,
    ) -> None:
        end = points[-1]
        direction = end - points[-2]
        length = math.hypot(direction.x(), direction.y())

        if length == 0.0:
            return

        unit_x = direction.x() / length
        unit_y = direction.y() / length
        base = QPointF(
            end.x() - unit_x * 12.0,
            end.y() - unit_y * 12.0,
        )
        perpendicular = QPointF(-unit_y * 5.0, unit_x * 5.0)
        polygon = QPolygonF([end, base + perpendicular, base - perpendicular])

        arrow = QGraphicsPolygonItem(polygon, self)
        arrow.setPen(QPen(color, 1.0))
        arrow.setBrush(color)


class MinimapWidget(QWidget):

    def __init__(self, graph_view: GraphView) -> None:
        super().__init__(graph_view)
        self.graph_view = graph_view
        self.setFixedSize(220, 150)
        self.setCursor(Qt.CursorShape.CrossCursor)

    def content_rect(self) -> QRectF:
        return QRectF(
            8.0,
            8.0,
            self.width() - 16.0,
            self.height() - 16.0,
        )

    def scene_bounds(self) -> QRectF:
        bounds = self.graph_view.scene().itemsBoundingRect()

        if bounds.isNull():
            return QRectF(-1.0, -1.0, 2.0, 2.0)

        return bounds.adjusted(-20.0, -20.0, 20.0, 20.0)

    def transform_for_scene(self) -> QTransform:
        source = self.scene_bounds()
        destination = self.content_rect()
        scale = min(
            destination.width() / source.width(),
            destination.height() / source.height(),
        )
        width = source.width() * scale
        height = source.height() * scale

        transform = QTransform()
        transform.translate(
            destination.left() + (destination.width() - width) / 2.0,
            destination.top() + (destination.height() - height) / 2.0,
        )
        transform.scale(scale, scale)
        transform.translate(-source.left(), -source.top())
        return transform

    def paintEvent(self, event) -> None:
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setPen(QColor("#646b73"))
        painter.setBrush(QColor(245, 247, 250, 235))
        painter.drawRoundedRect(
            self.rect().adjusted(0, 0, -1, -1),
            4.0,
            4.0,
        )
        painter.setTransform(self.transform_for_scene())

        scale_width = self.scene_bounds().width() / self.width()
        painter.setPen(QPen(QColor("#9aa0a6"), scale_width))
        painter.setBrush(Qt.BrushStyle.NoBrush)

        for edge in self.graph_view.edge_items:
            painter.drawPath(edge.mapToScene(edge.path()))

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QColor("#59636e"))

        for node in self.graph_view.node_items:
            painter.drawRect(node.sceneBoundingRect())

        viewport = self.graph_view.mapToScene(
            self.graph_view.viewport().rect()).boundingRect()
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(QPen(QColor("#d32f2f"), scale_width * 2.0))
        painter.drawRect(viewport)

    def mousePressEvent(self, event) -> None:
        inverse, valid = self.transform_for_scene().inverted()

        if valid:
            self.graph_view.centerOn(inverse.map(event.position()))

        event.accept()

    def mouseMoveEvent(self, event) -> None:
        if event.buttons() & Qt.MouseButton.LeftButton:
            self.mousePressEvent(event)


class GraphView(QGraphicsView):
    elementSelected = pyqtSignal(QModelIndex)

    def __init__(self) -> None:
        super().__init__(QGraphicsScene())
        self.model: GraphLayoutModel | None = None
        self.node_items: list[NodeItem] = []
        self.edge_items: list[EdgeItem] = []

        self.setRenderHints(QPainter.RenderHint.Antialiasing
                            | QPainter.RenderHint.TextAntialiasing
                            | QPainter.RenderHint.SmoothPixmapTransform)
        self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
        self.setTransformationAnchor(
            QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.ViewportAnchor.AnchorViewCenter)
        self.setBackgroundBrush(QColor("#f3f4f6"))

        self.minimap = MinimapWidget(self)
        self.horizontalScrollBar().valueChanged.connect(self.minimap.update)
        self.verticalScrollBar().valueChanged.connect(self.minimap.update)

    def set_model(self, model: GraphLayoutModel) -> None:
        self.model = model
        self.scene().clear()
        self.node_items.clear()
        self.edge_items.clear()

        root_index = model.index(0, 0)
        self._create_items(root_index, QPointF())

        bounds = self.scene().itemsBoundingRect()
        self.scene().setSceneRect(bounds.adjusted(-100.0, -100.0, 100.0,
                                                  100.0))

        if not bounds.isNull():
            self.fitInView(
                bounds.adjusted(-30.0, -30.0, 30.0, 30.0),
                Qt.AspectRatioMode.KeepAspectRatio,
            )

        self._position_minimap()
        self.minimap.update()

    def _create_items(
        self,
        index: QModelIndex,
        parent_origin: QPointF,
    ) -> None:
        kind = index.data(GraphRole.ElementKind)
        geometry = index.data(GraphRole.Geometry)
        origin = parent_origin

        if kind == "cluster":
            origin = parent_origin + geometry.topLeft()
            style = index.data(GraphRole.Style)
            item = QGraphicsRectItem(QRectF(QPointF(), geometry.size()))
            item.setPos(origin)
            item.setPen(QPen(QColor(style["border"]), 1.0))
            item.setBrush(QBrush(QColor(style["background"])))
            item.setZValue(-20.0)
            self.scene().addItem(item)

        elif kind == "node":
            item = NodeItem(index, self.elementSelected.emit)
            item.setPos(parent_origin + geometry.topLeft())
            self.scene().addItem(item)
            self.node_items.append(item)

        elif kind == "edge":
            points = [
                parent_origin + point
                for point in index.data(GraphRole.EdgePoints)
            ]
            style = index.data(GraphRole.Style)
            item = EdgeItem(points, QColor(style["color"]))
            self.scene().addItem(item)
            self.edge_items.append(item)

        for row in range(self.model.rowCount(index)):
            self._create_items(self.model.index(row, 0, index), origin)

    def _position_minimap(self) -> None:
        margin = 12
        self.minimap.move(
            margin,
            self.height() - self.minimap.height() - margin,
        )
        self.minimap.raise_()

    def resizeEvent(self, event) -> None:
        super().resizeEvent(event)
        self._position_minimap()
        self.minimap.update()

    def scrollContentsBy(self, dx: int, dy: int) -> None:
        super().scrollContentsBy(dx, dy)
        self.minimap.update()

    def wheelEvent(self, event) -> None:
        if event.angleDelta().y() == 0:
            super().wheelEvent(event)
            return

        factor = 1.15 if event.angleDelta().y() > 0 else 1.0 / 1.15
        target = self.transform().m11() * factor

        if 0.03 <= target <= 20.0:
            self.scale(factor, factor)

        self.minimap.update()
        event.accept()

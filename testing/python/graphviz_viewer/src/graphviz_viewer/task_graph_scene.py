from __future__ import annotations

from beartype.typing import Optional
from PyQt6.QtCore import QModelIndex, QPointF, QRectF, Qt, pyqtSignal
from PyQt6.QtGui import (
    QColor,
    QMouseEvent,
    QPainter,
    QPainterPath,
    QPen,
    QTextDocument,
    QWheelEvent,
)
from PyQt6.QtWidgets import (
    QGraphicsObject,
    QGraphicsPathItem,
    QGraphicsScene,
    QGraphicsView,
    QHBoxLayout,
    QHeaderView,
    QSplitter,
    QStyleOptionGraphicsItem,
    QTableView,
    QVBoxLayout,
    QWidget,
)

from graphviz_viewer.task_graph_model import PropertyModel, TaskGraphModel, TaskGraphRole
from graphviz_viewer.task_graph_types import ElementKind, Point, Rect


def qrect(rect: Rect) -> QRectF:
    return QRectF(rect.x, rect.y, rect.width, rect.height)


class SelectableGroupItem(QGraphicsObject):

    def __init__(
        self,
        geometry: Rect,
        label: str,
        index: QModelIndex,
        color: QColor,
    ) -> None:
        super().__init__()
        self.geometry = geometry
        self.label = label
        self.index = index
        self.color = color
        self.setPos(geometry.x, geometry.y)
        self.setZValue(-10.0)
        self.setAcceptedMouseButtons(Qt.MouseButton.LeftButton)

    def boundingRect(self) -> QRectF:
        return QRectF(0.0, 0.0, self.geometry.width, self.geometry.height)

    def shape(self) -> QPainterPath:
        outer = QPainterPath()
        outer.addRoundedRect(self.boundingRect(), 10.0, 10.0)
        inner = QPainterPath()
        inner.addRoundedRect(
            self.boundingRect().adjusted(5.0, 5.0, -5.0, -5.0),
            7.0,
            7.0,
        )
        return outer.subtracted(inner)

    def paint(
        self,
        painter: QPainter,
        option: QStyleOptionGraphicsItem,
        widget: Optional[QWidget] = None,
    ) -> None:
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(QPen(self.color, 2.0))
        painter.drawRoundedRect(self.boundingRect(), 10.0, 10.0)
        painter.setPen(self.color)
        painter.drawText(
            QRectF(12.0, 4.0, self.geometry.width - 24.0, 24.0),
            int(Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter),
            self.label,
        )

    def mousePressEvent(self, event) -> None:
        scene = self.scene()
        if isinstance(scene, TaskGraphicsScene):
            scene.element_selected.emit(self.index)
        event.accept()


class TaskNodeItem(QGraphicsObject):

    def __init__(
        self,
        geometry: Rect,
        rich_text: str,
        index: QModelIndex,
        color: QColor,
        calendar_mode: bool,
    ) -> None:
        super().__init__()
        self.geometry = geometry
        self.rich_text = rich_text
        self.index = index
        self.color = color
        self.calendar_mode = calendar_mode
        self.document = QTextDocument()
        self.document.setDocumentMargin(0.0)
        self.document.setHtml(rich_text)
        self.document.setTextWidth(max(1.0, geometry.width - 28.0))
        self.setPos(geometry.x, geometry.y)
        self.setZValue(5.0)
        self.setAcceptedMouseButtons(Qt.MouseButton.LeftButton)

    def boundingRect(self) -> QRectF:
        return QRectF(0.0, 0.0, self.geometry.width, self.geometry.height)

    def paint(
        self,
        painter: QPainter,
        option: QStyleOptionGraphicsItem,
        widget: Optional[QWidget] = None,
    ) -> None:
        base = self.color.lighter(175)
        base.setAlpha(235)
        painter.setBrush(base)
        painter.setPen(QPen(self.color, 3.0))
        painter.drawRoundedRect(
            self.boundingRect().adjusted(1.5, 1.5, -1.5, -1.5),
            9.0,
            9.0,
        )

        content = self.boundingRect().adjusted(14.0, 9.0, -14.0, -9.0)
        painter.save()
        painter.setClipRect(content)
        painter.translate(content.topLeft())
        self.document.drawContents(
            painter,
            QRectF(0.0, 0.0, content.width(), content.height()),
        )
        painter.restore()

    def mousePressEvent(self, event) -> None:
        scene = self.scene()
        if isinstance(scene, TaskGraphicsScene):
            scene.element_selected.emit(self.index)
        event.accept()


class TaskEdgeItem(QGraphicsPathItem):

    def __init__(
        self,
        points: list[Point],
        index: QModelIndex,
        color: QColor,
    ) -> None:
        super().__init__()
        self.index = index
        path = QPainterPath()
        if points:
            path.moveTo(points[0].x, points[0].y)
            for point in points[1:]:
                path.lineTo(point.x, point.y)

        self.setPath(path)
        self.setPen(QPen(color, 3.0))
        self.setZValue(1.0)
        self.setAcceptedMouseButtons(Qt.MouseButton.LeftButton)

    def shape(self) -> QPainterPath:
        stroker = QPainterPath()
        pen = QPen(self.pen())
        pen.setWidthF(12.0)
        stroker.addPath(self.path())
        return super().shape().united(stroker)

    def mousePressEvent(self, event) -> None:
        scene = self.scene()
        if isinstance(scene, TaskGraphicsScene):
            scene.element_selected.emit(self.index)
        event.accept()


class TaskGraphicsScene(QGraphicsScene):
    element_selected = pyqtSignal(object)

    def __init__(
        self,
        model: TaskGraphModel,
        calendar_mode: bool,
    ) -> None:
        super().__init__()
        self.model = model
        self.calendar_mode = calendar_mode
        self.populate(QModelIndex())
        self.setSceneRect(self.itemsBoundingRect().adjusted(
            -30.0, -30.0, 30.0, 30.0))

    def populate(self, parent: QModelIndex) -> None:
        for row in range(self.model.rowCount(parent)):
            index = self.model.index(row, 0, parent)
            kind = index.data(int(TaskGraphRole.ELEMENT_KIND))
            color = QColor(index.data(int(TaskGraphRole.COLOR)))

            match kind:
                case ElementKind.GROUP:
                    geometry = index.data(int(TaskGraphRole.ABSOLUTE_GEOMETRY))
                    item = SelectableGroupItem(
                        geometry,
                        str(index.data(int(Qt.ItemDataRole.DisplayRole))),
                        index,
                        color,
                    )
                    self.addItem(item)
                    self.populate(index)
                case ElementKind.NODE:
                    geometry = index.data(int(TaskGraphRole.ABSOLUTE_GEOMETRY))
                    item = TaskNodeItem(
                        geometry,
                        index.data(int(TaskGraphRole.RICH_TEXT)),
                        index,
                        color,
                        self.calendar_mode,
                    )
                    self.addItem(item)
                case ElementKind.EDGE:
                    points = index.data(int(TaskGraphRole.EDGE_POINTS))
                    item = TaskEdgeItem(points, index, color)
                    self.addItem(item)


class TaskGraphicsView(QGraphicsView):
    viewport_changed = pyqtSignal()

    def __init__(self, scene: TaskGraphicsScene) -> None:
        super().__init__(scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing, True)
        self.setRenderHint(QPainter.RenderHint.TextAntialiasing, True)
        self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
        self.setTransformationAnchor(
            QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.horizontalScrollBar().valueChanged.connect(self.viewport_changed)
        self.verticalScrollBar().valueChanged.connect(self.viewport_changed)

    def wheelEvent(self, event: QWheelEvent) -> None:
        factor = 1.18 if 0 < event.angleDelta().y() else 1.0 / 1.18
        self.scale(factor, factor)
        self.viewport_changed.emit()
        event.accept()


class Minimap(QWidget):

    def __init__(self, view: TaskGraphicsView) -> None:
        super().__init__(view)
        self.view = view
        self.setFixedSize(240, 160)
        self.setAutoFillBackground(True)
        self.view.viewport_changed.connect(self.update)
        self.setCursor(Qt.CursorShape.CrossCursor)

    def scene_to_widget(self, point: QPointF) -> QPointF:
        scene_rect = self.view.sceneRect()
        scale = min(
            self.width() / scene_rect.width(),
            self.height() / scene_rect.height(),
        )
        offset_x = (self.width() - scene_rect.width() * scale) / 2.0
        offset_y = (self.height() - scene_rect.height() * scale) / 2.0
        return QPointF(
            offset_x + (point.x() - scene_rect.left()) * scale,
            offset_y + (point.y() - scene_rect.top()) * scale,
        )

    def widget_to_scene(self, point: QPointF) -> QPointF:
        scene_rect = self.view.sceneRect()
        scale = min(
            self.width() / scene_rect.width(),
            self.height() / scene_rect.height(),
        )
        offset_x = (self.width() - scene_rect.width() * scale) / 2.0
        offset_y = (self.height() - scene_rect.height() * scale) / 2.0
        return QPointF(
            scene_rect.left() + (point.x() - offset_x) / scale,
            scene_rect.top() + (point.y() - offset_y) / scale,
        )

    def paintEvent(self, event) -> None:
        painter = QPainter(self)
        painter.fillRect(self.rect(), QColor("white"))
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)

        for item in self.view.scene().items():
            if isinstance(item, SelectableGroupItem):
                rectangle = item.sceneBoundingRect()
                top_left = self.scene_to_widget(rectangle.topLeft())
                bottom_right = self.scene_to_widget(rectangle.bottomRight())
                painter.setBrush(Qt.BrushStyle.NoBrush)
                painter.setPen(QPen(QColor("#9a9a9a"), 1.0))
                painter.drawRect(QRectF(top_left, bottom_right))
            elif isinstance(item, TaskNodeItem):
                rectangle = item.sceneBoundingRect()
                top_left = self.scene_to_widget(rectangle.topLeft())
                bottom_right = self.scene_to_widget(rectangle.bottomRight())
                painter.setPen(Qt.PenStyle.NoPen)
                painter.setBrush(item.color)
                painter.drawRect(QRectF(top_left, bottom_right))

        viewport_rect = self.view.mapToScene(
            self.view.viewport().rect()).boundingRect()
        top_left = self.scene_to_widget(viewport_rect.topLeft())
        bottom_right = self.scene_to_widget(viewport_rect.bottomRight())
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(QPen(QColor("#202020"), 2.0))
        painter.drawRect(QRectF(top_left, bottom_right))

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            self.view.centerOn(self.widget_to_scene(event.position()))
            self.update()
            event.accept()


class ViewContainer(QWidget):

    def __init__(self, view: TaskGraphicsView) -> None:
        super().__init__()
        self.view = view
        self.minimap = Minimap(view)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(view)

    def resizeEvent(self, event) -> None:
        margin = 12
        self.minimap.move(
            margin,
            self.height() - self.minimap.height() - margin,
        )
        self.minimap.raise_()
        super().resizeEvent(event)


class VisualizationPanel(QWidget):

    def __init__(
        self,
        model: TaskGraphModel,
        calendar_mode: bool,
    ) -> None:
        super().__init__()
        self.property_model = PropertyModel(model)
        self.property_view = QTableView()
        self.property_view.setModel(self.property_model)
        self.property_view.setEditTriggers(
            QTableView.EditTrigger.NoEditTriggers)
        self.property_view.horizontalHeader().setSectionResizeMode(
            0,
            QHeaderView.ResizeMode.ResizeToContents,
        )
        self.property_view.horizontalHeader().setSectionResizeMode(
            1,
            QHeaderView.ResizeMode.Stretch,
        )

        self.scene = TaskGraphicsScene(model, calendar_mode)
        self.scene.element_selected.connect(self.property_model.set_selected)
        self.graphics_view = TaskGraphicsView(self.scene)
        self.graphics_view.fitInView(
            self.scene.sceneRect(),
            Qt.AspectRatioMode.KeepAspectRatio,
        )

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self.property_view)
        splitter.addWidget(ViewContainer(self.graphics_view))
        splitter.setSizes([320, 1100])

        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(splitter)

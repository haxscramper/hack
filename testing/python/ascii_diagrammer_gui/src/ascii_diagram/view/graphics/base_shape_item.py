from __future__ import annotations

from typing import Callable, Optional

from PySide6.QtCore import QPointF, Qt
from PySide6.QtGui import QBrush, QColor, QPen
from PySide6.QtWidgets import (
    QGraphicsItem,
    QGraphicsPathItem,
    QGraphicsSceneMouseEvent,
)

from ascii_diagram.model.scene_model import SceneModel, QModelIndex


class BaseShapeItem(QGraphicsPathItem):

    def __init__(self, model_index: QModelIndex, model: SceneModel):
        super().__init__()
        self._model_index = model_index
        self._model = model
        self._dragging = False
        self._drag_start_pos: Optional[QPointF] = None
        self._moved_callback: Optional[Callable[[], None]] = None
        self.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, True)
        self.setFlag(QGraphicsItem.GraphicsItemFlag.ItemSendsGeometryChanges,
                     True)
        self.setAcceptHoverEvents(True)
        self.setPen(QPen(QColor(50, 100, 200), 1.5))
        self.setBrush(QBrush(QColor(180, 200, 255, 80)))
        self.setZValue(10)

        selected_pen = QPen(QColor(50, 130, 255), 2.0)
        self._selected_pen = selected_pen

    def set_moved_callback(self, cb: Callable[[], None]) -> None:
        self._moved_callback = cb

    def model_index(self) -> QModelIndex:
        return self._model_index

    def set_model_index(self, idx: QModelIndex) -> None:
        self._model_index = idx

    def model(self) -> SceneModel:
        return self._model

    def update_geometry(self) -> None:
        raise NotImplementedError

    def mousePressEvent(self, event: QGraphicsSceneMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton:
            self._dragging = True
            self._drag_start_pos = event.scenePos()
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event: QGraphicsSceneMouseEvent) -> None:
        if self._dragging and self._drag_start_pos is not None:
            delta = event.scenePos() - self._drag_start_pos
            dx = round(delta.x())
            dy = round(delta.y())
            if dx != 0 or dy != 0:
                self._drag_start_pos = event.scenePos()
                item = self._model.item_from_index(self._model_index)
                if item:
                    item.properties.x += dx
                    item.properties.y += dy
                    self._model.dataChanged.emit(self._model_index,
                                                 self._model_index)
                    self.update_geometry()
                    if self._moved_callback:
                        self._moved_callback()
        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event: QGraphicsSceneMouseEvent) -> None:
        self._dragging = False
        self._drag_start_pos = None
        super().mouseReleaseEvent(event)

    def itemChange(self, change, value):
        if change == QGraphicsItem.GraphicsItemChange.ItemSelectedHasChanged:
            if value:
                self.setPen(self._selected_pen)
            else:
                self.setPen(QPen(QColor(50, 100, 200), 1.5))
        return super().itemChange(change, value)

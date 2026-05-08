from __future__ import annotations

from typing import Dict, Optional

from PySide6.QtCore import (
    QEvent,
    QModelIndex,
    QPoint,
    QPointF,
    Qt,
    Signal,
)
from PySide6.QtGui import (
    QBrush,
    QColor,
    QDragEnterEvent,
    QDragMoveEvent,
    QDropEvent,
    QFont,
    QFontMetrics,
    QMouseEvent,
    QPainter,
    QPen,
    QWheelEvent,
)
from PySide6.QtWidgets import (
    QGraphicsItem,
    QGraphicsScene,
    QGraphicsView,
)

from ascii_diagram.model.enums import ShapeType
from ascii_diagram.model.scene_model import SceneModel
from ascii_diagram.view.graphics.base_shape_item import BaseShapeItem
from ascii_diagram.view.graphics.raster_overlay import RasterOverlayItem
from ascii_diagram.view.graphics.shape_items import make_shape_item
from ascii_diagram.interaction.drag_drop import decode_palette_mime, PALETTE_MIME

_GRID_FONT = QFont("Courier New", 12)
_GRID_FONT.setStyleHint(QFont.Monospace)


class SceneView(QGraphicsView):
    selection_updated = Signal()

    def __init__(self, model: SceneModel, parent=None):
        super().__init__(parent)
        self._model = model
        self._scene = QGraphicsScene(self)
        self.setScene(self._scene)

        self._raster_overlay = RasterOverlayItem(model, self._scene)
        self._scene.addItem(self._raster_overlay)

        self._item_map: Dict[int, BaseShapeItem] = {}
        self._shape_type_map: Dict[int, ShapeType] = {}

        self.setRenderHint(QPainter.Antialiasing, False)
        self.setDragMode(QGraphicsView.RubberBandDrag)
        self.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.AnchorUnderMouse)
        self.setAcceptDrops(True)
        self.setViewportUpdateMode(QGraphicsView.FullViewportUpdate)
        self.setBackgroundBrush(QBrush(QColor(255, 255, 255)))

        fm = QFontMetrics(_GRID_FONT)
        self._cell_width = fm.averageCharWidth()
        self._cell_height = fm.height()

        self._draw_grid_lines = False
        self._panning = False
        self._pan_start = QPoint()

        self._scene.selectionChanged.connect(self._on_scene_selection_changed)

        self._model.modelAboutToBeReset.connect(self._on_model_reset)
        self._model.modelReset.connect(self._rebuild_all)
        self._model.rowsInserted.connect(self._on_rows_inserted)
        self._model.rowsRemoved.connect(self._on_rows_removed)
        self._model.rowsMoved.connect(self._on_rows_moved)
        self._model.dataChanged.connect(self._on_data_changed)

        self._rebuild_all()

    def cell_size(self) -> tuple[float, float]:
        return (self._cell_width, self._cell_height)

    def _on_model_reset(self) -> None:
        for item in self._item_map.values():
            self._scene.removeItem(item)
        self._item_map.clear()
        self._shape_type_map.clear()

    def _rebuild_all(self) -> None:
        for item in self._item_map.values():
            self._scene.removeItem(item)
        self._item_map.clear()
        self._shape_type_map.clear()

        self._raster_overlay.rebuild()

        def add_items(parent_index: QModelIndex) -> None:
            for row in range(self._model.rowCount(parent_index)):
                idx = self._model.index(row, 0, parent_index)
                shape = make_shape_item(self._model, idx)
                if shape:
                    item_id = idx.data(Qt.UserRole + 103)
                    self._item_map[item_id] = shape
                    st = idx.data(Qt.UserRole + 102)
                    if isinstance(st, ShapeType):
                        self._shape_type_map[item_id] = st
                    shape.set_moved_callback(self._on_item_moved)
                    self._scene.addItem(shape)
                    add_items(idx)

        add_items(QModelIndex())

    def _on_rows_inserted(self, parent: QModelIndex, first: int,
                          last: int) -> None:
        for row in range(first, last + 1):
            idx = self._model.index(row, 0, parent)
            shape = make_shape_item(self._model, idx)
            if shape:
                item_id = idx.data(Qt.UserRole + 103)
                self._item_map[item_id] = shape
                st = idx.data(Qt.UserRole + 102)
                if isinstance(st, ShapeType):
                    self._shape_type_map[item_id] = st
                shape.set_moved_callback(self._on_item_moved)
                self._scene.addItem(shape)
            add_children = self._model.index(row, 0, parent)
            if add_children.isValid():
                self._add_child_items(add_children)
        self._raster_overlay.rebuild()

    def _add_child_items(self, parent_idx: QModelIndex) -> None:
        for row in range(self._model.rowCount(parent_idx)):
            idx = self._model.index(row, 0, parent_idx)
            shape = make_shape_item(self._model, idx)
            if shape:
                item_id = idx.data(Qt.UserRole + 103)
                self._item_map[item_id] = shape
                shape.set_moved_callback(self._on_item_moved)
                self._scene.addItem(shape)
            self._add_child_items(idx)

    def _on_rows_removed(self, parent: QModelIndex, first: int,
                         last: int) -> None:
        for item_id, shape in list(self._item_map.items()):
            if not shape.model_index().isValid():
                self._scene.removeItem(shape)
                del self._item_map[item_id]
                self._shape_type_map.pop(item_id, None)

        self._raster_overlay.rebuild()

    def _on_rows_moved(
        self,
        source_parent: QModelIndex,
        source_start: int,
        source_end: int,
        dest_parent: QModelIndex,
        dest_row: int,
    ) -> None:
        self._rebuild_all()

    def _on_data_changed(self, top_left: QModelIndex,
                         bottom_right: QModelIndex) -> None:
        item_id = top_left.data(Qt.UserRole + 103)
        if item_id is not None and item_id in self._item_map:
            self._item_map[item_id].update_geometry()
        self._raster_overlay.rebuild()

    def _on_item_moved(self) -> None:
        self._raster_overlay.rebuild()

    def _on_scene_selection_changed(self) -> None:
        self.selection_updated.emit()

    def selected_model_indices(self) -> list[QModelIndex]:
        result = []
        for item in self._scene.selectedItems():
            if isinstance(item, BaseShapeItem):
                idx = item.model_index()
                if idx.isValid():
                    result.append(idx)
        return result

    def select_model_indices(self, indices: list[QModelIndex]) -> None:
        self._scene.blockSignals(True)
        self._scene.clearSelection()
        for idx in indices:
            item_id = idx.data(Qt.UserRole + 103)
            if item_id is not None and item_id in self._item_map:
                self._item_map[item_id].setSelected(True)
        self._scene.blockSignals(False)

    def wheelEvent(self, event: QWheelEvent) -> None:
        factor = 1.1
        if event.angleDelta().y() > 0:
            self.scale(factor, factor)
        else:
            self.scale(1.0 / factor, 1.0 / factor)

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        if event.mimeData().hasFormat(PALETTE_MIME):
            event.acceptProposedAction()
        else:
            super().dragEnterEvent(event)

    def dragMoveEvent(self, event: QDragMoveEvent) -> None:
        if event.mimeData().hasFormat(PALETTE_MIME):
            event.acceptProposedAction()
        else:
            super().dragMoveEvent(event)

    def dropEvent(self, event: QDropEvent) -> None:
        mime_data = event.mimeData()
        if mime_data.hasFormat(PALETTE_MIME):
            shape_type = decode_palette_mime(mime_data)
            if shape_type is not None:
                scene_pos = self.mapToScene(event.position().toPoint())
                gx = round(scene_pos.x())
                gy = round(scene_pos.y())
                idx = self._model.insert_item(shape_type)
                item = self._model.item_from_index(idx)
                if item:
                    item.properties.x = gx
                    item.properties.y = gy
                    self._model.dataChanged.emit(idx, idx)
                event.acceptProposedAction()
        else:
            super().dropEvent(event)

    def drawBackground(self, painter: QPainter, rect) -> None:
        super().drawBackground(painter, rect)
        if not self._draw_grid_lines:
            return
        painter.setPen(QPen(QColor(230, 230, 230), 0.5))
        cw = self._cell_width
        ch = self._cell_height
        if cw <= 0 or ch <= 0:
            return

        left = int(rect.left() / cw) * cw
        top = int(rect.top() / ch) * ch

        x = left
        while x < rect.right():
            painter.drawLine(
                QPointF(x, rect.top()),
                QPointF(x, rect.bottom()),
            )
            x += cw

        y = top
        while y < rect.bottom():
            painter.drawLine(
                QPointF(rect.left(), y),
                QPointF(rect.right(), y),
            )
            y += ch

    def keyPressEvent(self, event) -> None:
        if event.key() == Qt.Key_Delete:
            self._delete_selected()
        else:
            super().keyPressEvent(event)

    def _delete_selected(self) -> None:
        indices = self.selected_model_indices()
        for idx in sorted(indices, key=lambda i: i.row(), reverse=True):
            self._model.remove_item(idx)

    def raster_overlay(self) -> RasterOverlayItem:
        return self._raster_overlay

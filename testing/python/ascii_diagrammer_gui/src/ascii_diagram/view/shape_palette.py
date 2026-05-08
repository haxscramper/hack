from __future__ import annotations

from PySide6.QtCore import QMimeData, QSize, Qt
from PySide6.QtGui import QPainter
from PySide6.QtWidgets import (
    QListWidget,
    QListWidgetItem,
    QWidget,
)

from ascii_diagram.model.enums import ShapeType
from ascii_diagram.interaction.drag_drop import encode_palette_mime, PALETTE_MIME


class ShapePaletteWidget(QListWidget):

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setDragEnabled(True)
        self.setIconSize(QSize(80, 50))
        self.setSpacing(4)
        self.setViewMode(QListWidget.IconMode)
        self.setFlow(QListWidget.TopToBottom)
        self.setWrapping(False)
        self.setResizeMode(QListWidget.Adjust)
        self._populate()

    def _populate(self) -> None:
        shapes = [
            ("Rectangle", ShapeType.RECTANGLE),
            ("Edge", ShapeType.EDGE),
            ("Ellipse", ShapeType.ELLIPSE),
            ("Text", ShapeType.TEXT),
        ]
        for name, shape_type in shapes:
            item = QListWidgetItem(name)
            item.setData(Qt.UserRole, shape_type)
            item.setFlags(Qt.ItemIsEnabled
                          | Qt.ItemIsSelectable
                          | Qt.ItemIsDragEnabled)
            self.addItem(item)

    def mimeData(self, items) -> QMimeData:
        if not items:
            return QMimeData()
        item = items[0]
        shape_type = item.data(Qt.UserRole)
        if isinstance(shape_type, ShapeType):
            return encode_palette_mime(shape_type)
        return QMimeData()

    def mimeTypes(self) -> list[str]:
        return [PALETTE_MIME]


def palette_shape_type(item: QListWidgetItem) -> ShapeType | None:
    data = item.data(Qt.UserRole)
    if isinstance(data, ShapeType):
        return data
    return None

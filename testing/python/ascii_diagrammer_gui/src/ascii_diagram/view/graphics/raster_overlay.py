from __future__ import annotations
import logging

from typing import Optional

from PySide6.QtCore import QPoint, QPointF, QRectF, Qt
from PySide6.QtGui import QColor, QFont, QFontMetrics, QPainter
from PySide6.QtWidgets import (
    QGraphicsItem,
    QGraphicsScene,
    QStyleOptionGraphicsItem,
    QWidget,
)

from ascii_diagram.model.scene_model import SceneModel
from ascii_diagram.rendering.ascii_grid import AsciiGrid
from ascii_diagram.rendering.raster_renderer import render_scene_to_grid

_MONO_FONT = QFont("Courier New", 12)
_MONO_FONT.setStyleHint(QFont.StyleHint.Monospace)


class RasterOverlayItem(QGraphicsItem):

    def __init__(self, model: SceneModel, scene: QGraphicsScene):
        super().__init__()
        self._model = model
        self._scene = scene
        self._cached_lines: list[str] = []
        self._cached_origin = QPoint(0, 0)
        self._font = _MONO_FONT
        self._dirty = True
        self.setZValue(-10)
        self.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, False)
        self.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsMovable, False)
        self.setAcceptHoverEvents(False)

    def boundingRect(self) -> QRectF:
        if not self._cached_lines:
            return QRectF()
        fm = QFontMetrics(self._font)
        cw = fm.averageCharWidth()
        ch = fm.height()
        w = len(self._cached_lines[0]) if self._cached_lines else 0
        h = len(self._cached_lines)
        return QRectF(
            float(self._cached_origin.x()),
            float(self._cached_origin.y()),
            float(w) * cw,
            float(h) * ch,
        )

    def paint(
        self,
        painter: QPainter,
        option: QStyleOptionGraphicsItem,
        widget: Optional[QWidget] = None,
    ) -> None:
        painter.setFont(self._font)
        painter.setPen(QColor(40, 40, 40))
        fm = painter.fontMetrics()
        cw = fm.averageCharWidth()
        ch = fm.height()
        ascent = fm.ascent()

        for row_idx, line in enumerate(self._cached_lines):
            y = float(self._cached_origin.y()) + float(row_idx) * ch + ascent
            for col_idx, ch_char in enumerate(line):
                if ch_char != " ":
                    x = float(self._cached_origin.x()) + float(col_idx) * cw
                    painter.drawText(QPointF(x, y), ch_char)

    def invalidate_cache(self) -> None:
        self._dirty = True
        self.update()

    def rebuild(self) -> None:
        grid = render_scene_to_grid(self._model)
        self._cached_lines = grid.to_lines(crop_to_bounds=False)
        bounds = grid.bounds()
        self._cached_origin = QPoint(bounds[0] - 10, bounds[1] - 10)
        self.prepareGeometryChange()
        self._dirty = False
        self.update()

    def cache_valid(self) -> bool:
        return not self._dirty

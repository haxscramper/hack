from __future__ import annotations

import math

from ascii_diagram.model.enums import EdgeType, ShapeType
from ascii_diagram.model.scene_item import SceneItem
from ascii_diagram.model.scene_model import SceneModel, QModelIndex
from ascii_diagram.model.shape_properties import (
    EdgeData,
    EllipseData,
    RectData,
    TextData,
)

from PySide6.QtCore import QPointF, Qt
from PySide6.QtGui import QPainterPath, QPolygonF
from PySide6.QtWidgets import QGraphicsItem, QGraphicsPathItem


def create_vector_item(model: SceneModel,
                       index: QModelIndex) -> QGraphicsItem | None:
    item = model.item_from_index(index)
    if item is None:
        return None
    abs_pos = item.absolute_position()
    props = item.properties

    if isinstance(props, RectData):
        return _make_rect_item(abs_pos, props)
    elif isinstance(props, EdgeData):
        return _make_edge_item(abs_pos, props)
    elif isinstance(props, EllipseData):
        return _make_ellipse_item(abs_pos, props)
    elif isinstance(props, TextData):
        return _make_text_item(abs_pos, props)
    return None


def _make_rect_item(abs_pos, data: RectData) -> QGraphicsPathItem:
    x = abs_pos.x() + data.x + 0.5
    y = abs_pos.y() + data.y + 0.5
    w = data.width
    h = data.height
    path = QPainterPath()
    path.addRect(x, y, float(w), float(h))
    item = QGraphicsPathItem(path)
    item.setFlag(QGraphicsItem.ItemIsSelectable, True)
    item.setFlag(QGraphicsItem.ItemIsMovable, False)
    return item


def _make_edge_item(abs_pos, data: EdgeData) -> QGraphicsPathItem:
    path = QPainterPath()
    sx = float(abs_pos.x() + data.start.x()) + 0.5
    sy = float(abs_pos.y() + data.start.y()) + 0.5
    ex = float(abs_pos.x() + data.end.x()) + 0.5
    ey = float(abs_pos.y() + data.end.y()) + 0.5

    if data.edge_type == EdgeType.SPLINE and len(data.bends) == 2:
        cp1 = data.bends[0]
        cp2 = data.bends[1]
        cx1 = float(abs_pos.x() + cp1.x()) + 0.5
        cy1 = float(abs_pos.y() + cp1.y()) + 0.5
        cx2 = float(abs_pos.x() + cp2.x()) + 0.5
        cy2 = float(abs_pos.y() + cp2.y()) + 0.5
        path.moveTo(sx, sy)
        path.cubicTo(cx1, cy1, cx2, cy2, ex, ey)
    elif data.edge_type == EdgeType.SPLINE and len(data.bends) == 1:
        cp = data.bends[0]
        cx = float(abs_pos.x() + cp.x()) + 0.5
        cy = float(abs_pos.y() + cp.y()) + 0.5
        path.moveTo(sx, sy)
        path.quadTo(cx, cy, ex, ey)
    else:
        path.moveTo(sx, sy)
        for b in data.bends:
            bx = float(abs_pos.x() + b.x()) + 0.5
            by = float(abs_pos.y() + b.y()) + 0.5
            path.lineTo(bx, by)
        path.lineTo(ex, ey)

    item = QGraphicsPathItem(path)
    item.setFlag(QGraphicsItem.ItemIsSelectable, True)
    item.setFlag(QGraphicsItem.ItemIsMovable, False)
    return item


def _make_ellipse_item(abs_pos, data: EllipseData) -> QGraphicsPathItem:
    x = abs_pos.x() + data.x + 0.5
    y = abs_pos.y() + data.y + 0.5
    w = data.width
    h = data.height
    path = QPainterPath()
    path.addEllipse(x, y, float(w), float(h))
    item = QGraphicsPathItem(path)
    item.setFlag(QGraphicsItem.ItemIsSelectable, True)
    item.setFlag(QGraphicsItem.ItemIsMovable, False)
    return item


def _make_text_item(abs_pos, data: TextData) -> QGraphicsPathItem:
    x = abs_pos.x() + data.x + 0.5
    y = abs_pos.y() + data.y + 0.5
    w = data.width
    h = data.height
    path = QPainterPath()
    path.addRect(x, y, float(w), float(h))
    item = QGraphicsPathItem(path)
    item.setFlag(QGraphicsItem.ItemIsSelectable, True)
    item.setFlag(QGraphicsItem.ItemIsMovable, False)
    return item

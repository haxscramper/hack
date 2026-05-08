from __future__ import annotations

from PySide6.QtCore import QPointF
from PySide6.QtGui import QPainterPath

from ascii_diagram.model.scene_model import SceneModel, QModelIndex
from ascii_diagram.model.shape_properties import (
    EdgeData,
    EllipseData,
    RectData,
    TextData,
)
from ascii_diagram.model.enums import EdgeType
from ascii_diagram.view.graphics.base_shape_item import BaseShapeItem


class RectShapeItem(BaseShapeItem):

    def update_geometry(self) -> None:
        item = self._model.item_from_index(self._model_index)
        if item is None:
            return
        abs_pos = item.absolute_position()
        props = item.properties
        assert isinstance(props, RectData)
        x = float(abs_pos.x() + props.x) + 0.5
        y = float(abs_pos.y() + props.y) + 0.5
        w = float(props.width)
        h = float(props.height)
        path = QPainterPath()
        path.addRect(x, y, w, h)
        self.setPath(path)


class EdgeShapeItem(BaseShapeItem):

    def update_geometry(self) -> None:
        item = self._model.item_from_index(self._model_index)
        if item is None:
            return
        abs_pos = item.absolute_position()
        props = item.properties
        if not isinstance(props, EdgeData):
            return
        path = QPainterPath()
        sx = float(abs_pos.x() + props.start.x()) + 0.5
        sy = float(abs_pos.y() + props.start.y()) + 0.5
        ex = float(abs_pos.x() + props.end.x()) + 0.5
        ey = float(abs_pos.y() + props.end.y()) + 0.5

        if props.edge_type == EdgeType.SPLINE and len(props.bends) == 2:
            cp1 = props.bends[0]
            cp2 = props.bends[1]
            path.moveTo(sx, sy)
            path.cubicTo(
                float(abs_pos.x() + cp1.x()) + 0.5,
                float(abs_pos.y() + cp1.y()) + 0.5,
                float(abs_pos.x() + cp2.x()) + 0.5,
                float(abs_pos.y() + cp2.y()) + 0.5,
                ex,
                ey,
            )
        elif props.edge_type == EdgeType.SPLINE and len(props.bends) == 1:
            cp = props.bends[0]
            path.moveTo(sx, sy)
            path.quadTo(
                float(abs_pos.x() + cp.x()) + 0.5,
                float(abs_pos.y() + cp.y()) + 0.5,
                ex,
                ey,
            )
        else:
            path.moveTo(sx, sy)
            for b in props.bends:
                path.lineTo(
                    float(abs_pos.x() + b.x()) + 0.5,
                    float(abs_pos.y() + b.y()) + 0.5,
                )
            path.lineTo(ex, ey)

        self.setPath(path)


class EllipseShapeItem(BaseShapeItem):

    def update_geometry(self) -> None:
        item = self._model.item_from_index(self._model_index)
        if item is None:
            return
        abs_pos = item.absolute_position()
        props = item.properties
        if not isinstance(props, EllipseData):
            return
        x = float(abs_pos.x() + props.x) + 0.5
        y = float(abs_pos.y() + props.y) + 0.5
        w = float(props.width)
        h = float(props.height)
        path = QPainterPath()
        path.addEllipse(x, y, w, h)
        self.setPath(path)


class TextShapeItem(BaseShapeItem):

    def update_geometry(self) -> None:
        item = self._model.item_from_index(self._model_index)
        if item is None:
            return
        abs_pos = item.absolute_position()
        props = item.properties
        if not isinstance(props, TextData):
            return
        x = float(abs_pos.x() + props.x) + 0.5
        y = float(abs_pos.y() + props.y) + 0.5
        w = float(props.width)
        h = float(props.height)
        path = QPainterPath()
        path.addRect(x, y, w, h)
        self.setPath(path)


def make_shape_item(model: SceneModel,
                    index: QModelIndex) -> BaseShapeItem | None:
    item = model.item_from_index(index)
    if item is None:
        return None
    props = item.properties

    if isinstance(props, RectData):
        shape = RectShapeItem(index, model)
    elif isinstance(props, EdgeData):
        shape = EdgeShapeItem(index, model)
    elif isinstance(props, EllipseData):
        shape = EllipseShapeItem(index, model)
    elif isinstance(props, TextData):
        shape = TextShapeItem(index, model)
    else:
        assert False

    shape.update_geometry()
    return shape

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List

from PySide6.QtCore import QPoint

from ascii_diagram.model.enums import (
    EdgeType,
    HJustify,
    OverlapMode,
    VAlign,
)


@dataclass
class ShapeProperties:
    x: int = 0
    y: int = 0
    overlap_mode: OverlapMode = OverlapMode.OVERWRITE
    visible: bool = True

    def to_dict(self) -> dict:
        raise NotImplementedError

    @staticmethod
    def from_dict(d: dict) -> ShapeProperties:
        raise NotImplementedError


@dataclass
class RectData(ShapeProperties):
    width: int = 4
    height: int = 3
    corner_ch: str = "+"
    top_ch: str = "-"
    bottom_ch: str = "-"
    left_ch: str = "|"
    right_ch: str = "|"

    def to_dict(self) -> dict:
        return {
            "x": self.x,
            "y": self.y,
            "width": self.width,
            "height": self.height,
            "corner_ch": self.corner_ch,
            "top_ch": self.top_ch,
            "bottom_ch": self.bottom_ch,
            "left_ch": self.left_ch,
            "right_ch": self.right_ch,
            "overlap_mode": self.overlap_mode.name,
            "visible": self.visible,
        }

    @staticmethod
    def from_dict(d: dict) -> RectData:
        return RectData(
            x=d.get("x", 0),
            y=d.get("y", 0),
            width=d.get("width", 4),
            height=d.get("height", 3),
            corner_ch=d.get("corner_ch", "+"),
            top_ch=d.get("top_ch", "-"),
            bottom_ch=d.get("bottom_ch", "-"),
            left_ch=d.get("left_ch", "|"),
            right_ch=d.get("right_ch", "|"),
            overlap_mode=OverlapMode[d.get("overlap_mode", "OVERWRITE")],
            visible=d.get("visible", True),
        )


@dataclass
class EdgeData(ShapeProperties):
    start: QPoint = field(default_factory=lambda: QPoint(0, 0))
    end: QPoint = field(default_factory=lambda: QPoint(5, 0))
    bends: List[QPoint] = field(default_factory=list)
    edge_type: EdgeType = EdgeType.POLYLINE
    h_char: str = "-"
    v_char: str = "|"
    start_arrow: str = ""
    end_arrow: str = ">"
    bend_char: str = "+"

    def to_dict(self) -> dict:
        return {
            "x": self.x,
            "y": self.y,
            "start_x": self.start.x(),
            "start_y": self.start.y(),
            "end_x": self.end.x(),
            "end_y": self.end.y(),
            "bends": [[p.x(), p.y()] for p in self.bends],
            "edge_type": self.edge_type.name,
            "h_char": self.h_char,
            "v_char": self.v_char,
            "start_arrow": self.start_arrow,
            "end_arrow": self.end_arrow,
            "bend_char": self.bend_char,
            "overlap_mode": self.overlap_mode.name,
            "visible": self.visible,
        }

    @staticmethod
    def from_dict(d: dict) -> EdgeData:
        bends = [QPoint(p[0], p[1]) for p in d.get("bends", [])]
        return EdgeData(
            x=d.get("x", 0),
            y=d.get("y", 0),
            start=QPoint(d.get("start_x", 0), d.get("start_y", 0)),
            end=QPoint(d.get("end_x", 5), d.get("end_y", 0)),
            bends=bends,
            edge_type=EdgeType[d.get("edge_type", "POLYLINE")],
            h_char=d.get("h_char", "-"),
            v_char=d.get("v_char", "|"),
            start_arrow=d.get("start_arrow", ""),
            end_arrow=d.get("end_arrow", ">"),
            bend_char=d.get("bend_char", "+"),
            overlap_mode=OverlapMode[d.get("overlap_mode", "OVERWRITE")],
            visible=d.get("visible", True),
        )


@dataclass
class EllipseData(ShapeProperties):
    width: int = 4
    height: int = 2
    char: str = "*"

    def to_dict(self) -> dict:
        return {
            "x": self.x,
            "y": self.y,
            "width": self.width,
            "height": self.height,
            "char": self.char,
            "overlap_mode": self.overlap_mode.name,
            "visible": self.visible,
        }

    @staticmethod
    def from_dict(d: dict) -> EllipseData:
        return EllipseData(
            x=d.get("x", 0),
            y=d.get("y", 0),
            width=d.get("width", 4),
            height=d.get("height", 2),
            char=d.get("char", "*"),
            overlap_mode=OverlapMode[d.get("overlap_mode", "OVERWRITE")],
            visible=d.get("visible", True),
        )


@dataclass
class TextData(ShapeProperties):
    width: int = 10
    height: int = 3
    text: str = "Text"
    h_justify: HJustify = HJustify.LEFT
    v_align: VAlign = VAlign.TOP

    def to_dict(self) -> dict:
        return {
            "x": self.x,
            "y": self.y,
            "width": self.width,
            "height": self.height,
            "text": self.text,
            "h_justify": self.h_justify.name,
            "v_align": self.v_align.name,
            "overlap_mode": self.overlap_mode.name,
            "visible": self.visible,
        }

    @staticmethod
    def from_dict(d: dict) -> TextData:
        return TextData(
            x=d.get("x", 0),
            y=d.get("y", 0),
            width=d.get("width", 10),
            height=d.get("height", 3),
            text=d.get("text", "Text"),
            h_justify=HJustify[d.get("h_justify", "LEFT")],
            v_align=VAlign[d.get("v_align", "TOP")],
            overlap_mode=OverlapMode[d.get("overlap_mode", "OVERWRITE")],
            visible=d.get("visible", True),
        )


_PROPERTY_CLASSES = {
    "RECTANGLE": RectData,
    "EDGE": EdgeData,
    "ELLIPSE": EllipseData,
    "TEXT": TextData,
}


def property_from_dict(shape_type: str, d: dict) -> ShapeProperties:
    cls = _PROPERTY_CLASSES.get(shape_type)
    if cls is None:
        raise ValueError(f"Unknown shape type: {shape_type}")
    return cls.from_dict(d)


def make_default_properties(shape_type: str) -> ShapeProperties:
    cls = _PROPERTY_CLASSES.get(shape_type)
    if cls is None:
        raise ValueError(f"Unknown shape type: {shape_type}")
    return cls()
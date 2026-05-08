from enum import Enum, auto


class EdgeType(Enum):
    POLYLINE = auto()
    SPLINE = auto()
    ORTHOGONAL = auto()


class OverlapMode(Enum):
    OVERWRITE = auto()
    EMPTY_ONLY = auto()
    MERGE = auto()


class HJustify(Enum):
    LEFT = auto()
    CENTER = auto()
    RIGHT = auto()


class VAlign(Enum):
    TOP = auto()
    CENTER = auto()
    BOTTOM = auto()


class ShapeType(Enum):
    RECTANGLE = auto()
    EDGE = auto()
    ELLIPSE = auto()
    TEXT = auto()
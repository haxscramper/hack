"""Input models describing the supported JSON schema.

This module matches the specification and represents the user-authored DSL
as a typed Pydantic AST. The implementation only supports JSON input that
validates against these models.
"""

from __future__ import annotations

from enum import Enum
from beartype import beartype
from beartype.typing import Annotated, Literal, Optional, Union

from pydantic import BaseModel, Field


class ShapeType(str, Enum):
    RECT = "rect"
    ELLIPSE = "ellipse"
    DIAMOND = "diamond"
    TEXT = "text"
    LINE = "line"
    GROUP = "group"


class SpatialRelation(str, Enum):
    LEFT_OF = "left-of"
    RIGHT_OF = "right-of"
    ABOVE = "above"
    BELOW = "below"


class BBoxAnchor(str, Enum):
    LEFT = "bbox-left"
    RIGHT = "bbox-right"
    TOP = "bbox-top"
    BOTTOM = "bbox-bottom"
    CENTER_X = "bbox-center-x"
    CENTER_Y = "bbox-center-y"
    CENTER = "bbox-center"


class PointAccessor(str, Enum):
    """For LINE shapes — access named points in the point sequence."""

    START_POINT = "start-point"
    END_POINT = "end-point"


class AlignAxis(str, Enum):
    X = "x"
    Y = "y"


class AnchorRef(BaseModel):
    """Reference to a shape anchor.

    Anchors refer to bounding-box anchors for all shapes and to additional
    start/end point anchors for LINE shapes.
    """

    shape_id: str
    anchor: Union[BBoxAnchor, PointAccessor]


class Vec2(BaseModel):
    """Simple numeric vector."""

    x: float
    y: float


class LinePointExprs(BaseModel):
    """Defines a line as a sequence of points relative to the line start position."""

    points: list["PointExpr"] = Field(min_length=2)


class PointLiteral(BaseModel):
    """Literal local point used in line point expressions."""

    type: Literal["point-literal"] = "point-literal"
    x: float
    y: float


class PointAdd(BaseModel):
    """Point-wise addition expression."""

    type: Literal["point-add"] = "point-add"
    left: "PointExpr"
    right: "PointExpr"


class PointSub(BaseModel):
    """Point-wise subtraction expression."""

    type: Literal["point-sub"] = "point-sub"
    left: "PointExpr"
    right: "PointExpr"


class PointScale(BaseModel):
    """Scales a point expression by a scalar factor."""

    type: Literal["point-scale"] = "point-scale"
    expr: "PointExpr"
    factor: float


PointExpr = Annotated[
    Union[
        PointLiteral,
        PointAdd,
        PointSub,
        PointScale,
    ],
    Field(discriminator="type"),
]


class AxisValue(BaseModel):
    """A single axis value expression.

    Exactly one source is intended to be used per axis. This is enforced
    semantically during expansion/validation.
    """

    type: Literal["axis-value"] = "axis-value"
    fixed: Optional[float] = None
    ref: Optional[str] = None
    pct: Optional[float] = None


class FixedSize(BaseModel):
    """Fixed-size shape dimensions."""

    type: Literal["fixed"] = "fixed"
    w: AxisValue
    h: AxisValue


class PercentOfRefSize(BaseModel):
    """Size as percentage or fixed amount per axis.

    ref=None in nested input defaults to the parent during expansion.
    """

    type: Literal["percent-of"] = "percent-of"
    w: AxisValue
    h: AxisValue


SizeExpr = Annotated[
    Union[FixedSize, PercentOfRefSize],
    Field(discriminator="type"),
]


class AbsolutePos(BaseModel):
    """Absolute shape origin in canvas coordinates."""

    type: Literal["absolute"] = "absolute"
    x: float
    y: float


class VecLiteral(BaseModel):
    """Literal vector used inside compositional position expressions."""

    type: Literal["vec-literal"] = "vec-literal"
    x: float
    y: float


class VecOffset(BaseModel):
    """Applies a literal offset to another position expression."""

    type: Literal["vec-offset"] = "vec-offset"
    base: "PositionExpr"
    offset: VecLiteral


class PercentOfRefPos(BaseModel):
    """Position at a percentage of a referenced shape's dimensions.

    ref=None means "parent" and is resolved during expansion.
    """

    type: Literal["percent-of"] = "percent-of"
    ref: Optional[str] = None
    x_pct: Optional[float] = None
    y_pct: Optional[float] = None


class RelativePlacement(BaseModel):
    """Relative placement against a target anchor."""

    type: Literal["relative"] = "relative"
    relation: SpatialRelation
    target: AnchorRef
    gap: float = 0.0


class InsideOf(BaseModel):
    """Containment relation requiring the shape to fit inside a target shape."""

    type: Literal["inside-of"] = "inside-of"
    target: str


class AlignWith(BaseModel):
    """Anchor equality along a single axis."""

    type: Literal["align-with"] = "align-with"
    anchors: list[AnchorRef]
    axis: AlignAxis


class Add(BaseModel):
    """Vector composition by addition."""

    type: Literal["add"] = "add"
    left: "PositionExpr"
    right: "PositionExpr"


class Sub(BaseModel):
    """Vector composition by subtraction."""

    type: Literal["sub"] = "sub"
    left: "PositionExpr"
    right: "PositionExpr"


class Scale(BaseModel):
    """Vector composition by scalar multiplication."""

    type: Literal["scale"] = "scale"
    expr: "PositionExpr"
    factor: float


class Conjunction(BaseModel):
    """Emit all constraints from the contained expressions."""

    type: Literal["conjunction"] = "conjunction"
    exprs: list["PositionExpr"]


PositionExpr = Annotated[
    Union[
        AbsolutePos,
        VecLiteral,
        VecOffset,
        PercentOfRefPos,
        RelativePlacement,
        InsideOf,
        AlignWith,
        Add,
        Sub,
        Scale,
        Conjunction,
    ],
    Field(discriminator="type"),
]

PointAdd.model_rebuild()
PointSub.model_rebuild()
PointScale.model_rebuild()
Conjunction.model_rebuild()
Add.model_rebuild()
Sub.model_rebuild()
Scale.model_rebuild()
VecOffset.model_rebuild()


class ShapeDefinition(BaseModel):
    """A shape in the input tree.

    `children` enables nesting. Expansion flattens this structure, records
    parent/child mappings, injects containment constraints, and resolves
    implicit parent references.
    """

    id: str
    shape_type: ShapeType
    size: SizeExpr
    position: PositionExpr
    text: Optional[str] = None
    line: Optional[LinePointExprs] = None
    children: list["ShapeDefinition"] = Field(default_factory=list)


class SpacedBy(BaseModel):
    """Constrain consecutive anchors by a constant 2D step."""

    type: Literal["spaced-by"] = "spaced-by"
    anchors: list[AnchorRef]
    spacing: Vec2


class HorizontalAlign(BaseModel):
    """Align anchors to a common horizontal line (same Y)."""

    type: Literal["horizontal-align"] = "horizontal-align"
    anchors: list[AnchorRef]
    max_offset: float = 0.0


class VerticalAlign(BaseModel):
    """Align anchors to a common vertical line (same X)."""

    type: Literal["vertical-align"] = "vertical-align"
    anchors: list[AnchorRef]
    max_offset: float = 0.0


MultiShapeConstraint = Annotated[
    Union[SpacedBy, HorizontalAlign, VerticalAlign],
    Field(discriminator="type"),
]


class DiagramInput(BaseModel):
    """Top-level user input."""

    canvas_width: float
    canvas_height: float
    shapes: list[ShapeDefinition]
    constraints: list[MultiShapeConstraint] = Field(default_factory=list)


ShapeDefinition.model_rebuild()

from __future__ import annotations

from beartype import beartype
from enum import Enum
from typing import Union

from pydantic import BaseModel, Field


@beartype
class ExprType(str, Enum):
    LITERAL = "literal"
    PERCENT = "percent"
    VEC = "vec"
    REF = "ref"
    LEFT_OF = "left_of"
    RIGHT_OF = "right_of"
    ABOVE = "above"
    BELOW = "below"
    ADD = "add"
    SUB = "sub"
    NEG = "neg"


@beartype
class Rect(BaseModel):
    x: float
    y: float
    width: float
    height: float


@beartype
class Vec(BaseModel):
    x: float
    y: float


@beartype
class Expr(BaseModel):
    type: ExprType
    value: float | str | Rect | Vec | None = None
    name: str | None = None
    left: Expr | None = None
    right: Expr | None = None

    @staticmethod
    def literal(v: float | str) -> Expr:
        return Expr(type=ExprType.LITERAL, value=v)

    @staticmethod
    def percent(v: float) -> Expr:
        return Expr(type=ExprType.PERCENT, value=v)

    @staticmethod
    def vec(x: Expr, y: Expr) -> Expr:
        return Expr(type=ExprType.VEC, left=x, right=y)

    @staticmethod
    def ref(name: str) -> Expr:
        return Expr(type=ExprType.REF, name=name)

    @staticmethod
    def left_of(name: str) -> Expr:
        return Expr(type=ExprType.LEFT_OF, name=name)

    @staticmethod
    def right_of(name: str) -> Expr:
        return Expr(type=ExprType.RIGHT_OF, name=name)

    @staticmethod
    def above(name: str) -> Expr:
        return Expr(type=ExprType.ABOVE, name=name)

    @staticmethod
    def below(name: str) -> Expr:
        return Expr(type=ExprType.BELOW, name=name)

    @staticmethod
    def add(l: Expr, r: Expr) -> Expr:
        return Expr(type=ExprType.ADD, left=l, right=r)

    @staticmethod
    def sub(l: Expr, r: Expr) -> Expr:
        return Expr(type=ExprType.SUB, left=l, right=r)

    @staticmethod
    def neg(e: Expr) -> Expr:
        return Expr(type=ExprType.NEG, left=e)


@beartype
class LineCommand(BaseModel):
    pass


@beartype
class LineToCommand(LineCommand):
    x: Expr
    y: Expr


@beartype
class MoveToCommand(LineCommand):
    x: Expr
    y: Expr


@beartype
class HorizontalToCommand(LineCommand):
    length: Expr


@beartype
class VerticalToCommand(LineCommand):
    length: Expr


@beartype
class ShapeKind(str, Enum):
    RECT = "rect"
    CIRCLE = "circle"
    ELLIPSE = "ellipse"
    GROUP = "group"
    LINE = "line"
    TEXT = "text"


@beartype
class Shape(BaseModel):
    kind: ShapeKind
    positional_args: list[Expr] = Field(default_factory=list)
    keyword_args: dict[str, Expr] = Field(default_factory=dict)
    subnodes: list[Statement] = Field(default_factory=list)
    line_commands: list[LineCommand] = Field(default_factory=list)
    charset_override: str | None = None


@beartype
class LetStatement(BaseModel):
    name: str
    shape: Shape | FunctionCall


@beartype
class FunctionDef(BaseModel):
    name: str
    params: list[str] = Field(default_factory=list)
    keyword_params: dict[str, Expr] = Field(default_factory=dict)
    body: list[Statement] = Field(default_factory=list)


@beartype
class FunctionCall(BaseModel):
    name: str
    positional_args: list[Expr] = Field(default_factory=list)
    keyword_args: dict[str, Expr] = Field(default_factory=dict)
    subnodes: list[Statement] = Field(default_factory=list)


Statement = Union[Shape, LetStatement, FunctionDef, FunctionCall]

# Pydantic v2 rebuild for forward references
LetStatement.model_rebuild()
Shape.model_rebuild()
FunctionDef.model_rebuild()
FunctionCall.model_rebuild()


@beartype
class ResolvedBox(BaseModel):
    x: float
    y: float
    width: float
    height: float


@beartype
class ResolvedShape(BaseModel):
    kind: ShapeKind
    box: ResolvedBox
    charset_override: str | None = None
    text: str | None = None
    wrap_width: int | None = None
    line_points: list[tuple[float, float] | None] = Field(default_factory=list)
    subnodes: list[ResolvedShape] = Field(default_factory=list)


@beartype
class Scene(BaseModel):
    shapes: list[ResolvedShape] = Field(default_factory=list)
    width: float | None = None
    height: float | None = None

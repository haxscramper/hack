from __future__ import annotations

from enum import Enum
from typing import Union

from pydantic import BaseModel, Field


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


class Expr(BaseModel):
    type: ExprType
    value: float | None = None
    name: str | None = None
    left: Expr | None = None
    right: Expr | None = None

    @staticmethod
    def literal(v: float) -> Expr:
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


class LineCommand(BaseModel):
    pass


class LineToCommand(LineCommand):
    x: Expr
    y: Expr


class MoveToCommand(LineCommand):
    x: Expr
    y: Expr


class HorizontalToCommand(LineCommand):
    length: Expr


class VerticalToCommand(LineCommand):
    length: Expr


class ShapeKind(str, Enum):
    RECT = "rect"
    CIRCLE = "circle"
    ELLIPSE = "ellipse"
    GROUP = "group"
    LINE = "line"
    TEXT = "text"


class Shape(BaseModel):
    kind: ShapeKind
    positional_args: list[Expr] = Field(default_factory=list)
    keyword_args: dict[str, Expr | str] = Field(default_factory=dict)
    subnodes: list[Statement] = Field(default_factory=list)
    line_commands: list[LineCommand] = Field(default_factory=list)
    charset_override: str | None = None


class LetStatement(BaseModel):
    name: str
    shape: Shape | FunctionCall


class FunctionDef(BaseModel):
    name: str
    params: list[str] = Field(default_factory=list)
    keyword_params: dict[str, Expr] = Field(default_factory=dict)
    body: list[Statement] = Field(default_factory=list)


class FunctionCall(BaseModel):
    name: str
    positional_args: list[Expr] = Field(default_factory=list)
    keyword_args: dict[str, Expr | str] = Field(default_factory=dict)
    subnodes: list[Statement] = Field(default_factory=list)


Statement = Union[Shape, LetStatement, FunctionDef, FunctionCall]

# Pydantic v2 rebuild for forward references
LetStatement.model_rebuild()
Shape.model_rebuild()
FunctionDef.model_rebuild()
FunctionCall.model_rebuild()


class ResolvedBox(BaseModel):
    x: float
    y: float
    width: float
    height: float


class ResolvedShape(BaseModel):
    kind: ShapeKind
    box: ResolvedBox
    charset_override: str | None = None
    text: str | None = None
    wrap_width: int | None = None
    line_points: list[tuple[float, float]] = Field(default_factory=list)
    subnodes: list[ResolvedShape] = Field(default_factory=list)


class Scene(BaseModel):
    shapes: list[ResolvedShape] = Field(default_factory=list)
    width: float | None = None
    height: float | None = None

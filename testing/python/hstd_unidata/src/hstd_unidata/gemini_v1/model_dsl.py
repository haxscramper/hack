#!/usr/bin/env python
from __future__ import annotations

import abc
import ast
import csv
import enum
import functools
import inspect
import sys
import tempfile
import textwrap
import types
from pathlib import Path

from beartype import beartype
from beartype.typing import (
    Annotated,
    Any,
    Callable,
    ClassVar,
    Dict,
    ForwardRef,
    List,
    Optional,
    Sequence,
    Tuple,
    Union,
    get_args,
    get_origin,
)
from plumbum import local
from pydantic import BaseModel, ConfigDict
from pydantic import Field as PydanticField

# ============================================================================
# 1. Constants & Metadata Enum
# ============================================================================


class DslAttr(str, enum.Enum):
    ROOT = "__dsl_root__"
    MODEL = "__dsl_model__"
    KIND = "__dsl_kind__"
    QNAME = "__dsl_qname__"
    UNRESOLVED = "__dsl_unresolved__"
    RECORD = "__dsl_record__"
    DIRECTIVES = "__dsl_directives__"


class RecordKind(str, enum.Enum):
    CLASS = "class"
    CASE = "case"
    ENUM = "enum"


class CheckLevel(str, enum.Enum):
    ERROR = "error"
    WARNING = "warning"


# ============================================================================
# 2. Source Location & Runtime Path Types
# ============================================================================


@beartype
class SourceLoc(BaseModel):
    file: str
    line: int
    function: str
    code_line: str = ""


_THIS_FILE = str(Path(__file__).resolve())


@beartype
def capture_caller_loc() -> SourceLoc:
    frame: Optional[types.FrameType] = sys._getframe(1)
    while frame is not None:
        fname = frame.f_code.co_filename
        if not fname.startswith("<") and str(Path(fname).resolve()) != _THIS_FILE:
            line_no = frame.f_lineno
            code_line = ""
            try:
                lines = Path(fname).read_text().splitlines()
                if 1 <= line_no <= len(lines):
                    code_line = lines[line_no - 1].strip()
            except OSError:
                pass
            return SourceLoc(
                file=fname,
                line=line_no,
                function=frame.f_code.co_name,
                code_line=code_line,
            )
        frame = frame.f_back
    fallback = sys._getframe(1)
    return SourceLoc(
        file=fallback.f_code.co_filename,
        line=fallback.f_lineno,
        function=fallback.f_code.co_name,
    )


def dsl_entry[F: Callable[..., Any]](func: F) -> F:
    @functools.wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        if "_loc" not in kwargs or kwargs["_loc"] is None:
            kwargs["_loc"] = capture_caller_loc()
        return func(*args, **kwargs)

    return wrapper  # type: ignore[return-value]


class PathStepKind(str, enum.Enum):
    FIELD = "field"
    INDEX = "index"
    KEY = "key"
    CASE = "case"


@beartype
class PathStep(BaseModel):
    kind: PathStepKind
    field_name: Optional[str] = None
    index: Optional[int] = None
    map_key: Optional[str] = None
    case_name: Optional[str] = None


@beartype
class ObjectPath(BaseModel):
    steps: List[PathStep] = PydanticField(default_factory=list)

    def to_json_path(self) -> str:
        parts = ["$"]
        for s in self.steps:
            match s.kind:
                case PathStepKind.FIELD | PathStepKind.CASE:
                    parts.append(f".{s.field_name or s.case_name}")
                case PathStepKind.INDEX:
                    parts.append(f"[{s.index}]")
                case PathStepKind.KEY:
                    parts.append(f"[{s.map_key!r}]")
        return "".join(parts)


# ============================================================================
# 3. Structural Types & QName IR
# ============================================================================


@beartype
class QName(BaseModel):
    name: str
    scope: Optional[QName] = None
    package: str = ""
    is_external: bool = False

    def to_id(self) -> str:
        prefix = f"{self.scope.to_id()}::" if self.scope else ""
        pkg = f"@{self.package}:" if self.is_external and self.package else ""
        return f"{pkg}{prefix}{self.name}"

    def to_souffle_adt(self) -> str:
        scope_adt = (
            f"$SomeScope({self.scope.to_souffle_adt()})" if self.scope else "$NoScope()"
        )
        return f'$QNameValue("{self.package}", "{self.name}", {scope_adt})'


class TypeKind(str, enum.Enum):
    PRIMITIVE = "primitive"
    LIST = "list"
    ARRAY = "array"
    MAP = "map"
    OPTIONAL = "optional"
    TUPLE = "tuple"
    VARIANT = "variant"
    LOCATED = "located"
    METATYPE = "metatype"
    NAMED = "named"


class PrimName(str, enum.Enum):
    INT8 = "PrimInt8"
    INT16 = "PrimInt16"
    INT32 = "PrimInt32"
    INT64 = "PrimInt64"
    UINT8 = "PrimUInt8"
    UINT16 = "PrimUInt16"
    UINT32 = "PrimUInt32"
    UINT64 = "PrimUInt64"
    FLOAT32 = "PrimFloat32"
    FLOAT64 = "PrimFloat64"
    STRING = "PrimString"
    BOOL = "PrimBool"
    NONE = "PrimNone"
    ANY = "PrimAny"
    PATH = "PrimPath"


@beartype
class DslType(BaseModel):
    kind: TypeKind
    prim: Optional[PrimName] = None
    params: List[DslType] = PydanticField(default_factory=list)
    fixed_size: Optional[int] = None
    qname: Optional[QName] = None

    def to_souffle_adt(self) -> str:
        match self.kind:
            case TypeKind.PRIMITIVE:
                assert self.prim is not None
                return f"$Primitive(${self.prim.value}())"
            case TypeKind.LIST:
                return f"$List({self.params[0].to_souffle_adt()})"
            case TypeKind.ARRAY:
                return (
                    f"$Array({self.params[0].to_souffle_adt()}, {self.fixed_size or 0})"
                )
            case TypeKind.MAP:
                return f"$Map({self.params[0].to_souffle_adt()}, {self.params[1].to_souffle_adt()})"
            case TypeKind.OPTIONAL:
                return f"$Optional({self.params[0].to_souffle_adt()})"
            case TypeKind.LOCATED:
                return f"$Located({self.params[0].to_souffle_adt()})"
            case TypeKind.METATYPE:
                return f"$MetaType({self.params[0].to_souffle_adt()})"
            case TypeKind.TUPLE:
                tlist = "$TNil()"
                for p in reversed(self.params):
                    tlist = f"$TCons({p.to_souffle_adt()}, {tlist})"
                return f"$Tuple({tlist})"
            case TypeKind.VARIANT:
                tlist = "$TNil()"
                for p in reversed(self.params):
                    tlist = f"$TCons({p.to_souffle_adt()}, {tlist})"
                return f"$Variant({tlist})"
            case TypeKind.NAMED:
                assert self.qname is not None
                return f'$Named({self.qname.to_souffle_adt()}, "{self.qname.to_id()}")'

    def display(self) -> str:
        match self.kind:
            case TypeKind.PRIMITIVE:
                return (
                    self.prim.value.removeprefix("Prim").lower()
                    if self.prim
                    else "prim"
                )
            case TypeKind.LIST:
                return f"list<{self.params[0].display()}>"
            case TypeKind.ARRAY:
                return f"array<{self.params[0].display()}, {self.fixed_size}>"
            case TypeKind.MAP:
                return f"map<{self.params[0].display()}, {self.params[1].display()}>"
            case TypeKind.OPTIONAL:
                return f"optional<{self.params[0].display()}>"
            case TypeKind.LOCATED:
                return f"located<{self.params[0].display()}>"
            case TypeKind.METATYPE:
                return f"typeof<{self.params[0].display()}>"
            case TypeKind.TUPLE:
                return f"tuple<{', '.join(p.display() for p in self.params)}>"
            case TypeKind.VARIANT:
                return f"variant<{', '.join(p.display() for p in self.params)}>"
            case TypeKind.NAMED:
                return self.qname.to_id() if self.qname else "unknown"


# ============================================================================
# 4. Operator Registry Architecture
# ============================================================================


@beartype
class OperatorSpec(abc.ABC):
    @property
    @abc.abstractmethod
    def name(self) -> str: ...

    @property
    @abc.abstractmethod
    def category(self) -> str: ...

    @property
    def min_args(self) -> int:
        return 0

    @property
    def max_args(self) -> int:
        return 999

    def to_souffle_fact(self) -> Tuple[str, str, int, int]:
        return (self.name, self.category, self.min_args, self.max_args)


@beartype
class StandardOpSpec(OperatorSpec):
    def __init__(
        self,
        name: str,
        category: str = "standard",
        min_args: int = 0,
        max_args: int = 99,
    ) -> None:
        self._name = name
        self._category = category
        self._min_args = min_args
        self._max_args = max_args

    @property
    def name(self) -> str:
        return self._name

    @property
    def category(self) -> str:
        return self._category

    @property
    def min_args(self) -> int:
        return self._min_args

    @property
    def max_args(self) -> int:
        return self._max_args


def get_default_operators() -> List[OperatorSpec]:
    return [
        StandardOpSpec("this", "context", 0, 0),
        StandardOpSpec("iter", "context", 0, 0),
        StandardOpSpec("var_name", "meta", 0, 0),
        StandardOpSpec("const_int", "literal", 0, 0),
        StandardOpSpec("const_float", "literal", 0, 0),
        StandardOpSpec("const_str", "literal", 0, 0),
        StandardOpSpec("const_bool", "literal", 0, 0),
        StandardOpSpec("get_field", "member", 1, 1),
        StandardOpSpec("eq", "comparison", 2, 2),
        StandardOpSpec("ne", "comparison", 2, 2),
        StandardOpSpec("lt", "comparison", 2, 2),
        StandardOpSpec("le", "comparison", 2, 2),
        StandardOpSpec("and", "logical", 1, 99),
        StandardOpSpec("or", "logical", 1, 99),
        StandardOpSpec("not", "logical", 1, 1),
        StandardOpSpec("str_size", "string", 1, 1),
        StandardOpSpec("str_regex", "string", 1, 1),
        StandardOpSpec("str_format", "string", 1, 99),
        StandardOpSpec("list_size", "collection", 1, 1),
        StandardOpSpec("list_slice", "collection", 3, 3),
        StandardOpSpec("map_size", "map", 1, 1),
        StandardOpSpec("map_contains", "map", 2, 2),
        StandardOpSpec("map_get", "map", 2, 2),
        StandardOpSpec("opt_has_value", "optional", 1, 1),
        StandardOpSpec("opt_value", "optional", 1, 1),
        StandardOpSpec("case_is", "variant", 1, 1),
        StandardOpSpec("case_which", "variant", 1, 1),
        StandardOpSpec("enum_is", "enum", 1, 1),
        StandardOpSpec("nth_get", "tuple", 1, 1),
        StandardOpSpec("is_derived", "type", 1, 1),
        StandardOpSpec("typeof", "type", 1, 1),
        StandardOpSpec("loc_path", "located", 1, 1),
        StandardOpSpec("loc_val", "located", 1, 1),
        StandardOpSpec("rec_nodes", "traversal", 1, 1),
        StandardOpSpec("filter_some", "collection", 1, 1),
        StandardOpSpec("all", "quantifier", 2, 2),
        StandardOpSpec("exists", "quantifier", 2, 2),
        StandardOpSpec("exists_one", "quantifier", 2, 2),
        StandardOpSpec("all_rec", "quantifier", 2, 2),
        StandardOpSpec("map", "transform", 2, 2),
        StandardOpSpec("filter", "transform", 2, 2),
        StandardOpSpec("group_by", "transform", 2, 2),
        StandardOpSpec("lfold", "fold", 4, 4),
        StandardOpSpec("type_cond", "type_cond", 1, 1),
    ]


# ============================================================================
# 5. Pure Expression IR & Frontend Accessor Layer
# ============================================================================


@beartype
class TypeCaseBranchIR(BaseModel):
    raw_type: Any
    resolved_type: Optional[DslType] = None
    var_name: str
    body: ExprIR


@beartype
class ExprIR(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    id: int
    op: str
    args: List[ExprIR] = PydanticField(default_factory=list)
    str_val: Optional[str] = None
    int_val: Optional[int] = None
    float_val: Optional[float] = None
    bool_val: Optional[bool] = None
    raw_type_val: Optional[Any] = None
    type_val: Optional[DslType] = None
    type_branches: List[TypeCaseBranchIR] = PydanticField(default_factory=list)
    loc: SourceLoc

    def render_summary(self) -> str:
        match self.op:
            case "this":
                return "M.This"
            case "iter":
                return f'M.Iter("{self.str_val}")'
            case "get_field":
                return f'{self.args[0].render_summary()}["{self.str_val}"]'
            case "case_is":
                return (
                    f'{self.args[0].render_summary()}._Case.is_case("{self.str_val}")'
                )
            case "enum_is":
                return f'{self.args[0].render_summary()}._Enum.is_val("{self.str_val}")'
            case (
                "all"
                | "exists"
                | "exists_one"
                | "map"
                | "filter"
                | "group_by"
                | "all_rec"
            ):
                return f'op_{self.op}({self.args[0].render_summary()}, "{self.str_val}", ...)'
            case "and" | "or":
                return (
                    f"op_{self.op}({', '.join(a.render_summary() for a in self.args)})"
                )
            case _:
                return f"{self.op}(...)"


_EXPR_COUNTER = 0


def _alloc_expr_id() -> int:
    global _EXPR_COUNTER
    val = _EXPR_COUNTER
    _EXPR_COUNTER += 1
    return val


@beartype
def build_expr(
    op: str,
    args: Sequence[Expr] = (),
    *,
    str_val: Optional[str] = None,
    int_val: Optional[int] = None,
    float_val: Optional[float] = None,
    bool_val: Optional[bool] = None,
    raw_type_val: Optional[Any] = None,
    type_branches: Sequence[TypeCaseBranchIR] = (),
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    loc = _loc or capture_caller_loc()
    ir = ExprIR(
        id=_alloc_expr_id(),
        op=op,
        args=[a.ir for a in args],
        str_val=str_val,
        int_val=int_val,
        float_val=float_val,
        bool_val=bool_val,
        raw_type_val=raw_type_val,
        type_branches=list(type_branches),
        loc=loc,
    )
    return Expr(ir)


@beartype
def coerce_expr(val: Any, _loc: Optional[SourceLoc] = None) -> Expr:
    loc = _loc or capture_caller_loc()
    match val:
        case Expr():
            return val
        case bool():
            return build_expr("const_bool", bool_val=val, _loc=loc)
        case int():
            return build_expr("const_int", int_val=val, _loc=loc)
        case float():
            return build_expr("const_float", float_val=val, _loc=loc)
        case str():
            return build_expr("const_str", str_val=val, _loc=loc)
        case _:
            raise TypeError(f"Cannot convert {val!r} of type {type(val)} into Expr")


@beartype
class StrAccessor:
    def __init__(self, target: Optional[Expr] = None) -> None:
        self.target = target

    @dsl_entry
    def size(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        if self.target is None:
            raise ValueError("size() requires an instance target")
        return build_expr("str_size", [self.target], _loc=_loc)

    @dsl_entry
    def matches(self, pattern: str, *, _loc: Optional[SourceLoc] = None) -> Expr:
        if self.target is None:
            raise ValueError("matches() requires an instance target")
        return build_expr("str_regex", [self.target], str_val=pattern, _loc=_loc)

    @dsl_entry
    def format(self, args: Sequence[Any], *, _loc: Optional[SourceLoc] = None) -> Expr:
        if len(args) == 0:
            raise ValueError("format() requires at least a format string")
        wrapped = [coerce_expr(a, _loc=_loc) for a in args]
        return build_expr("str_format", wrapped, _loc=_loc)


@beartype
class ListAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @dsl_entry
    def size(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("list_size", [self.target], _loc=_loc)

    @dsl_entry
    def slice(self, start: Any, end: Any, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr(
            "list_slice",
            [
                self.target,
                coerce_expr(start, _loc=_loc),
                coerce_expr(end, _loc=_loc),
            ],
            _loc=_loc,
        )

    @dsl_entry
    def filter_some(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("filter_some", [self.target], _loc=_loc)


@beartype
class MapAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @dsl_entry
    def size(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("map_size", [self.target], _loc=_loc)

    @dsl_entry
    def contains(self, key: Any, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr(
            "map_contains",
            [self.target, coerce_expr(key, _loc=_loc)],
            _loc=_loc,
        )

    @dsl_entry
    def get(self, key: Any, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr(
            "map_get",
            [self.target, coerce_expr(key, _loc=_loc)],
            _loc=_loc,
        )


@beartype
class OptAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @dsl_entry
    def has_value(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("opt_has_value", [self.target], _loc=_loc)

    @dsl_entry
    def value(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("opt_value", [self.target], _loc=_loc)


@beartype
class CaseAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @dsl_entry
    def is_case(self, name: str, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("case_is", [self.target], str_val=name, _loc=_loc)

    @dsl_entry
    def which_case(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("case_which", [self.target], _loc=_loc)


@beartype
class EnumAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @dsl_entry
    def is_val(self, name: str, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("enum_is", [self.target], str_val=name, _loc=_loc)


@beartype
class NthAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @dsl_entry
    def get(self, index: int, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("nth_get", [self.target], int_val=index, _loc=_loc)


@beartype
class LocatedAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @dsl_entry
    def path(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("loc_path", [self.target], _loc=_loc)

    @dsl_entry
    def value(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("loc_val", [self.target], _loc=_loc)


class ExprAccessorMixin:
    """Separate inheritance layer for domain-specific accessors on Expr."""

    @property
    def _Str(self) -> StrAccessor:
        return StrAccessor(self)  # type: ignore[arg-type]

    @property
    def _List(self) -> ListAccessor:
        return ListAccessor(self)  # type: ignore[arg-type]

    @property
    def _Map(self) -> MapAccessor:
        return MapAccessor(self)  # type: ignore[arg-type]

    @property
    def _Opt(self) -> OptAccessor:
        return OptAccessor(self)  # type: ignore[arg-type]

    @property
    def _Case(self) -> CaseAccessor:
        return CaseAccessor(self)  # type: ignore[arg-type]

    @property
    def _Enum(self) -> EnumAccessor:
        return EnumAccessor(self)  # type: ignore[arg-type]

    @property
    def _Nth(self) -> NthAccessor:
        return NthAccessor(self)  # type: ignore[arg-type]

    @property
    def _Loc(self) -> LocatedAccessor:
        return LocatedAccessor(self)  # type: ignore[arg-type]

    @dsl_entry
    def is_derived_from(self, target: Any, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr(
            "is_derived",
            [self],
            raw_type_val=target,
            _loc=_loc,  # type: ignore[list-item]
        )


@beartype
class Expr(ExprAccessorMixin):
    """Frontend wrapper around pure ExprIR. Overloads operators and forbids bool()."""

    def __init__(self, ir: ExprIR) -> None:
        self.ir = ir

    def __bool__(self) -> bool:
        raise TypeError(
            "DSL Expr cannot be converted to Python bool. "
            "Use op_and, op_or, op_not, or bitwise &, |, ~ instead of Python 'and', 'or', 'not'."
        )

    @dsl_entry
    def __eq__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:  # type: ignore[override]
        return build_expr("eq", [self, coerce_expr(other, _loc=_loc)], _loc=_loc)

    @dsl_entry
    def __ne__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:  # type: ignore[override]
        return build_expr("ne", [self, coerce_expr(other, _loc=_loc)], _loc=_loc)

    @dsl_entry
    def __lt__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("lt", [self, coerce_expr(other, _loc=_loc)], _loc=_loc)

    @dsl_entry
    def __le__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("le", [self, coerce_expr(other, _loc=_loc)], _loc=_loc)

    @dsl_entry
    def __gt__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("lt", [coerce_expr(other, _loc=_loc), self], _loc=_loc)

    @dsl_entry
    def __ge__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("le", [coerce_expr(other, _loc=_loc), self], _loc=_loc)

    @dsl_entry
    def __and__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("and", [self, coerce_expr(other, _loc=_loc)], _loc=_loc)

    @dsl_entry
    def __or__(self, other: object, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("or", [self, coerce_expr(other, _loc=_loc)], _loc=_loc)

    @dsl_entry
    def __invert__(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("not", [self], _loc=_loc)

    @dsl_entry
    def __getitem__(self, key: str, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("get_field", [self], str_val=key, _loc=_loc)


# ============================================================================
# 6. Top-level Combinators & Type Condition Syntax
# ============================================================================


@beartype
@dsl_entry
def op_and(*args: Any, _loc: Optional[SourceLoc] = None) -> Expr:
    if len(args) == 0:
        raise ValueError("op_and requires at least one argument")
    return build_expr("and", [coerce_expr(a, _loc=_loc) for a in args], _loc=_loc)


@beartype
@dsl_entry
def op_or(*args: Any, _loc: Optional[SourceLoc] = None) -> Expr:
    if len(args) == 0:
        raise ValueError("op_or requires at least one argument")
    return build_expr("or", [coerce_expr(a, _loc=_loc) for a in args], _loc=_loc)


@beartype
@dsl_entry
def op_not(arg: Any, *, _loc: Optional[SourceLoc] = None) -> Expr:
    return build_expr("not", [coerce_expr(arg, _loc=_loc)], _loc=_loc)


@beartype
@dsl_entry
def op_all(
    collection: Expr,
    var_name: str,
    predicate: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    return build_expr("all", [collection, predicate], str_val=var_name, _loc=_loc)


@beartype
@dsl_entry
def op_exists(
    collection: Expr,
    var_name: str,
    predicate: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    return build_expr("exists", [collection, predicate], str_val=var_name, _loc=_loc)


@beartype
@dsl_entry
def op_exists_one(
    collection: Expr,
    var_name: str,
    predicate: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    return build_expr(
        "exists_one", [collection, predicate], str_val=var_name, _loc=_loc
    )


@beartype
@dsl_entry
def op_map(
    collection: Expr,
    var_name: str,
    transform: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    return build_expr("map", [collection, transform], str_val=var_name, _loc=_loc)


@beartype
@dsl_entry
def op_filter(
    collection: Expr,
    var_name: str,
    predicate: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    return build_expr("filter", [collection, predicate], str_val=var_name, _loc=_loc)


@beartype
@dsl_entry
def op_group_by(
    collection: Expr,
    var_name: str,
    key_expr: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    return build_expr("group_by", [collection, key_expr], str_val=var_name, _loc=_loc)


@beartype
@dsl_entry
def op_lfold(
    collection: Expr,
    iter_var: str,
    acc_var: str,
    init_val: Any,
    body: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    acc_node = build_expr("var_name", str_val=acc_var, _loc=_loc)
    return build_expr(
        "lfold",
        [collection, coerce_expr(init_val, _loc=_loc), body, acc_node],
        str_val=iter_var,
        _loc=_loc,
    )


@beartype
@dsl_entry
def op_rec_nodes(root: Expr, *, _loc: Optional[SourceLoc] = None) -> Expr:
    return build_expr("rec_nodes", [root], _loc=_loc)


@beartype
@dsl_entry
def op_all_rec(
    root: Expr,
    var_name: str,
    predicate: Expr,
    *,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    return build_expr("all_rec", [root, predicate], str_val=var_name, _loc=_loc)


@beartype
@dsl_entry
def op_typeof(target: Expr, *, _loc: Optional[SourceLoc] = None) -> Expr:
    return build_expr("typeof", [target], _loc=_loc)


@beartype
class TypeCaseSpec:
    def __init__(self, match_type: Any, var_name: str, body: Expr) -> None:
        self.match_type = match_type
        self.var_name = var_name
        self.body = body


@beartype
def type_case(match_type: Any, var_name: str, body: Any) -> TypeCaseSpec:
    return TypeCaseSpec(match_type, var_name, coerce_expr(body))


@beartype
@dsl_entry
def type_cond(
    target: Expr,
    *cases: TypeCaseSpec,
    unhandled_ignore: bool = False,
    _loc: Optional[SourceLoc] = None,
) -> Expr:
    if len(cases) == 0:
        raise ValueError("type_cond requires at least one type_case")
    branches = [
        TypeCaseBranchIR(
            raw_type=c.match_type,
            var_name=c.var_name,
            body=c.body.ir,
        )
        for c in cases
    ]
    return build_expr(
        "type_cond",
        [target],
        int_val=1 if unhandled_ignore else 0,
        type_branches=branches,
        _loc=_loc,
    )


# ============================================================================
# 7. Scalar Types & Declarations IR
# ============================================================================


class Int8:
    pass


class Int16:
    pass


class Int32:
    pass


class Int64:
    pass


class UInt8:
    pass


class UInt16:
    pass


class UInt32:
    pass


class UInt64:
    pass


class Float32:
    pass


class Float64:
    pass


@beartype
class FixedArrayMeta:
    def __init__(self, elem_type: Any, size: int) -> None:
        self.elem_type = elem_type
        self.size = size


class FixedArrayFactory:
    def __getitem__(self, params: Tuple[Any, int]) -> FixedArrayMeta:
        elem_type, size = params
        return FixedArrayMeta(elem_type, size)


FixedArray = FixedArrayFactory()


@beartype
class EnFieldAnnotation(BaseModel):
    doc: str = ""


@beartype
class VariantMarker(BaseModel):
    enum_name: Optional[str] = None


_NO_DEFAULT = object()


@beartype
class FieldSpec(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    doc: str = ""
    default: Any = _NO_DEFAULT
    check: Optional[ExprIR] = None
    loc: SourceLoc

    @property
    def has_default(self) -> bool:
        return self.default is not _NO_DEFAULT


@beartype
class VariantDeclIR(BaseModel):
    field_name: str = "data"
    enum_name: str = "Kind"
    cases: List[DslType] = PydanticField(default_factory=list)
    loc: SourceLoc


@beartype
class CheckDef(BaseModel):
    id: str = ""
    level: CheckLevel = CheckLevel.ERROR
    cond: ExprIR
    message: ExprIR
    loc: SourceLoc


@beartype
class MethodArgDef(BaseModel):
    name: str
    type: DslType
    default: Optional[Any] = None
    is_vararg: bool = False
    is_kwarg: bool = False


@beartype
class MethodDef(BaseModel):
    name: str
    args: List[MethodArgDef] = PydanticField(default_factory=list)
    return_type: DslType
    type_params: List[str] = PydanticField(default_factory=list)
    is_static: bool = False
    is_constructor: bool = False
    backend: Dict[str, Any] = PydanticField(default_factory=dict)
    doc: str = ""
    loc: SourceLoc


@beartype
class FieldDef(BaseModel):
    name: str
    type: DslType
    doc: str = ""
    has_default: bool = False
    default: Optional[Any] = None
    check: Optional[ExprIR] = None
    is_inline_variant: bool = False
    inline_enum_name: Optional[str] = None
    inline_cases: List[DslType] = PydanticField(default_factory=list)
    loc: SourceLoc


@beartype
class EnumValueDef(BaseModel):
    name: str
    value: int
    doc: str = ""
    loc: SourceLoc


@beartype
class RecordDef(BaseModel):
    kind: RecordKind
    name: QName
    doc: str = ""
    bases: List[QName] = PydanticField(default_factory=list)
    fields: List[FieldDef] = PydanticField(default_factory=list)
    enum_values: List[EnumValueDef] = PydanticField(default_factory=list)
    nested: List[RecordDef] = PydanticField(default_factory=list)
    variants: List[VariantDeclIR] = PydanticField(default_factory=list)
    methods: List[MethodDef] = PydanticField(default_factory=list)
    checks: List[CheckDef] = PydanticField(default_factory=list)
    equality_fields: Optional[List[str]] = None
    hash_fields: Optional[List[str]] = None
    backend: Dict[str, Any] = PydanticField(default_factory=dict)
    loc: SourceLoc


# ============================================================================
# 8. Frame-local Class Directives (No Global Context Stack)
# ============================================================================


@beartype
class RawVariantDirective:
    def __init__(
        self,
        cases: Sequence[Any],
        field_name: str,
        enum_name: str,
        loc: SourceLoc,
    ) -> None:
        self.cases = list(cases)
        self.field_name = field_name
        self.enum_name = enum_name
        self.loc = loc


@beartype
class RawCheckDirective:
    def __init__(
        self,
        id: str,
        level: CheckLevel,
        cond: ExprIR,
        message: ExprIR,
        loc: SourceLoc,
    ) -> None:
        self.id = id
        self.level = level
        self.cond = cond
        self.message = message
        self.loc = loc


@beartype
class ClassDirectives:
    def __init__(self) -> None:
        self.variants: List[RawVariantDirective] = []
        self.checks: List[RawCheckDirective] = []
        self.backend: Dict[str, Any] = {}


def _get_enclosing_class_directives() -> ClassDirectives:
    frame = sys._getframe(2)
    f_locals = frame.f_locals
    key = DslAttr.DIRECTIVES.value
    if key not in f_locals or not isinstance(f_locals[key], ClassDirectives):
        raise RuntimeError(
            "DSL directive (__variant__, __check__, __config__) called outside of a DSL class body"
        )
    return f_locals[key]


@beartype
@dsl_entry
def __variant__(
    *cases: Any,
    field_name: str = "data",
    enum_name: str = "Kind",
    _loc: Optional[SourceLoc] = None,
) -> None:
    directives = _get_enclosing_class_directives()
    directives.variants.append(
        RawVariantDirective(
            cases=cases,
            field_name=field_name,
            enum_name=enum_name,
            loc=_loc or capture_caller_loc(),
        )
    )


@beartype
@dsl_entry
def __check__(
    cond: Any,
    message: Union[str, Expr],
    id: str = "",
    level: Union[str, CheckLevel] = CheckLevel.ERROR,
    _loc: Optional[SourceLoc] = None,
) -> None:
    loc = _loc or capture_caller_loc()
    directives = _get_enclosing_class_directives()
    chk_level = CheckLevel(level) if isinstance(level, str) else level
    directives.checks.append(
        RawCheckDirective(
            id=id,
            level=chk_level,
            cond=coerce_expr(cond, _loc=loc).ir,
            message=coerce_expr(message, _loc=loc).ir,
            loc=loc,
        )
    )


@beartype
def __config__(backend: Dict[str, Any] = {}) -> None:
    directives = _get_enclosing_class_directives()
    directives.backend.update(backend)


# ============================================================================
# 9. AST Parsing for __eq__ and __hash__ & Unresolved Capture
# ============================================================================


@beartype
def parse_eq_method_ast(func: Callable[..., Any]) -> Optional[List[str]]:
    """
    If body is '...' or 'pass', returns None (meaning compare all fields).
    Otherwise enforces: return (self.f1, self.f2, ...) == (other.f1, other.f2, ...)
    """
    try:
        src = textwrap.dedent(inspect.getsource(func))
        tree = ast.parse(src)
    except OSError:
        return None

    if not tree.body or not isinstance(tree.body[0], ast.FunctionDef):
        return None
    fdef = tree.body[0]
    stmts = [
        s
        for s in fdef.body
        if not (isinstance(s, ast.Expr) and isinstance(s.value, ast.Constant))
    ]
    if len(stmts) == 1:
        stmt = stmts[0]
        if isinstance(stmt, ast.Pass) or (
            isinstance(stmt, ast.Expr)
            and isinstance(stmt.value, ast.Constant)
            and stmt.value.value is Ellipsis
        ):
            return []

        if isinstance(stmt, ast.Return) and isinstance(stmt.value, ast.Compare):
            cmp = stmt.value
            if (
                len(cmp.ops) == 1
                and isinstance(cmp.ops[0], ast.Eq)
                and len(cmp.comparators) == 1
                and isinstance(cmp.left, ast.Tuple)
                and isinstance(cmp.comparators[0], ast.Tuple)
            ):
                left_fields: List[str] = []
                right_fields: List[str] = []
                for elt in cmp.left.elts:
                    if (
                        isinstance(elt, ast.Attribute)
                        and isinstance(elt.value, ast.Name)
                        and elt.value.id == "self"
                    ):
                        left_fields.append(elt.attr)
                    else:
                        raise ValueError(
                            f"Invalid __eq__ tuple element in {func.__qualname__}: expected self.<field>"
                        )
                for elt in cmp.comparators[0].elts:
                    if (
                        isinstance(elt, ast.Attribute)
                        and isinstance(elt.value, ast.Name)
                        and elt.value.id == "other"
                    ):
                        right_fields.append(elt.attr)
                    else:
                        raise ValueError(
                            f"Invalid __eq__ comparator element in {func.__qualname__}: expected other.<field>"
                        )
                if left_fields != right_fields:
                    raise ValueError(
                        f"__eq__ field mismatch in {func.__qualname__}: {left_fields} != {right_fields}"
                    )
                return left_fields

    raise ValueError(
        f"Unsupported __eq__ body in {func.__qualname__}. "
        "Allowed syntax: '...' or 'return (self.f1, ...) == (other.f1, ...)'"
    )


@beartype
def parse_hash_method_ast(func: Callable[..., Any]) -> List[str]:
    """Enforces: return hash((self.f1, self.f2, ...))"""
    src = textwrap.dedent(inspect.getsource(func))
    tree = ast.parse(src)
    if not tree.body or not isinstance(tree.body[0], ast.FunctionDef):
        raise ValueError(f"Could not parse __hash__ in {func.__qualname__}")
    fdef = tree.body[0]
    stmts = [
        s
        for s in fdef.body
        if not (isinstance(s, ast.Expr) and isinstance(s.value, ast.Constant))
    ]
    if len(stmts) == 1 and isinstance(stmts[0], ast.Return):
        val = stmts[0].value
        if (
            isinstance(val, ast.Call)
            and isinstance(val.func, ast.Name)
            and val.func.id == "hash"
            and len(val.args) == 1
            and isinstance(val.args[0], ast.Tuple)
        ):
            fields: List[str] = []
            for elt in val.args[0].elts:
                if (
                    isinstance(elt, ast.Attribute)
                    and isinstance(elt.value, ast.Name)
                    and elt.value.id == "self"
                ):
                    fields.append(elt.attr)
                else:
                    raise ValueError(
                        f"Invalid __hash__ element in {func.__qualname__}: expected self.<field>"
                    )
            return fields

    raise ValueError(
        f"Unsupported __hash__ body in {func.__qualname__}. "
        "Allowed syntax: 'return hash((self.f1, self.f2, ...))'"
    )


@beartype
class MethodWrapper:
    def __init__(
        self, func: Callable[..., Any], backend: Dict[str, Any], loc: SourceLoc
    ) -> None:
        self.func = func
        self.backend = backend
        self.loc = loc


@beartype
class UnresolvedRecord:
    def __init__(
        self,
        cls_obj: type,
        kind: RecordKind,
        qname: QName,
        bases: List[type],
        namespace: Dict[str, Any],
        directives: ClassDirectives,
        loc: SourceLoc,
    ) -> None:
        self.cls_obj = cls_obj
        self.kind = kind
        self.qname = qname
        self.bases = bases
        self.namespace = dict(namespace)
        self.directives = directives
        self.loc = loc
        self.nested: List[UnresolvedRecord] = []


@beartype
def resolve_qualname_scope(qualname: str, package: str) -> Optional[QName]:
    parts = [p for p in qualname.split(".")[:-1] if p != "<locals>"]
    cur: Optional[QName] = None
    for part in parts:
        cur = QName(name=part, scope=cur, package=package)
    return cur


class DslMeta(type):
    @classmethod
    def __prepare__(
        mcs, name: str, bases: Tuple[type, ...], **kwargs: Any
    ) -> Dict[str, Any]:
        return {DslAttr.DIRECTIVES.value: ClassDirectives()}

    def __new__(
        mcs,
        name: str,
        bases: Tuple[type, ...],
        namespace: Dict[str, Any],
        **kwargs: Any,
    ) -> DslMeta:
        directives: ClassDirectives = namespace.pop(
            DslAttr.DIRECTIVES.value, ClassDirectives()
        )
        cls_obj = super().__new__(mcs, name, bases, namespace)

        if namespace.get(DslAttr.ROOT.value, False):
            return cls_obj

        model: DslModel = getattr(cls_obj, DslAttr.MODEL.value)
        rec_kind: RecordKind = getattr(cls_obj, DslAttr.KIND.value)
        qualname = namespace.get("__qualname__", name)
        scope = resolve_qualname_scope(qualname, model.package)
        qname = QName(name=name, scope=scope, package=model.package)
        setattr(cls_obj, DslAttr.QNAME.value, qname)

        loc = capture_caller_loc()
        try:
            _, line = inspect.getsourcelines(cls_obj)
            file = inspect.getsourcefile(cls_obj) or loc.file
            loc = SourceLoc(file=file, line=line, function=name)
        except OSError:
            pass

        dsl_bases = [
            b
            for b in bases
            if DslAttr.ROOT.value not in b.__dict__ and hasattr(b, DslAttr.QNAME.value)
        ]

        unres = UnresolvedRecord(
            cls_obj=cls_obj,
            kind=rec_kind,
            qname=qname,
            bases=dsl_bases,
            namespace=namespace,
            directives=directives,
            loc=loc,
        )

        for attr_val in namespace.values():
            if isinstance(attr_val, DslMeta) and hasattr(
                attr_val, DslAttr.UNRESOLVED.value
            ):
                child_unres: UnresolvedRecord = getattr(
                    attr_val, DslAttr.UNRESOLVED.value
                )
                unres.nested.append(child_unres)

        setattr(cls_obj, DslAttr.UNRESOLVED.value, unres)
        model._register_unresolved(unres, is_top_level=(scope is None))
        return cls_obj


# ============================================================================
# 10. Two-Phase Name & Type Resolution
# ============================================================================


@beartype
class ExternalTypeLookup:
    def __init__(self, package: str) -> None:
        self.package = package

    def __getitem__(self, type_path: str) -> DslType:
        parts = type_path.split(".")
        cur_scope: Optional[QName] = None
        for part in parts[:-1]:
            cur_scope = QName(
                name=part,
                scope=cur_scope,
                package=self.package,
                is_external=True,
            )
        qn = QName(
            name=parts[-1],
            scope=cur_scope,
            package=self.package,
            is_external=True,
        )
        return DslType(kind=TypeKind.NAMED, qname=qn)


@beartype
class DslValidationError(Exception):
    pass


@beartype
class DslModel:
    def __init__(
        self,
        package: str = "",
        operators: Optional[Sequence[OperatorSpec]] = None,
    ) -> None:
        self.package = package
        self.operators: List[OperatorSpec] = (
            list(operators) if operators is not None else get_default_operators()
        )
        self._unresolved_top: List[UnresolvedRecord] = []
        self._qname_table: Dict[str, QName] = {}
        self.records: List[RecordDef] = []
        self._name_collisions: List[Tuple[str, str, str, int]] = []
        self._finalized = False

        self._T = ExternalTypeLookup(package)
        self._Str = StrAccessor(None)

        class ClassBase(metaclass=DslMeta):
            __dsl_root__: ClassVar[bool] = True
            __dsl_model__: ClassVar[DslModel] = self
            __dsl_kind__: ClassVar[RecordKind] = RecordKind.CLASS

        class CaseBase(metaclass=DslMeta):
            __dsl_root__: ClassVar[bool] = True
            __dsl_model__: ClassVar[DslModel] = self
            __dsl_kind__: ClassVar[RecordKind] = RecordKind.CASE

        class EnumBase(metaclass=DslMeta):
            __dsl_root__: ClassVar[bool] = True
            __dsl_model__: ClassVar[DslModel] = self
            __dsl_kind__: ClassVar[RecordKind] = RecordKind.ENUM

        self.Class = ClassBase
        self.Case = CaseBase
        self.Enum = EnumBase

    def _register_unresolved(self, unres: UnresolvedRecord, is_top_level: bool) -> None:
        self._finalized = False
        dotted = self._qname_to_dotted(unres.qname)
        self._qname_table[dotted] = unres.qname
        if is_top_level:
            self._unresolved_top.append(unres)

    @staticmethod
    def _qname_to_dotted(qn: QName) -> str:
        if qn.scope is None:
            return qn.name
        return f"{DslModel._qname_to_dotted(qn.scope)}.{qn.name}"

    def resolve_lexical_qname(
        self, dotted_name: str, current_scope: Optional[QName]
    ) -> QName:
        scope_chain: List[str] = []
        cur = current_scope
        while cur is not None:
            scope_chain.append(self._qname_to_dotted(cur))
            cur = cur.scope

        for prefix in scope_chain:
            candidate = f"{prefix}.{dotted_name}"
            if candidate in self._qname_table:
                return self._qname_table[candidate]
            if prefix.endswith(f".{dotted_name}") or prefix == dotted_name:
                if prefix in self._qname_table:
                    return self._qname_table[prefix]

        if dotted_name in self._qname_table:
            return self._qname_table[dotted_name]

        parts = dotted_name.split(".")
        sc: Optional[QName] = None
        for p in parts[:-1]:
            sc = QName(name=p, scope=sc, package=self.package)
        return QName(name=parts[-1], scope=sc, package=self.package)

    def resolve_type(
        self,
        ann: Any,
        current_scope: Optional[QName],
        type_vars: Sequence[str] = (),
    ) -> DslType:
        match ann:
            case DslType():
                return ann
            case QName():
                return DslType(kind=TypeKind.NAMED, qname=ann)
            case FixedArrayMeta():
                return DslType(
                    kind=TypeKind.ARRAY,
                    params=[self.resolve_type(ann.elem_type, current_scope, type_vars)],
                    fixed_size=ann.size,
                )
            case None:
                return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.NONE)
            case ForwardRef():
                return self.resolve_type(ann.__forward_arg__, current_scope, type_vars)
            case str():
                if ann in type_vars:
                    return DslType(kind=TypeKind.NAMED, qname=QName(name=ann))
                try:
                    parsed = ast.parse(ann, mode="eval").body
                    return self._resolve_ast_type(parsed, current_scope, type_vars)
                except SyntaxError:
                    qn = self.resolve_lexical_qname(ann, current_scope)
                    return DslType(kind=TypeKind.NAMED, qname=qn)

        if ann is types.NoneType:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.NONE)
        if ann is int or ann is Int64:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.INT64)
        if ann is Int8:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.INT8)
        if ann is Int16:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.INT16)
        if ann is Int32:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.INT32)
        if ann is UInt8:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.UINT8)
        if ann is UInt16:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.UINT16)
        if ann is UInt32:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.UINT32)
        if ann is UInt64:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.UINT64)
        if ann is float or ann is Float64:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.FLOAT64)
        if ann is Float32:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.FLOAT32)
        if ann is str:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.STRING)
        if ann is bool:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.BOOL)
        if ann is Any:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.ANY)
        if ann is ObjectPath:
            return DslType(kind=TypeKind.PRIMITIVE, prim=PrimName.PATH)

        if hasattr(ann, DslAttr.QNAME.value):
            qn = getattr(ann, DslAttr.QNAME.value)
            return DslType(kind=TypeKind.NAMED, qname=qn)

        if hasattr(ann, "__name__") and ann.__name__ in type_vars:
            return DslType(kind=TypeKind.NAMED, qname=QName(name=ann.__name__))

        origin = get_origin(ann)
        args = get_args(ann)

        if origin is Annotated:
            return self.resolve_type(args[0], current_scope, type_vars)

        if origin is list or origin is List:
            return DslType(
                kind=TypeKind.LIST,
                params=[self.resolve_type(args[0], current_scope, type_vars)],
            )

        if origin is dict or origin is Dict:
            return DslType(
                kind=TypeKind.MAP,
                params=[
                    self.resolve_type(args[0], current_scope, type_vars),
                    self.resolve_type(args[1], current_scope, type_vars),
                ],
            )

        if origin is tuple or origin is Tuple:
            return DslType(
                kind=TypeKind.TUPLE,
                params=[self.resolve_type(a, current_scope, type_vars) for a in args],
            )

        if origin is Union or origin is types.UnionType:
            non_none = [a for a in args if a is not types.NoneType]
            if len(non_none) == 1 and len(args) == 2:
                return DslType(
                    kind=TypeKind.OPTIONAL,
                    params=[self.resolve_type(non_none[0], current_scope, type_vars)],
                )
            return DslType(
                kind=TypeKind.VARIANT,
                params=[self.resolve_type(a, current_scope, type_vars) for a in args],
            )

        raise TypeError(f"Unsupported type annotation: {ann!r}")

    def _resolve_ast_type(
        self,
        node: ast.AST,
        current_scope: Optional[QName],
        type_vars: Sequence[str],
    ) -> DslType:
        if isinstance(node, ast.Name):
            if node.id in type_vars:
                return DslType(kind=TypeKind.NAMED, qname=QName(name=node.id))
            mapping: Dict[str, Any] = {
                "int": Int64,
                "float": Float64,
                "str": str,
                "bool": bool,
                "None": None,
                "Any": Any,
                "Int8": Int8,
                "Int16": Int16,
                "Int32": Int32,
                "Int64": Int64,
                "UInt8": UInt8,
                "UInt16": UInt16,
                "UInt32": UInt32,
                "UInt64": UInt64,
                "Float32": Float32,
                "Float64": Float64,
            }
            if node.id in mapping:
                return self.resolve_type(mapping[node.id], current_scope, type_vars)
            qn = self.resolve_lexical_qname(node.id, current_scope)
            return DslType(kind=TypeKind.NAMED, qname=qn)

        if isinstance(node, ast.Attribute):
            parts: List[str] = []
            cur: ast.AST = node
            while isinstance(cur, ast.Attribute):
                parts.append(cur.attr)
                cur = cur.value
            if isinstance(cur, ast.Name):
                parts.append(cur.id)
                dotted = ".".join(reversed(parts))
                qn = self.resolve_lexical_qname(dotted, current_scope)
                return DslType(kind=TypeKind.NAMED, qname=qn)

        if isinstance(node, ast.Subscript) and isinstance(node.value, ast.Name):
            head = node.value.id
            slice_node = node.slice
            elts = (
                slice_node.elts if isinstance(slice_node, ast.Tuple) else [slice_node]
            )
            if head in ("List", "list"):
                return DslType(
                    kind=TypeKind.LIST,
                    params=[self._resolve_ast_type(elts[0], current_scope, type_vars)],
                )
            if head in ("Optional",):
                return DslType(
                    kind=TypeKind.OPTIONAL,
                    params=[self._resolve_ast_type(elts[0], current_scope, type_vars)],
                )
            if head in ("Dict", "dict"):
                return DslType(
                    kind=TypeKind.MAP,
                    params=[
                        self._resolve_ast_type(elts[0], current_scope, type_vars),
                        self._resolve_ast_type(elts[1], current_scope, type_vars),
                    ],
                )
            if head in ("Tuple", "tuple"):
                return DslType(
                    kind=TypeKind.TUPLE,
                    params=[
                        self._resolve_ast_type(e, current_scope, type_vars)
                        for e in elts
                    ],
                )

        raise TypeError(f"Unsupported string AST type node: {ast.dump(node)}")

    def _resolve_expr_types(self, expr: ExprIR, current_scope: QName) -> None:
        if expr.raw_type_val is not None and expr.type_val is None:
            expr.type_val = self.resolve_type(expr.raw_type_val, current_scope)
        for br in expr.type_branches:
            if br.resolved_type is None:
                br.resolved_type = self.resolve_type(br.raw_type, current_scope)
            self._resolve_expr_types(br.body, current_scope)
        for arg in expr.args:
            self._resolve_expr_types(arg, current_scope)

    def _finalize_record(self, unres: UnresolvedRecord) -> RecordDef:
        qname = unres.qname
        namespace = unres.namespace
        loc = unres.loc

        base_qnames = [getattr(b, DslAttr.QNAME.value) for b in unres.bases]
        nested_defs = [self._finalize_record(n) for n in unres.nested]
        existing_nested_names = {n.name.name for n in nested_defs}
        existing_field_names: set[str] = set()

        annotations = namespace.get("__annotations__", {})
        fields: List[FieldDef] = []
        enum_values: List[EnumValueDef] = []

        if unres.kind == RecordKind.ENUM:
            for fname, fval in namespace.items():
                if fname.startswith("__") or isinstance(fval, DslMeta):
                    continue
                if not isinstance(fval, int):
                    continue
                edoc = ""
                if fname in annotations:
                    fann = annotations[fname]
                    if get_origin(fann) is Annotated:
                        for meta in get_args(fann)[1:]:
                            if isinstance(meta, EnFieldAnnotation):
                                edoc = meta.doc
                enum_values.append(
                    EnumValueDef(name=fname, value=fval, doc=edoc, loc=loc)
                )
        else:
            for fname, fann in annotations.items():
                existing_field_names.add(fname)
                raw_default = namespace.get(fname, _NO_DEFAULT)
                fspec: Optional[FieldSpec] = None
                has_default = False
                default_val: Any = None
                floc = loc

                if isinstance(raw_default, FieldSpec):
                    fspec = raw_default
                    has_default = fspec.has_default
                    default_val = fspec.default if has_default else None
                    floc = fspec.loc
                elif raw_default is not _NO_DEFAULT:
                    has_default = True
                    default_val = raw_default

                is_inline_variant = False
                inline_enum_name: Optional[str] = None
                inline_cases: List[DslType] = []

                if get_origin(fann) is Annotated:
                    ann_args = get_args(fann)
                    for meta in ann_args[1:]:
                        if isinstance(meta, VariantMarker):
                            is_inline_variant = True
                            inline_enum_name = (
                                meta.enum_name or f"{fname.capitalize()}Kind"
                            )
                            union_args = get_args(ann_args[0])
                            for ua in union_args:
                                inline_cases.append(self.resolve_type(ua, qname))

                ftype = self.resolve_type(fann, qname)
                if fspec and fspec.check is not None:
                    self._resolve_expr_types(fspec.check, qname)

                fields.append(
                    FieldDef(
                        name=fname,
                        type=ftype,
                        doc=fspec.doc if fspec else "",
                        has_default=has_default,
                        default=default_val,
                        check=fspec.check if fspec else None,
                        is_inline_variant=is_inline_variant,
                        inline_enum_name=inline_enum_name,
                        inline_cases=inline_cases,
                        loc=floc,
                    )
                )

        # Synthesize __variant__ declarations, fields, and Kind enums
        variant_decls: List[VariantDeclIR] = []
        for vdir in unres.directives.variants:
            case_types = [self.resolve_type(c, qname) for c in vdir.cases]
            vtype = DslType(kind=TypeKind.VARIANT, params=case_types)

            if vdir.field_name in existing_field_names:
                self._name_collisions.append(
                    (
                        qname.to_id(),
                        vdir.field_name,
                        vdir.loc.file,
                        vdir.loc.line,
                    )
                )
            else:
                existing_field_names.add(vdir.field_name)
                fields.append(
                    FieldDef(
                        name=vdir.field_name,
                        type=vtype,
                        doc="Synthesized variant data field",
                        is_inline_variant=True,
                        inline_enum_name=vdir.enum_name,
                        inline_cases=case_types,
                        loc=vdir.loc,
                    )
                )

            if (
                vdir.enum_name in existing_nested_names
                or vdir.enum_name in existing_field_names
            ):
                self._name_collisions.append(
                    (
                        qname.to_id(),
                        vdir.enum_name,
                        vdir.loc.file,
                        vdir.loc.line,
                    )
                )
            else:
                existing_nested_names.add(vdir.enum_name)
                enum_qname = QName(
                    name=vdir.enum_name, scope=qname, package=self.package
                )
                synth_enum_vals = [
                    EnumValueDef(
                        name=ct.qname.name if ct.qname else f"Case{idx}",
                        value=idx,
                        loc=vdir.loc,
                    )
                    for idx, ct in enumerate(case_types)
                ]
                nested_defs.append(
                    RecordDef(
                        kind=RecordKind.ENUM,
                        name=enum_qname,
                        enum_values=synth_enum_vals,
                        loc=vdir.loc,
                    )
                )

            variant_decls.append(
                VariantDeclIR(
                    field_name=vdir.field_name,
                    enum_name=vdir.enum_name,
                    cases=case_types,
                    loc=vdir.loc,
                )
            )

        # Methods, __eq__, __hash__
        methods: List[MethodDef] = []
        eq_fields: Optional[List[str]] = None
        hash_fields: Optional[List[str]] = None

        for attr_name, attr_val in namespace.items():
            if attr_name == "__eq__" and inspect.isfunction(attr_val):
                parsed_eq = parse_eq_method_ast(attr_val)
                eq_fields = [f.name for f in fields] if parsed_eq == [] else parsed_eq
                continue
            if attr_name == "__hash__" and inspect.isfunction(attr_val):
                hash_fields = parse_hash_method_ast(attr_val)
                continue
            if attr_name.startswith("__") and attr_name != "__init__":
                continue
            mdef = self._parse_method(attr_name, attr_val, qname, namespace)
            if mdef is not None:
                methods.append(mdef)

        checks: List[CheckDef] = []
        for idx, rchk in enumerate(unres.directives.checks):
            self._resolve_expr_types(rchk.cond, qname)
            self._resolve_expr_types(rchk.message, qname)
            chk_id = rchk.id or f"{qname.to_id()}::check_{idx}"
            checks.append(
                CheckDef(
                    id=chk_id,
                    level=rchk.level,
                    cond=rchk.cond,
                    message=rchk.message,
                    loc=rchk.loc,
                )
            )

        rec_def = RecordDef(
            kind=unres.kind,
            name=qname,
            doc=inspect.getdoc(unres.cls_obj) or "",
            bases=base_qnames,
            fields=fields,
            enum_values=enum_values,
            nested=nested_defs,
            variants=variant_decls,
            methods=methods,
            checks=checks,
            equality_fields=eq_fields,
            hash_fields=hash_fields,
            backend=unres.directives.backend,
            loc=loc,
        )
        setattr(unres.cls_obj, DslAttr.RECORD.value, rec_def)
        return rec_def

    def _parse_method(
        self,
        name: str,
        raw_attr: Any,
        class_qname: QName,
        class_ns: Dict[str, Any],
    ) -> Optional[MethodDef]:
        is_static = False
        backend: Dict[str, Any] = {}
        loc: Optional[SourceLoc] = None
        func: Any = raw_attr

        if isinstance(func, staticmethod):
            is_static = True
            func = func.__func__

        if isinstance(func, MethodWrapper):
            backend = func.backend
            loc = func.loc
            func = func.func
            if isinstance(func, staticmethod):
                is_static = True
                func = func.__func__

        if not inspect.isfunction(func):
            return None

        if loc is None:
            _, line = inspect.getsourcelines(func)
            file = inspect.getsourcefile(func) or "<unknown>"
            loc = SourceLoc(file=file, line=line, function=name)

        type_params = [tp.__name__ for tp in getattr(func, "__type_params__", ())]
        sig = inspect.signature(func)
        annotations = getattr(func, "__annotations__", {})

        is_constructor = name == "__init__"
        if not is_constructor and "return" not in annotations:
            raise TypeError(
                f"Method '{name}' in '{class_qname.to_id()}' at {loc.file}:{loc.line} is missing a return type annotation"
            )

        ret_type = (
            DslType(kind=TypeKind.NAMED, qname=class_qname)
            if is_constructor
            else self.resolve_type(annotations["return"], class_qname, type_params)
        )

        method_args: List[MethodArgDef] = []
        params = list(sig.parameters.values())
        for idx, param in enumerate(params):
            if idx == 0 and not is_static and param.name == "self":
                continue
            if param.name not in annotations:
                raise TypeError(
                    f"Argument '{param.name}' of method '{name}' in '{class_qname.to_id()}' at {loc.file}:{loc.line} is missing a type annotation"
                )
            raw_ann = annotations[param.name]
            arg_type = self.resolve_type(raw_ann, class_qname, type_params)
            default_val = (
                None if param.default is inspect.Parameter.empty else param.default
            )
            method_args.append(
                MethodArgDef(
                    name=param.name,
                    type=arg_type,
                    default=default_val,
                    is_vararg=(param.kind == inspect.Parameter.VAR_POSITIONAL),
                    is_kwarg=(param.kind == inspect.Parameter.VAR_KEYWORD),
                )
            )

        return MethodDef(
            name=name,
            args=method_args,
            return_type=ret_type,
            type_params=type_params,
            is_static=is_static,
            is_constructor=is_constructor,
            backend=backend,
            doc=inspect.getdoc(func) or "",
            loc=loc,
        )

    def finalize(self) -> None:
        if self._finalized:
            return
        self.records.clear()
        self._name_collisions.clear()
        for unres in self._unresolved_top:
            self.records.append(self._finalize_record(unres))
        self._finalized = True

    # --- Frontend Entry Points ---

    @property
    @dsl_entry
    def This(self, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("this", _loc=_loc)

    @dsl_entry
    def Iter(self, name: str, *, _loc: Optional[SourceLoc] = None) -> Expr:
        return build_expr("iter", str_val=name, _loc=_loc)

    @dsl_entry
    def Field(
        self,
        doc: str = "",
        default: Any = _NO_DEFAULT,
        check: Optional[Expr] = None,
        *,
        _loc: Optional[SourceLoc] = None,
    ) -> Any:
        return FieldSpec(
            doc=doc,
            default=default,
            check=check.ir if check is not None else None,
            loc=_loc or capture_caller_loc(),
        )

    def Variant(self, enum_name: Optional[str] = None) -> VariantMarker:
        return VariantMarker(enum_name=enum_name)

    @dsl_entry
    def Method(
        self,
        backend: Dict[str, Any] = {},
        *,
        _loc: Optional[SourceLoc] = None,
    ) -> Callable[[Callable[..., Any]], MethodWrapper]:
        loc = _loc or capture_caller_loc()

        def decorator(func: Callable[..., Any]) -> MethodWrapper:
            return MethodWrapper(func=func, backend=backend, loc=loc)

        return decorator

    def to_dict(self) -> Dict[str, Any]:
        self.finalize()
        return {
            "package": self.package,
            "records": [r.model_dump(mode="json") for r in self.records],
        }

    # --- Soufflé Type Checking & Diagnostic Formatting ---

    def validate_with_souffle(self) -> None:
        self.finalize()
        dl_source = Path(__file__).with_suffix(".dl").read_text()

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)
            dl_file = tmp_path / "check.dl"
            dl_file.write_text(dl_source)

            facts: Dict[str, List[Tuple[Any, ...]]] = {
                "record": [],
                "record_type": [],
                "inheritance": [],
                "field": [],
                "field_default_key": [],
                "enum_entry": [],
                "variant_decl": [],
                "variant_case_map": [],
                "name_collision": list(self._name_collisions),
                "op_def": [op.to_souffle_fact() for op in self.operators],
                "expr": [],
                "expr_arg": [],
                "type_case_branch": [],
                "check_def": [],
                "field_check_def": [],
            }

            expr_map: Dict[int, ExprIR] = {}
            parent_map: Dict[int, Tuple[int, int]] = {}
            variant_tags_by_owner: Dict[str, List[str]] = {}
            emitted_exprs: set[int] = set()

            def emit_expr(e: ExprIR) -> None:
                expr_map[e.id] = e
                if e.id in emitted_exprs:
                    return
                emitted_exprs.add(e.id)
                tval_adt = (
                    e.type_val.to_souffle_adt()
                    if e.type_val is not None
                    else "$Primitive($PrimNone())"
                )
                facts["expr"].append(
                    (
                        e.id,
                        e.op,
                        e.str_val or "",
                        e.int_val or 0,
                        tval_adt,
                        e.loc.file,
                        e.loc.line,
                    )
                )
                for idx, arg in enumerate(e.args):
                    parent_map[arg.id] = (e.id, idx)
                    facts["expr_arg"].append((e.id, idx, arg.id))
                    emit_expr(arg)
                for idx, br in enumerate(e.type_branches):
                    assert br.resolved_type is not None
                    parent_map[br.body.id] = (e.id, idx)
                    facts["type_case_branch"].append(
                        (
                            e.id,
                            idx,
                            br.resolved_type.to_souffle_adt(),
                            br.var_name,
                            br.body.id,
                        )
                    )
                    emit_expr(br.body)

            def walk_record(r: RecordDef) -> None:
                rid = r.name.to_id()
                rtype = DslType(kind=TypeKind.NAMED, qname=r.name)
                facts["record"].append(
                    (
                        rid,
                        r.kind.value,
                        r.name.to_souffle_adt(),
                        r.loc.file,
                        r.loc.line,
                    )
                )
                facts["record_type"].append((rid, rtype.to_souffle_adt()))

                for b in r.bases:
                    facts["inheritance"].append((rid, b.to_id()))

                for ev in r.enum_values:
                    facts["enum_entry"].append((rid, ev.name))

                for f in r.fields:
                    ft_adt = f.type.to_souffle_adt()
                    has_def_flag = 1 if f.has_default else 0
                    is_null_flag = 1 if (f.has_default and f.default is None) else 0
                    facts["field"].append(
                        (
                            rid,
                            f.name,
                            ft_adt,
                            has_def_flag,
                            is_null_flag,
                            f.loc.file,
                            f.loc.line,
                        )
                    )
                    if f.has_default and isinstance(f.default, dict):
                        for k in f.default.keys():
                            facts["field_default_key"].append(
                                (rid, f.name, str(k), f.loc.file, f.loc.line)
                            )

                    if f.is_inline_variant:
                        tags: List[str] = []
                        for ic in f.inline_cases:
                            assert ic.qname is not None
                            tag_name = ic.qname.name
                            tags.append(tag_name)
                            facts["variant_case_map"].append(
                                (
                                    ft_adt,
                                    tag_name,
                                    ic.qname.to_id(),
                                    ic.to_souffle_adt(),
                                )
                            )
                        variant_tags_by_owner[f"{rid}.{f.name}"] = tags

                    if f.check is not None:
                        emit_expr(f.check)
                        fchk_id = f"{rid}.{f.name}::field_check"
                        facts["field_check_def"].append(
                            (
                                fchk_id,
                                rid,
                                f.name,
                                f.check.id,
                                f.loc.file,
                                f.loc.line,
                            )
                        )

                for chk in r.checks:
                    emit_expr(chk.cond)
                    emit_expr(chk.message)
                    facts["check_def"].append(
                        (
                            chk.id,
                            rid,
                            chk.level.value,
                            chk.cond.id,
                            chk.message.id,
                            chk.loc.file,
                            chk.loc.line,
                        )
                    )

                for nr in r.nested:
                    walk_record(nr)

            for top_rec in self.records:
                walk_record(top_rec)

            for rel_name, rows in facts.items():
                fact_path = tmp_path / f"{rel_name}.facts"
                with fact_path.open("w", newline="") as fobj:
                    writer = csv.writer(fobj, delimiter="\t", lineterminator="\n")
                    for row in rows:
                        writer.writerow(row)

            souffle_cmd = local["souffle"]
            souffle_cmd["-F", str(tmp_path), "-D", str(tmp_path), str(dl_file)]()

            diag_file = tmp_path / "diagnostic.csv"
            if diag_file.exists():
                formatted_errors: List[str] = []
                with diag_file.open("r", newline="") as fobj:
                    reader = csv.reader(fobj, delimiter="\t")
                    for row in reader:
                        if len(row) != 7:
                            raise RuntimeError(
                                f"Malformed diagnostic row from Soufflé: {row!r}"
                            )
                        (
                            code,
                            check_id,
                            expr_id_str,
                            arg_idx_str,
                            file,
                            line_str,
                            detail,
                        ) = row
                        expr_id = int(expr_id_str)
                        arg_idx = int(arg_idx_str)

                        msg_lines = [
                            f"[{code}] {file}:{line_str} (in {check_id}): {detail}"
                        ]

                        if expr_id in expr_map:
                            failed_expr = expr_map[expr_id]
                            if failed_expr.loc.code_line:
                                msg_lines.append(
                                    f"    Source: {failed_expr.loc.code_line}"
                                )
                            msg_lines.append(
                                f"    Expr:   {failed_expr.render_summary()}"
                            )

                            if code == "INVALID_VARIANT_CASE":
                                all_tags = sorted(
                                    {
                                        t
                                        for tags in variant_tags_by_owner.values()
                                        for t in tags
                                    }
                                )
                                if all_tags:
                                    msg_lines.append(
                                        f"    Expected cases: {', '.join(all_tags)}"
                                    )

                            # Build enclosing expression context chain
                            chain: List[str] = []
                            cur_id = expr_id
                            while cur_id in parent_map:
                                pid, pidx = parent_map[cur_id]
                                if pid in expr_map:
                                    pexpr = expr_map[pid]
                                    chain.append(
                                        f"arg #{pidx} of '{pexpr.op}' at {Path(pexpr.loc.file).name}:{pexpr.loc.line}"
                                    )
                                cur_id = pid
                            if chain:
                                msg_lines.append(f"    Context: {' -> '.join(chain)}")

                        formatted_errors.append("\n".join(msg_lines))

                if formatted_errors:
                    joined = "\n\n  ".join(formatted_errors)
                    raise DslValidationError(
                        f"Soufflé Datalog validation failed with {len(formatted_errors)} primary error(s):\n\n  {joined}"
                    )


@beartype
def get_dsl_model(
    package: str = "",
    operators: Optional[Sequence[OperatorSpec]] = None,
) -> DslModel:
    return DslModel(package=package, operators=operators)

#!/usr/bin/env python
from __future__ import annotations

import ast
import csv
import enum
import inspect
import sys
import tempfile
import textwrap
import types
from contextvars import ContextVar
from dataclasses import dataclass, field
from functools import wraps
from pathlib import Path

from beartype import beartype
from beartype.typing import (
    Annotated,
    Any,
    Callable,
    ForwardRef,
    Sequence,
    Union,
    get_args,
    get_origin,
)
from model_dsl_ir import (
    CheckDef,
    DefaultValue,
    Diagnostic,
    DiagnosticNote,
    EnumValueDef,
    ExprOccurrence,
    FieldDef,
    MethodArgDef,
    MethodDef,
    ModelIR,
    QName,
    RecordDef,
    RecordKind,
    Severity,
    SourceLoc,
    TypeKind,
    TypeRef,
    VariantDef,
    parse_souffle_type,
    souffle_type,
)
from model_dsl_operations import (
    BindingMode,
    Operation,
    standard_operations,
)
from plumbum import local

__all__ = [
    "DslModel",
    "DslValidationError",
    "EnFieldAnnotation",
    "Expr",
    "FixedArray",
    "Float32",
    "Float64",
    "Int8",
    "Int16",
    "Int32",
    "Int64",
    "UInt8",
    "UInt16",
    "UInt32",
    "UInt64",
    "__check__",
    "__config__",
    "__variant__",
    "get_dsl_model",
    "op_all",
    "op_all_rec",
    "op_and",
    "op_apply",
    "op_exists",
    "op_exists_one",
    "op_filter",
    "op_filter_map",
    "op_group_by",
    "op_map",
    "op_not",
    "op_or",
    "op_slice",
    "op_takewhile",
    "op_tuple",
    "op_walk",
    "type_case",
    "type_cond",
    "typeof",
]


class DslAttribute(enum.StrEnum):
    MODEL = "__dsl_model__"
    KIND = "__dsl_kind__"
    NAME = "__dsl_name__"
    ROOT = "__dsl_root__"


class Op(enum.StrEnum):
    THIS = "this"
    ITER = "iter"
    INT = "int"
    FLOAT = "float"
    STRING = "string"
    BOOL = "bool"
    NONE = "none"
    TYPE = "type"
    TYPEOF = "typeof"
    FIELD = "field"
    EQ = "eq"
    NE = "ne"
    LT = "lt"
    LE = "le"
    AND = "and"
    OR = "or"
    NOT = "not"
    STR_SIZE = "str_size"
    STR_REGEX = "str_regex"
    STR_FORMAT = "str_format"
    LIST_SIZE = "list_size"
    MAP_SIZE = "map_size"
    MAP_CONTAINS = "map_contains"
    MAP_GET = "map_get"
    MAP_KEYS = "map_keys"
    MAP_VALUES = "map_values"
    OPT_HAS = "opt_has"
    OPT_VALUE = "opt_value"
    CASE_IS = "case_is"
    CASE_KIND = "case_kind"
    ENUM_IS = "enum_is"
    NTH = "nth"
    DERIVED = "derived"
    TUPLE = "tuple"
    WALK = "walk"
    PATH = "path"
    VALUE = "value"
    SLICE = "slice"
    ALL = "all"
    EXISTS = "exists"
    EXISTS_ONE = "exists_one"
    MAP = "map"
    FILTER = "filter"
    FILTER_MAP = "filter_map"
    TAKEWHILE = "takewhile"
    GROUP_BY = "group_by"
    TYPE_MATCH = "type_match"
    TYPE_CASE = "type_case"


ORIGIN: ContextVar[SourceLoc | None] = ContextVar("dsl_origin", default=None)


@beartype
def caller_location() -> SourceLoc:
    frame = sys._getframe(1)
    implementation = Path(__file__).resolve()
    while frame is not None:
        filename = frame.f_code.co_filename
        generated = filename.startswith("<")
        internal = not generated and Path(filename).resolve() == implementation
        if not generated and not internal:
            return SourceLoc(
                file=filename,
                line=frame.f_lineno,
                function=frame.f_code.co_name,
            )
        frame = frame.f_back
    raise RuntimeError(
        "Cannot locate the DSL declaration because no user-code frame "
        "exists outside the DSL implementation"
    )


@beartype
def source_entry(func: Callable[..., Any]) -> Callable[..., Any]:
    @wraps(func)
    def wrapped(*args: Any, **kwargs: Any) -> Any:
        if ORIGIN.get() is not None:
            return func(*args, **kwargs)
        token = ORIGIN.set(caller_location())
        try:
            return func(*args, **kwargs)
        finally:
            ORIGIN.reset(token)

    return wrapped


@beartype
def origin() -> SourceLoc:
    value = ORIGIN.get()
    if value is None:
        raise RuntimeError(
            "Expression construction requires a source-entry boundary; "
            "the expression builder was called directly"
        )
    return value


@beartype
@dataclass(frozen=True)
class BuildExpr:
    op: str
    args: tuple[BuildExpr, ...]
    loc: SourceLoc
    text: str = ""
    value: Any = None
    annotation: Any = None
    binder: str | None = None


@beartype
def build(
    op: str,
    args: Sequence[Any] = (),
    *,
    text: str = "",
    value: Any = None,
    annotation: Any = None,
    binder: str | None = None,
) -> Expr:
    return Expr(
        BuildExpr(
            op=op,
            args=tuple(wrap(value).node for value in args),
            loc=origin(),
            text=text,
            value=value,
            annotation=annotation,
            binder=binder,
        )
    )


@beartype
def wrap(value: Any) -> Expr:
    match value:
        case Expr():
            return value
        case bool():
            return build(Op.BOOL, value=value)
        case int():
            return build(Op.INT, value=value)
        case float():
            return build(Op.FLOAT, value=value)
        case str():
            return build(Op.STRING, value=value)
        case None:
            return build(Op.NONE)
        case _:
            raise TypeError(
                f"{origin().file}:{origin().line}: cannot use "
                f"{value!r} of type {type(value).__name__} as a DSL expression"
            )


@beartype
class ExprAccessors:
    @property
    def _Str(self) -> StrAccessor:
        return StrAccessor(self)

    @property
    def _List(self) -> ListAccessor:
        return ListAccessor(self)

    @property
    def _Map(self) -> MapAccessor:
        return MapAccessor(self)

    @property
    def _Opt(self) -> OptAccessor:
        return OptAccessor(self)

    @property
    def _Case(self) -> CaseAccessor:
        return CaseAccessor(self)

    @property
    def _Enum(self) -> EnumAccessor:
        return EnumAccessor(self)

    @property
    def _Nth(self) -> NthAccessor:
        return NthAccessor(self)


@beartype
class Expr(ExprAccessors):
    def __init__(self, node: BuildExpr) -> None:
        self.node = node

    def __bool__(self) -> bool:
        raise TypeError(
            f"{self.node.loc.file}:{self.node.loc.line}: a DSL expression "
            "cannot be converted to Python bool; use op_and, op_or, op_not, "
            "&, |, or ~ instead of Python and, or, not, or chained comparisons"
        )

    @source_entry
    def __eq__(self, other: object) -> Expr:
        return build(Op.EQ, (self, other))

    @source_entry
    def __ne__(self, other: object) -> Expr:
        return build(Op.NE, (self, other))

    @source_entry
    def __lt__(self, other: object) -> Expr:
        return build(Op.LT, (self, other))

    @source_entry
    def __le__(self, other: object) -> Expr:
        return build(Op.LE, (self, other))

    @source_entry
    def __gt__(self, other: object) -> Expr:
        return build(Op.LT, (other, self))

    @source_entry
    def __ge__(self, other: object) -> Expr:
        return build(Op.LE, (other, self))

    @source_entry
    def __and__(self, other: object) -> Expr:
        return build(Op.AND, (self, other))

    @source_entry
    def __or__(self, other: object) -> Expr:
        return build(Op.OR, (self, other))

    @source_entry
    def __invert__(self) -> Expr:
        return build(Op.NOT, (self,))

    @source_entry
    def __getitem__(self, name: str) -> Expr:
        return build(Op.FIELD, (self,), text=name)

    @source_entry
    def is_derived_from(self, annotation: Any) -> Expr:
        return build(Op.DERIVED, (self,), annotation=annotation)

    @source_entry
    def path(self) -> Expr:
        return build(Op.PATH, (self,))

    @source_entry
    def value(self) -> Expr:
        return build(Op.VALUE, (self,))


@beartype
class StrAccessor:
    def __init__(self, target: Expr | None) -> None:
        self.target = target

    def instance(self) -> Expr:
        if self.target is None:
            raise ValueError("String instance operation requires an expression target")
        return self.target

    @source_entry
    def size(self) -> Expr:
        return build(Op.STR_SIZE, (self.instance(),))

    @source_entry
    def matches(self, pattern: str) -> Expr:
        return build(Op.STR_REGEX, (self.instance(),), text=pattern)

    @source_entry
    def format(self, args: Sequence[Any]) -> Expr:
        return build(Op.STR_FORMAT, args)


@beartype
class ListAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @source_entry
    def size(self) -> Expr:
        return build(Op.LIST_SIZE, (self.target,))


@beartype
class MapAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @source_entry
    def size(self) -> Expr:
        return build(Op.MAP_SIZE, (self.target,))

    @source_entry
    def contains(self, key: Any) -> Expr:
        return build(Op.MAP_CONTAINS, (self.target, key))

    @source_entry
    def get(self, key: Any) -> Expr:
        return build(Op.MAP_GET, (self.target, key))

    @source_entry
    def keys(self) -> Expr:
        return build(Op.MAP_KEYS, (self.target,))

    @source_entry
    def values(self) -> Expr:
        return build(Op.MAP_VALUES, (self.target,))


@beartype
class OptAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @source_entry
    def has_value(self) -> Expr:
        return build(Op.OPT_HAS, (self.target,))

    @source_entry
    def value(self) -> Expr:
        return build(Op.OPT_VALUE, (self.target,))


@beartype
class CaseAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @source_entry
    def is_case(self, name: str) -> Expr:
        return build(Op.CASE_IS, (self.target,), text=name)

    @source_entry
    def which_case(self) -> Expr:
        return build(Op.CASE_KIND, (self.target,))


@beartype
class EnumAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @source_entry
    def is_val(self, name: str) -> Expr:
        return build(Op.ENUM_IS, (self.target,), text=name)


@beartype
class NthAccessor:
    def __init__(self, target: Expr) -> None:
        self.target = target

    @source_entry
    def get(self, index: int) -> Expr:
        return build(Op.NTH, (self.target,), value=index)


@beartype
@source_entry
def op_apply(
    name: str,
    *args: Any,
    binder: str | None = None,
) -> Expr:
    return build(name, args, binder=binder)


@beartype
@source_entry
def op_and(*args: Any) -> Expr:
    return build(Op.AND, args)


@beartype
@source_entry
def op_or(*args: Any) -> Expr:
    return build(Op.OR, args)


@beartype
@source_entry
def op_not(value: Any) -> Expr:
    return build(Op.NOT, (value,))


@beartype
@source_entry
def op_all(collection: Expr, name: str, predicate: Any) -> Expr:
    return build(Op.ALL, (collection, predicate), binder=name)


@beartype
@source_entry
def op_exists(collection: Expr, name: str, predicate: Any) -> Expr:
    return build(Op.EXISTS, (collection, predicate), binder=name)


@beartype
@source_entry
def op_exists_one(collection: Expr, name: str, predicate: Any) -> Expr:
    return build(Op.EXISTS_ONE, (collection, predicate), binder=name)


@beartype
@source_entry
def op_map(collection: Expr, name: str, expression: Any) -> Expr:
    return build(Op.MAP, (collection, expression), binder=name)


@beartype
@source_entry
def op_filter(collection: Expr, name: str, predicate: Any) -> Expr:
    return build(Op.FILTER, (collection, predicate), binder=name)


@beartype
@source_entry
def op_filter_map(collection: Expr, name: str, expression: Any) -> Expr:
    return build(Op.FILTER_MAP, (collection, expression), binder=name)


@beartype
@source_entry
def op_group_by(collection: Expr, name: str, key: Any) -> Expr:
    return build(Op.GROUP_BY, (collection, key), binder=name)


@beartype
@source_entry
def op_takewhile(collection: Expr, name: str, predicate: Any) -> Expr:
    return build(Op.TAKEWHILE, (collection, predicate), binder=name)


@beartype
@source_entry
def op_slice(collection: Expr, start: Any, stop: Any) -> Expr:
    return build(Op.SLICE, (collection, start, stop))


@beartype
@source_entry
def op_tuple(*values: Any) -> Expr:
    return build(Op.TUPLE, values)


@beartype
@source_entry
def op_walk(value: Expr) -> Expr:
    return build(Op.WALK, (value,))


@beartype
@source_entry
def op_all_rec(value: Expr, name: str, predicate: Any) -> Expr:
    return op_all(op_walk(value), name, predicate)


@beartype
@source_entry
def typeof(value: Expr) -> Expr:
    return build(Op.TYPEOF, (value,))


@beartype
@source_entry
def type_case(annotation: Any, name: str, expression: Any) -> Expr:
    return build(
        Op.TYPE_CASE,
        (expression,),
        annotation=annotation,
        binder=name,
    )


@beartype
@source_entry
def type_cond(
    value: Expr,
    *cases: Expr,
    unhandled_ignore: bool = False,
) -> Expr:
    for case in cases:
        if case.node.op != Op.TYPE_CASE:
            raise TypeError(
                f"{origin().file}:{origin().line}: type_cond expects "
                f"type_case expressions, got operation {case.node.op!r}"
            )
    return build(
        Op.TYPE_MATCH,
        (value, *cases),
        text="ignore" if unhandled_ignore else "total",
    )


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
@dataclass(frozen=True)
class FixedArraySpec:
    element: Any
    size: int


@beartype
class FixedArrayFactory:
    def __getitem__(self, params: tuple[Any, int]) -> FixedArraySpec:
        element, size = params
        if size < 0:
            raise ValueError(f"FixedArray size must be nonnegative, got {size}")
        return FixedArraySpec(element=element, size=size)


FixedArray = FixedArrayFactory()


@beartype
@dataclass(frozen=True)
class EnFieldAnnotation:
    doc: str = ""


class ImplicitDefault(enum.Enum):
    VALUE = "implicit"


@beartype
@dataclass(frozen=True)
class FieldSpec:
    default: Any
    doc: str
    check: BuildExpr | None
    loc: SourceLoc


@beartype
@dataclass(frozen=True)
class CheckBuild:
    id: str
    condition: BuildExpr
    message: BuildExpr
    severity: Severity
    loc: SourceLoc
    field_name: str | None = None


@beartype
@dataclass(frozen=True)
class VariantBuild:
    cases: tuple[Any, ...]
    field_name: str
    enum_name: str
    loc: SourceLoc


@beartype
@dataclass
class Directives:
    checks: list[CheckBuild] = field(default_factory=list)
    variants: list[VariantBuild] = field(default_factory=list)
    backend: dict[str, Any] = field(default_factory=dict)


@beartype
class ClassNamespace(dict[str, Any]):
    def __init__(self) -> None:
        super().__init__()
        self.directives = Directives()
        self["__check__"] = self.check
        self["__variant__"] = self.variant
        self["__config__"] = self.config

    @source_entry
    def check(
        self,
        cond: Any,
        message: str | Expr,
        id: str = "",
        level: Severity | str = Severity.ERROR,
    ) -> None:
        self.directives.checks.append(
            CheckBuild(
                id=id,
                condition=wrap(cond).node,
                message=wrap(message).node,
                severity=Severity(level),
                loc=origin(),
            )
        )

    @source_entry
    def variant(
        self,
        *cases: Any,
        field_name: str = "data",
        enum_name: str = "Kind",
    ) -> None:
        if not cases:
            raise ValueError(
                f"{origin().file}:{origin().line}: __variant__ requires "
                "at least one case"
            )
        self.directives.variants.append(
            VariantBuild(
                cases=cases,
                field_name=field_name,
                enum_name=enum_name,
                loc=origin(),
            )
        )

    @source_entry
    def config(self, backend: dict[str, Any]) -> None:
        self.directives.backend.update(backend)


@beartype
def __check__(*args: Any, **kwargs: Any) -> None:
    raise RuntimeError("__check__ must be called directly in a DSL class body")


@beartype
def __variant__(*args: Any, **kwargs: Any) -> None:
    raise RuntimeError("__variant__ must be called directly in a DSL class body")


@beartype
def __config__(*args: Any, **kwargs: Any) -> None:
    raise RuntimeError("__config__ must be called directly in a DSL class body")


@beartype
@dataclass(frozen=True)
class MethodWrapper:
    function: Callable[..., Any]
    backend: dict[str, Any]
    loc: SourceLoc


@beartype
@dataclass
class DeclarationBuild:
    cls: type
    name: QName
    kind: RecordKind
    namespace: dict[str, Any]
    directives: Directives
    loc: SourceLoc


class DslMeta(type):
    @classmethod
    def __prepare__(
        metaclass,
        name: str,
        bases: tuple[type, ...],
        **kwargs: Any,
    ) -> ClassNamespace:
        return ClassNamespace()

    def __new__(
        metaclass,
        name: str,
        bases: tuple[type, ...],
        namespace: ClassNamespace,
        **kwargs: Any,
    ) -> DslMeta:
        values = dict(namespace)
        for directive in ("__check__", "__variant__", "__config__"):
            values.pop(directive, None)
        cls = super().__new__(metaclass, name, bases, values)
        if values.get(DslAttribute.ROOT.value, False):
            return cls
        model = getattr(cls, DslAttribute.MODEL.value)
        kind = getattr(cls, DslAttribute.KIND.value)
        qualname = values.get("__qualname__", name)
        parts = tuple(part for part in qualname.split(".") if part != "<locals>")
        qname = QName(package=model.package, parts=parts)
        setattr(cls, DslAttribute.NAME.value, qname)
        model.declarations.append(
            DeclarationBuild(
                cls=cls,
                name=qname,
                kind=kind,
                namespace=values,
                directives=namespace.directives,
                loc=caller_location(),
            )
        )
        return cls


@beartype
class TypeLookup:
    def __init__(self, package: str) -> None:
        self.package = package

    def __getitem__(self, path: str) -> QName:
        return QName(package=self.package, parts=tuple(path.split(".")))


@beartype
class DslValidationError(Exception):
    def __init__(self, diagnostics: Sequence[Diagnostic]) -> None:
        self.diagnostics = list(diagnostics)
        text = "\n".join(value.display() for value in diagnostics)
        super().__init__(
            f"DSL validation failed with {len(diagnostics)} diagnostic(s):\n{text}"
        )


@beartype
class DslModel:
    def __init__(
        self,
        package: str = "",
        operations: Sequence[Operation] | None = None,
    ) -> None:
        self.package = package
        self.declarations: list[DeclarationBuild] = []
        self.operations = list(
            standard_operations() if operations is None else operations
        )
        self._T = TypeLookup(package)
        self._Str = StrAccessor(None)

        class ClassBase(metaclass=DslMeta):
            __dsl_root__ = True
            __dsl_model__ = self
            __dsl_kind__ = RecordKind.CLASS

        class CaseBase(metaclass=DslMeta):
            __dsl_root__ = True
            __dsl_model__ = self
            __dsl_kind__ = RecordKind.CASE

        class EnumBase(metaclass=DslMeta):
            __dsl_root__ = True
            __dsl_model__ = self
            __dsl_kind__ = RecordKind.ENUM

        self.Class = ClassBase
        self.Case = CaseBase
        self.Enum = EnumBase

    @property
    @source_entry
    def This(self) -> Expr:
        return build(Op.THIS)

    @source_entry
    def Iter(self, name: str) -> Expr:
        return build(Op.ITER, text=name)

    @source_entry
    def Type(self, annotation: Any) -> Expr:
        return build(Op.TYPE, annotation=annotation)

    @source_entry
    def Field(
        self,
        default: Any = ImplicitDefault.VALUE,
        *,
        doc: str = "",
        check: Expr | None = None,
    ) -> Any:
        return FieldSpec(
            default=default,
            doc=doc,
            check=check.node if check is not None else None,
            loc=origin(),
        )

    @source_entry
    def Method(
        self,
        backend: dict[str, Any] | None = None,
    ) -> Callable[[Callable[..., Any]], MethodWrapper]:
        location = origin()
        configuration = {} if backend is None else backend

        def decorate(function: Callable[..., Any]) -> MethodWrapper:
            return MethodWrapper(
                function=function,
                backend=configuration,
                loc=location,
            )

        return decorate

    def compile(self) -> ModelIR:
        return Compiler(self).compile()

    def validate_with_souffle(self) -> ModelIR:
        return self.compile()


@beartype
class Compiler:
    def __init__(self, model: DslModel) -> None:
        self.model = model
        self.declarations = {value.name.to_id(): value for value in model.declarations}
        if len(self.declarations) != len(model.declarations):
            counts: dict[str, int] = {}
            for declaration in model.declarations:
                name = declaration.name.to_id()
                counts[name] = counts.get(name, 0) + 1
            duplicates = sorted(name for name, count in counts.items() if 1 < count)
            raise ValueError(
                f"Package {model.package!r} declares duplicate records: "
                f"{', '.join(duplicates)}"
            )
        self.operations: dict[str, Operation] = {}
        for operation in model.operations:
            if operation.name in self.operations:
                raise ValueError(
                    f"Package {model.package!r} registers operation "
                    f"{operation.name!r} more than once"
                )
            self.operations[operation.name] = operation
        self.records: dict[str, RecordDef] = {}
        self.effective: dict[str, dict[str, FieldDef]] = {}
        self.expressions: list[ExprOccurrence] = []
        self.checks: list[CheckDef] = []
        self.check_builds: list[tuple[DeclarationBuild, CheckBuild]] = []
        self.facts: dict[str, list[list[str]]] = {
            name: []
            for name in (
                "operation",
                "expression",
                "argument",
                "arity",
                "this_type",
                "reference",
                "collection_binding",
                "case_binding",
                "case_source",
                "expected",
                "known_type",
                "record",
                "inheritance",
                "field",
                "variant_case",
                "enum_entry",
                "tuple_element",
            )
        }
        self.types: set[TypeRef] = set()
        self.binding_counter = 0
        self.expression_checks: dict[int, CheckDef] = {}

    def fail(self, code: str, message: str, loc: SourceLoc) -> None:
        raise DslValidationError([Diagnostic(code=code, message=message, loc=loc)])

    def fact(self, relation: str, *values: str | int) -> None:
        row = [str(value) for value in values]
        for value in row:
            if any(character in value for character in ("\t", "\r", "\n")):
                raise ValueError(
                    f"Relation {relation!r} cannot encode a raw tab or newline "
                    f"in fact value {value!r}"
                )
        self.facts[relation].append(row)

    def register_type(self, value: TypeRef) -> None:
        if value in self.types:
            return
        self.types.add(value)
        self.fact("known_type", souffle_type(value))
        for argument in value.args:
            self.register_type(argument)
        if value.kind == TypeKind.TUPLE:
            for position, argument in enumerate(value.args):
                self.fact(
                    "tuple_element",
                    souffle_type(value),
                    position,
                    souffle_type(argument),
                )

    def environment(
        self,
        declaration: DeclarationBuild,
        parameters: Sequence[Any] = (),
    ) -> dict[str, Any]:
        module = sys.modules[declaration.cls.__module__]
        result = dict(vars(module))
        parts = declaration.name.parts
        for depth in range(len(parts)):
            scope = parts[:depth]
            for candidate in self.model.declarations:
                if candidate.name.parts[:-1] == scope:
                    result[candidate.name.parts[-1]] = candidate.cls
        for depth in range(1, len(parts) + 1):
            name = QName(
                package=self.model.package,
                parts=parts[:depth],
            ).to_id()
            if name in self.declarations:
                result.update(vars(self.declarations[name].cls))
        result.update(declaration.namespace)
        for parameter in parameters:
            result[parameter.__name__] = parameter
        return result

    def resolve(
        self,
        annotation: Any,
        declaration: DeclarationBuild,
        parameters: Sequence[Any] = (),
    ) -> TypeRef:
        match annotation:
            case TypeRef():
                self.register_type(annotation)
                return annotation
            case QName():
                name = annotation.to_id()
                if (
                    annotation.package == self.model.package
                    and name not in self.declarations
                    and name not in self.records
                ):
                    self.fail(
                        "unknown_type",
                        f"Type {name!r} is not declared in package "
                        f"{self.model.package!r}",
                        declaration.loc,
                    )
                result = TypeRef(kind=TypeKind.NAMED, name=name)
            case FixedArraySpec():
                result = TypeRef(
                    kind=TypeKind.ARRAY,
                    args=(self.resolve(annotation.element, declaration, parameters),),
                    size=annotation.size,
                )
            case ForwardRef():
                return self.resolve(annotation.__forward_arg__, declaration, parameters)
            case str():
                environment = self.environment(declaration, parameters)
                try:
                    evaluated = eval(annotation, environment, environment)
                except NameError as error:
                    self.fail(
                        "unknown_annotation",
                        f"Cannot resolve annotation {annotation!r} in "
                        f"{declaration.name.to_id()!r}: {error}",
                        declaration.loc,
                    )
                if isinstance(evaluated, str):
                    if evaluated == annotation:
                        self.fail(
                            "recursive_annotation",
                            f"Annotation {annotation!r} in "
                            f"{declaration.name.to_id()!r} resolves to itself "
                            "instead of a type",
                            declaration.loc,
                        )
                    return self.resolve(evaluated, declaration, parameters)
                return self.resolve(evaluated, declaration, parameters)
            case _:
                primitives = {
                    Int8: TypeKind.INT8,
                    Int16: TypeKind.INT16,
                    Int32: TypeKind.INT32,
                    Int64: TypeKind.INT64,
                    UInt8: TypeKind.UINT8,
                    UInt16: TypeKind.UINT16,
                    UInt32: TypeKind.UINT32,
                    UInt64: TypeKind.UINT64,
                    Float32: TypeKind.FLOAT32,
                    Float64: TypeKind.FLOAT64,
                    int: TypeKind.INT64,
                    float: TypeKind.FLOAT64,
                    str: TypeKind.STRING,
                    bool: TypeKind.BOOL,
                    types.NoneType: TypeKind.NONE,
                    Any: TypeKind.ANY,
                }
                if annotation is None:
                    result = TypeRef(kind=TypeKind.NONE)
                elif isinstance(annotation, type) and annotation in primitives:
                    result = TypeRef(kind=primitives[annotation])
                elif annotation is Any:
                    result = TypeRef(kind=TypeKind.ANY)
                elif isinstance(annotation, DslMeta):
                    qname = getattr(annotation, DslAttribute.NAME.value)
                    result = TypeRef(
                        kind=TypeKind.NAMED,
                        name=qname.to_id(),
                    )
                elif any(annotation is value for value in parameters):
                    result = TypeRef(
                        kind=TypeKind.PARAM,
                        name=annotation.__name__,
                    )
                else:
                    result = self.resolve_composite(annotation, declaration, parameters)
        self.register_type(result)
        return result

    def resolve_composite(
        self,
        annotation: Any,
        declaration: DeclarationBuild,
        parameters: Sequence[Any],
    ) -> TypeRef:
        constructor = get_origin(annotation)
        arguments = get_args(annotation)
        if constructor is Annotated:
            return self.resolve(arguments[0], declaration, parameters)
        kinds = {
            list: TypeKind.LIST,
            dict: TypeKind.MAP,
            tuple: TypeKind.TUPLE,
        }
        if constructor in kinds:
            if constructor is tuple and Ellipsis in arguments:
                self.fail(
                    "variadic_tuple",
                    f"Annotation {annotation!r} uses a variadic tuple; "
                    "use list[T] for a variable-length sequence",
                    declaration.loc,
                )
            return TypeRef(
                kind=kinds[constructor],
                args=tuple(
                    self.resolve(value, declaration, parameters) for value in arguments
                ),
            )
        if constructor in (Union, types.UnionType):
            values = tuple(
                self.resolve(value, declaration, parameters) for value in arguments
            )
            present = tuple(value for value in values if value.kind != TypeKind.NONE)
            if len(values) == 2 and len(present) == 1:
                return TypeRef(
                    kind=TypeKind.OPTIONAL,
                    args=present,
                )
            return TypeRef(kind=TypeKind.VARIANT, args=values)
        self.fail(
            "unsupported_annotation",
            f"Unsupported annotation {annotation!r} in {declaration.name.to_id()!r}",
            declaration.loc,
        )
        raise AssertionError

    def collect_records(self) -> None:
        for declaration in self.model.declarations:
            record = RecordDef(
                kind=declaration.kind,
                name=declaration.name,
                loc=declaration.loc,
                backend=declaration.directives.backend,
            )
            self.records[record.name.to_id()] = record

        for declaration in self.model.declarations:
            record = self.records[declaration.name.to_id()]
            for base in declaration.cls.__bases__:
                if DslAttribute.ROOT.value in vars(base):
                    continue
                if isinstance(base, DslMeta):
                    record.bases.append(self.resolve(base, declaration))

            annotations = declaration.namespace.get("__annotations__", {})
            if declaration.kind == RecordKind.ENUM:
                for name, value in declaration.namespace.items():
                    if name.startswith("__") or type(value) is not int:
                        continue
                    doc = ""
                    annotation = annotations.get(name)
                    if annotation is not None:
                        if isinstance(annotation, str):
                            environment = self.environment(declaration)
                            annotation = eval(annotation, environment, environment)
                        if get_origin(annotation) is Annotated:
                            for metadata in get_args(annotation)[1:]:
                                if isinstance(metadata, EnFieldAnnotation):
                                    doc = metadata.doc
                    record.enum_values.append(
                        EnumValueDef(
                            name=name,
                            value=value,
                            doc=doc,
                            loc=declaration.loc,
                        )
                    )
            else:
                for name, annotation in annotations.items():
                    raw = declaration.namespace.get(name, ImplicitDefault.VALUE)
                    spec = raw if isinstance(raw, FieldSpec) else None
                    value = spec.default if spec is not None else raw
                    record.fields.append(
                        FieldDef(
                            name=name,
                            type=self.resolve(annotation, declaration),
                            default=(
                                None
                                if value is ImplicitDefault.VALUE
                                else DefaultValue(value=value)
                            ),
                            doc=spec.doc if spec is not None else "",
                            loc=spec.loc if spec is not None else declaration.loc,
                        )
                    )
                    if spec is not None and spec.check is not None:
                        self.check_builds.append(
                            (
                                declaration,
                                CheckBuild(
                                    id=f"field:{name}",
                                    condition=spec.check,
                                    message=BuildExpr(
                                        op=Op.STRING,
                                        args=(),
                                        loc=spec.loc,
                                        value=f"Field {name} failed validation",
                                    ),
                                    severity=Severity.ERROR,
                                    loc=spec.loc,
                                    field_name=name,
                                ),
                            )
                        )

            for variant in declaration.directives.variants:
                self.collect_variant(declaration, record, variant)

            for name, value in declaration.namespace.items():
                if name.startswith("__") and name not in (
                    "__init__",
                    "__eq__",
                    "__hash__",
                ):
                    continue
                method = self.collect_method(name, value, declaration)
                if method is not None:
                    record.methods.append(method)

            for check in declaration.directives.checks:
                self.check_builds.append((declaration, check))

    def collect_variant(
        self,
        declaration: DeclarationBuild,
        record: RecordDef,
        variant: VariantBuild,
    ) -> None:
        if any(value.name == variant.field_name for value in record.fields):
            self.fail(
                "variant_field_conflict",
                f"Generated variant field {variant.field_name!r} conflicts "
                f"with a field of {record.name.to_id()!r}",
                variant.loc,
            )
        enum_name = QName(
            package=self.model.package,
            parts=(*record.name.parts, variant.enum_name),
        )
        enum_id = enum_name.to_id()
        if enum_id in self.records:
            self.fail(
                "variant_enum_conflict",
                f"Generated enum {enum_id!r} conflicts with an existing "
                "declaration; configure a different enum_name",
                variant.loc,
            )
        cases = [self.resolve(value, declaration) for value in variant.cases]
        tags = []
        for case in cases:
            candidate = self.records.get(case.name)
            if candidate is None or candidate.kind != RecordKind.CASE:
                self.fail(
                    "invalid_variant_case",
                    f"Variant of {record.name.to_id()!r} references "
                    f"{case.display()!r}, which is not a local M.Case declaration",
                    variant.loc,
                )
            tags.append(candidate.name.parts[-1])
        duplicates = sorted(tag for tag in set(tags) if 1 < tags.count(tag))
        if duplicates:
            self.fail(
                "duplicate_variant_tags",
                f"Variant of {record.name.to_id()!r} has duplicate tags "
                f"{duplicates!r}; case names must be distinct",
                variant.loc,
            )
        enum_type = TypeRef(kind=TypeKind.NAMED, name=enum_id)
        self.records[enum_id] = RecordDef(
            kind=RecordKind.ENUM,
            name=enum_name,
            enum_values=[
                EnumValueDef(
                    name=tag,
                    value=position,
                    loc=variant.loc,
                )
                for position, tag in enumerate(tags)
            ],
            loc=variant.loc,
        )
        union = TypeRef(kind=TypeKind.VARIANT, args=tuple(cases))
        self.register_type(union)
        self.register_type(enum_type)
        record.fields.append(
            FieldDef(
                name=variant.field_name,
                type=union,
                loc=variant.loc,
            )
        )
        record.variants.append(
            VariantDef(
                field_name=variant.field_name,
                enum_type=enum_type,
                cases=cases,
                tags=tags,
            )
        )

    def collect_method(
        self,
        name: str,
        value: Any,
        declaration: DeclarationBuild,
    ) -> MethodDef | None:
        static = False
        backend: dict[str, Any] = {}
        location: SourceLoc | None = None
        if isinstance(value, staticmethod):
            static = True
            value = value.__func__
        if isinstance(value, MethodWrapper):
            backend = value.backend
            location = value.loc
            value = value.function
            if isinstance(value, staticmethod):
                static = True
                value = value.__func__
        if not inspect.isfunction(value):
            return None
        if location is None:
            location = SourceLoc(
                file=inspect.getsourcefile(value) or declaration.loc.file,
                line=value.__code__.co_firstlineno,
                function=name,
            )
        signature = inspect.signature(value)
        parameters = getattr(value, "__type_params__", ())
        constructor = name == "__init__"
        if not constructor and signature.return_annotation is inspect.Signature.empty:
            self.fail(
                "missing_return_annotation",
                f"Method {declaration.name.to_id()}.{name} has no return annotation",
                location,
            )
        arguments = []
        for position, parameter in enumerate(signature.parameters.values()):
            if position == 0 and not static and parameter.name == "self":
                continue
            if parameter.annotation is inspect.Parameter.empty:
                self.fail(
                    "missing_argument_annotation",
                    f"Argument {parameter.name!r} of method "
                    f"{declaration.name.to_id()}.{name} has no type annotation",
                    location,
                )
            arguments.append(
                MethodArgDef(
                    name=parameter.name,
                    type=self.resolve(parameter.annotation, declaration, parameters),
                    default=(
                        None
                        if parameter.default is inspect.Parameter.empty
                        else DefaultValue(value=parameter.default)
                    ),
                    is_vararg=parameter.kind == inspect.Parameter.VAR_POSITIONAL,
                    is_kwarg=parameter.kind == inspect.Parameter.VAR_KEYWORD,
                )
            )
        result = MethodDef(
            name=name,
            args=arguments,
            return_type=(
                TypeRef(
                    kind=TypeKind.NAMED,
                    name=declaration.name.to_id(),
                )
                if constructor
                else self.resolve(
                    signature.return_annotation,
                    declaration,
                    parameters,
                )
            ),
            type_params=[value.__name__ for value in parameters],
            is_static=static,
            is_constructor=constructor,
            backend=backend,
            loc=location,
        )
        if name in ("__eq__", "__hash__"):
            fields = self.special_method_fields(name, value, declaration, location)
            if name == "__eq__":
                result.equality_fields = fields
            else:
                result.hash_fields = fields
        return result

    def tuple_fields(
        self,
        value: ast.expr,
        receiver: str,
        declaration: DeclarationBuild,
        location: SourceLoc,
    ) -> list[str]:
        if not isinstance(value, ast.Tuple):
            self.fail(
                "unsupported_special_method",
                f"Special method in {declaration.name.to_id()!r} must "
                "operate on a tuple of direct fields",
                location,
            )
        result = []
        for element in value.elts:
            if (
                not isinstance(element, ast.Attribute)
                or not isinstance(element.value, ast.Name)
                or element.value.id != receiver
            ):
                self.fail(
                    "unsupported_special_method",
                    f"Expected {receiver}.field in tuple for "
                    f"{declaration.name.to_id()!r}, got {ast.unparse(element)!r}",
                    location,
                )
            result.append(element.attr)
        return result

    def special_method_fields(
        self,
        name: str,
        function: Callable[..., Any],
        declaration: DeclarationBuild,
        location: SourceLoc,
    ) -> list[str] | None:
        tree = ast.parse(textwrap.dedent(inspect.getsource(function)))
        definition = tree.body[0]
        if not isinstance(definition, ast.FunctionDef):
            self.fail(
                "unsupported_special_method",
                f"Cannot inspect {declaration.name.to_id()}.{name} as "
                "a regular function",
                location,
            )
        body = definition.body
        if (
            len(body) == 1
            and isinstance(body[0], ast.Expr)
            and isinstance(body[0].value, ast.Constant)
            and body[0].value.value is Ellipsis
        ):
            return None
        if (
            len(body) != 1
            or not isinstance(body[0], ast.Return)
            or body[0].value is None
        ):
            self.fail(
                "unsupported_special_method",
                f"{declaration.name.to_id()}.{name} must contain one return "
                "statement, or an ellipsis declaration",
                location,
            )
        expression = body[0].value
        if name == "__hash__":
            if (
                not isinstance(expression, ast.Call)
                or not isinstance(expression.func, ast.Name)
                or expression.func.id != "hash"
                or len(expression.args) != 1
                or expression.keywords
            ):
                self.fail(
                    "unsupported_hash_body",
                    f"{declaration.name.to_id()}.__hash__ must return "
                    "hash((self.field1, self.field2, ...))",
                    location,
                )
            return self.tuple_fields(expression.args[0], "self", declaration, location)
        if (
            not isinstance(expression, ast.Compare)
            or len(expression.ops) != 1
            or not isinstance(expression.ops[0], ast.Eq)
            or len(expression.comparators) != 1
        ):
            self.fail(
                "unsupported_equality_body",
                f"{declaration.name.to_id()}.__eq__ must compare "
                "(self.field1, ...) == (other.field1, ...)",
                location,
            )
        left = self.tuple_fields(expression.left, "self", declaration, location)
        right = self.tuple_fields(
            expression.comparators[0], "other", declaration, location
        )
        if left != right:
            self.fail(
                "mismatched_equality_fields",
                f"{declaration.name.to_id()}.__eq__ compares different "
                f"field sequences: self uses {left!r}, other uses {right!r}",
                location,
            )
        return left

    def effective_fields(
        self,
        name: str,
        stack: tuple[str, ...] = (),
    ) -> dict[str, FieldDef]:
        if name in self.effective:
            return self.effective[name]
        if name in stack:
            record = self.records[name]
            self.fail(
                "inheritance_cycle",
                f"Inheritance cycle: {' -> '.join((*stack, name))}",
                record.loc,
            )
        record = self.records[name]
        result: dict[str, FieldDef] = {}
        for base in record.bases:
            if base.name not in self.records:
                self.fail(
                    "external_base",
                    f"Record {name!r} inherits from {base.name!r}, whose "
                    "fields are unavailable in this model",
                    record.loc,
                )
            for field_name, value in self.effective_fields(
                base.name, (*stack, name)
            ).items():
                if field_name in result and result[field_name].type != value.type:
                    self.fail(
                        "ambiguous_inherited_field",
                        f"Record {name!r} inherits incompatible types for "
                        f"field {field_name!r}: "
                        f"{result[field_name].type.display()} and "
                        f"{value.type.display()}",
                        record.loc,
                    )
                result[field_name] = value
        for value in record.fields:
            if value.name in result and result[value.name].type != value.type:
                self.fail(
                    "incompatible_field_override",
                    f"Field {name}.{value.name} changes inherited type "
                    f"{result[value.name].type.display()} to "
                    f"{value.type.display()}",
                    value.loc,
                )
            result[value.name] = value
        self.effective[name] = result
        return result

    def validate_default(
        self,
        value: Any,
        target: TypeRef,
        context: str,
        location: SourceLoc,
    ) -> None:
        def reject(reason: str) -> None:
            self.fail(
                "invalid_default",
                f"Default for {context} is {value!r}, but "
                f"{target.display()} is required: {reason}",
                location,
            )

        if value is None:
            if target.kind not in (
                TypeKind.NONE,
                TypeKind.OPTIONAL,
                TypeKind.ANY,
            ):
                reject("this type does not accept the no-value default")
            return
        match target.kind:
            case TypeKind.ANY:
                return
            case TypeKind.OPTIONAL:
                self.validate_default(value, target.args[0], context, location)
            case TypeKind.INT8 | TypeKind.INT16 | TypeKind.INT32 | TypeKind.INT64:
                widths = {
                    TypeKind.INT8: 8,
                    TypeKind.INT16: 16,
                    TypeKind.INT32: 32,
                    TypeKind.INT64: 64,
                }
                width = widths[target.kind]
                if (
                    type(value) is not int
                    or value < -(2 ** (width - 1))
                    or not value < 2 ** (width - 1)
                ):
                    reject(f"expected a signed {width}-bit integer")
            case TypeKind.UINT8 | TypeKind.UINT16 | TypeKind.UINT32 | TypeKind.UINT64:
                widths = {
                    TypeKind.UINT8: 8,
                    TypeKind.UINT16: 16,
                    TypeKind.UINT32: 32,
                    TypeKind.UINT64: 64,
                }
                width = widths[target.kind]
                if type(value) is not int or value < 0 or not value < 2**width:
                    reject(f"expected an unsigned {width}-bit integer")
            case TypeKind.FLOAT32 | TypeKind.FLOAT64:
                if type(value) not in (int, float):
                    reject("expected an integer or floating-point number")
            case TypeKind.STRING:
                if not isinstance(value, str):
                    reject("expected a string")
            case TypeKind.BOOL:
                if type(value) is not bool:
                    reject("expected a boolean")
            case TypeKind.LIST | TypeKind.ARRAY:
                if not isinstance(value, (list, tuple)):
                    reject("expected a sequence")
                if target.kind == TypeKind.ARRAY and len(value) != target.size:
                    reject(f"expected {target.size} elements, got {len(value)}")
                for position, item in enumerate(value):
                    self.validate_default(
                        item,
                        target.args[0],
                        f"{context}[{position}]",
                        location,
                    )
            case TypeKind.TUPLE:
                if not isinstance(value, (list, tuple)) or len(value) != len(
                    target.args
                ):
                    reject(f"expected {len(target.args)} tuple elements")
                for position, (item, item_type) in enumerate(
                    zip(value, target.args, strict=True)
                ):
                    self.validate_default(
                        item,
                        item_type,
                        f"{context}[{position}]",
                        location,
                    )
            case TypeKind.MAP:
                if not isinstance(value, dict):
                    reject("expected a mapping")
                for key, item in value.items():
                    self.validate_default(
                        key, target.args[0], f"{context} key", location
                    )
                    self.validate_default(
                        item,
                        target.args[1],
                        f"{context}[{key!r}]",
                        location,
                    )
            case TypeKind.NAMED:
                record = self.records.get(target.name)
                if record is None:
                    reject(
                        "an explicit default for an external opaque type "
                        "cannot be checked"
                    )
                if record.kind == RecordKind.ENUM:
                    accepted = [entry.value for entry in record.enum_values]
                    if type(value) is not int or value not in accepted:
                        reject(f"expected an enum value from {accepted!r}")
                    return
                if not isinstance(value, dict):
                    reject("expected a field-name mapping")
                fields = self.effective_fields(target.name)
                for name, item in value.items():
                    if name not in fields:
                        reject(f"unknown field {name!r}; expected {sorted(fields)!r}")
                    self.validate_default(
                        item,
                        fields[name].type,
                        f"{context}.{name}",
                        location,
                    )
            case TypeKind.VARIANT:
                reject(
                    "explicit variant defaults are not represented by a "
                    "bare payload; use implicit initialization in this iteration"
                )
            case _:
                reject("this type has no supported explicit default representation")

    def emit_model(self) -> None:
        for record in self.records.values():
            name = record.name.to_id()
            named = TypeRef(kind=TypeKind.NAMED, name=name)
            self.register_type(named)
            self.fact("record", name)
            for base in record.bases:
                self.fact("inheritance", name, base.name)
            for value in self.effective_fields(name).values():
                self.fact(
                    "field",
                    souffle_type(named),
                    value.name,
                    souffle_type(value.type),
                )
            for entry in record.enum_values:
                self.fact("enum_entry", souffle_type(named), entry.name)
            for variant in record.variants:
                owner = TypeRef(
                    kind=TypeKind.VARIANT,
                    args=tuple(variant.cases),
                )
                for tag, case in zip(variant.tags, variant.cases, strict=True):
                    self.fact(
                        "variant_case",
                        souffle_type(owner),
                        tag,
                        souffle_type(case),
                        souffle_type(variant.enum_type),
                    )
            for value in record.fields:
                if value.default is not None:
                    self.validate_default(
                        value.default.value,
                        value.type,
                        f"{name}.{value.name}",
                        value.loc,
                    )
            fields = self.effective_fields(name)
            equality: list[str] | None = None
            hashing: list[str] | None = None
            for method in record.methods:
                selected = (
                    method.equality_fields
                    if method.equality_fields is not None
                    else method.hash_fields
                )
                if selected is not None:
                    for field_name in selected:
                        if field_name not in fields:
                            self.fail(
                                "unknown_special_method_field",
                                f"{name}.{method.name} references field "
                                f"{field_name!r}; expected {sorted(fields)!r}",
                                method.loc,
                            )
                if method.equality_fields is not None:
                    equality = method.equality_fields
                if method.hash_fields is not None:
                    hashing = method.hash_fields
            if equality is not None and hashing is not None:
                extra = sorted(set(hashing) - set(equality))
                if extra:
                    self.fail(
                        "inconsistent_hash_fields",
                        f"{name} hashes fields {extra!r} that are not used "
                        "by equality, so equal values could hash differently",
                        record.loc,
                    )

        for value in tuple(self.types):
            if value.kind != TypeKind.VARIANT:
                continue
            alternatives = [
                self.effective_fields(case.name)
                if case.kind == TypeKind.NAMED and case.name in self.records
                else {}
                for case in value.args
            ]
            if not alternatives:
                continue
            common = set(alternatives[0])
            for fields in alternatives[1:]:
                common.intersection_update(fields)
            for name in sorted(common):
                target = alternatives[0][name].type
                if all(fields[name].type == target for fields in alternatives):
                    self.fact(
                        "field",
                        souffle_type(value),
                        name,
                        souffle_type(target),
                    )

    def allocate_binding(self) -> int:
        result = self.binding_counter
        self.binding_counter += 1
        return result

    def expression(
        self,
        node: BuildExpr,
        declaration: DeclarationBuild,
        this_type: TypeRef,
        bindings: dict[str, int],
        source: int | None = None,
    ) -> int:
        if node.op not in self.operations:
            self.fail(
                "unknown_operation",
                f"Operation {node.op!r} is not registered; expected "
                f"{sorted(self.operations)!r}",
                node.loc,
            )
        operation = self.operations[node.op]
        operation.validate_arity(len(node.args), f"{node.loc.file}:{node.loc.line}")
        target = (
            self.resolve(node.annotation, declaration)
            if node.annotation is not None
            else None
        )
        occurrence = ExprOccurrence(
            id=len(self.expressions),
            op=node.op,
            text=node.text,
            value=node.value,
            target=target,
            binder=node.binder,
            loc=node.loc,
        )
        self.expressions.append(occurrence)
        eid = occurrence.id
        self.fact("arity", eid, len(node.args))
        payload = (
            node.text
            if node.op
            in (
                Op.FIELD,
                Op.CASE_IS,
                Op.ENUM_IS,
                Op.TYPE_MATCH,
            )
            else ""
        )
        self.fact(
            "expression",
            eid,
            node.op,
            payload,
            node.value if node.op == Op.NTH else 0,
            souffle_type(target or TypeRef(kind=TypeKind.ANY)),
        )

        if operation.behavior == "this":
            self.fact("this_type", eid, souffle_type(this_type))

        if operation.behavior == "reference":
            if node.text not in bindings:
                self.fail(
                    "unknown_binding",
                    f"Iterator {node.text!r} is not bound in this expression; "
                    f"available bindings are {sorted(bindings)!r}",
                    node.loc,
                )
            occurrence.binding = bindings[node.text]
            self.fact("reference", eid, occurrence.binding)

        match operation.binding:
            case BindingMode.COLLECTION:
                if node.binder is None:
                    self.fail(
                        "missing_binding",
                        f"Collection operation {node.op!r} requires "
                        "an explicit iterator name",
                        node.loc,
                    )
                binding = self.allocate_binding()
                occurrence.binding = binding
                self.fact("collection_binding", binding, eid)
                collection = self.expression(
                    node.args[0], declaration, this_type, bindings
                )
                body = self.expression(
                    node.args[1],
                    declaration,
                    this_type,
                    {**bindings, node.binder: binding},
                )
                occurrence.args = [collection, body]
            case BindingMode.MATCH:
                scrutinee = self.expression(
                    node.args[0], declaration, this_type, bindings
                )
                occurrence.args = [scrutinee]
                for case in node.args[1:]:
                    occurrence.args.append(
                        self.expression(
                            case,
                            declaration,
                            this_type,
                            bindings,
                            source=scrutinee,
                        )
                    )
            case BindingMode.CASE:
                if source is None or node.binder is None:
                    self.fail(
                        "unattached_type_case",
                        "type_case must appear directly inside type_cond "
                        "and provide a binding name",
                        node.loc,
                    )
                binding = self.allocate_binding()
                occurrence.binding = binding
                self.fact("case_binding", binding, eid)
                self.fact("case_source", eid, source)
                occurrence.args = [
                    self.expression(
                        node.args[0],
                        declaration,
                        this_type,
                        {**bindings, node.binder: binding},
                    )
                ]
            case BindingMode.NONE:
                occurrence.args = [
                    self.expression(argument, declaration, this_type, bindings)
                    for argument in node.args
                ]
        for position, argument in enumerate(occurrence.args):
            self.fact("argument", eid, position, argument)
        return eid

    def emit_checks(self) -> None:
        identifiers: set[str] = set()
        for declaration, check in self.check_builds:
            owner = declaration.name.to_id()
            identifier = check.id or f"check:{check.loc.line}"
            qualified = f"{owner}/{identifier}"
            if qualified in identifiers:
                self.fail(
                    "duplicate_check",
                    f"Check identifier {identifier!r} is repeated in {owner!r}",
                    check.loc,
                )
            identifiers.add(qualified)
            this_type = (
                self.effective_fields(owner)[check.field_name].type
                if check.field_name is not None
                else TypeRef(kind=TypeKind.NAMED, name=owner)
            )
            start = len(self.expressions)
            condition = self.expression(check.condition, declaration, this_type, {})
            message = self.expression(check.message, declaration, this_type, {})
            definition = CheckDef(
                id=identifier,
                owner=owner,
                field_name=check.field_name,
                condition=condition,
                message=message,
                severity=check.severity,
                loc=check.loc,
            )
            self.checks.append(definition)
            self.fact(
                "expected",
                condition,
                souffle_type(TypeRef(kind=TypeKind.BOOL)),
            )
            self.fact(
                "expected",
                message,
                souffle_type(TypeRef(kind=TypeKind.STRING)),
            )
            for occurrence in self.expressions[start:]:
                self.expression_checks[occurrence.id] = definition

    def expression_text(self, eid: int) -> str:
        expression = self.expressions[eid]
        arguments = [self.expression_text(argument) for argument in expression.args]
        match expression.op:
            case Op.THIS:
                return "This"
            case Op.ITER:
                return f"Iter({expression.text!r})"
            case Op.FIELD:
                return f"{arguments[0]}[{expression.text!r}]"
            case Op.INT | Op.FLOAT | Op.STRING | Op.BOOL | Op.NONE:
                return repr(expression.value)
            case _:
                payload = [repr(expression.text)] if expression.text else []
                return f"{expression.op}({', '.join((*arguments, *payload))})"

    def receiver(self, occurrence: ExprOccurrence) -> TypeRef | None:
        if not occurrence.args:
            return None
        value = self.expressions[occurrence.args[0]].type
        if value is not None and value.kind == TypeKind.LOCATED:
            return value.args[0]
        return value

    def diagnose(self, eid: int, code: str) -> Diagnostic:
        occurrence = self.expressions[eid]
        receiver = self.receiver(occurrence)
        operands = [
            (
                self.expressions[value].type.display()
                if self.expressions[value].type is not None
                else "untyped"
            )
            for value in occurrence.args
        ]
        message = (
            f"Operation {occurrence.op!r} cannot accept operand types {operands!r}"
        )
        if code == "unexpected_result":
            check = self.expression_checks[eid]
            expected = "bool" if check.condition == eid else "string"
            actual = (
                occurrence.type.display() if occurrence.type is not None else "untyped"
            )
            message = (
                f"Check result must be {expected}, but "
                f"{self.expression_text(eid)} has type {actual}"
            )
        elif code == "unhandled_type":
            message = (
                "Type condition does not cover every possible input type; "
                "add matching cases or set unhandled_ignore=True"
            )
        elif occurrence.op == Op.FIELD and receiver is not None:
            fields = (
                sorted(self.effective_fields(receiver.name))
                if (receiver.kind == TypeKind.NAMED and receiver.name in self.records)
                else []
            )
            message = (
                f"Field {occurrence.text!r} is unavailable on "
                f"{receiver.display()}; expected fields {fields!r}"
            )
        elif occurrence.op == Op.CASE_IS and receiver is not None:
            tags = sorted(
                tag
                for record in self.records.values()
                for variant in record.variants
                if TypeRef(
                    kind=TypeKind.VARIANT,
                    args=tuple(variant.cases),
                )
                == receiver
                for tag in variant.tags
            )
            message = (
                f"Variant case {occurrence.text!r} is unavailable on "
                f"{receiver.display()}; expected cases {tags!r}"
            )
        elif occurrence.op == Op.ENUM_IS and receiver is not None:
            record = self.records.get(receiver.name)
            names = (
                [entry.name for entry in record.enum_values]
                if record is not None
                else []
            )
            message = (
                f"Enum value {occurrence.text!r} is unavailable on "
                f"{receiver.display()}; expected values {names!r}"
            )
        elif occurrence.op == Op.NTH and receiver is not None:
            message = (
                f"Tuple index {occurrence.value!r} is invalid for {receiver.display()}"
            )
        elif occurrence.op == Op.TYPE_CASE:
            message = (
                f"Type case {occurrence.target.display() if occurrence.target else '?'} "
                "cannot match the input type"
            )

        notes = [DiagnosticNote(message=f"Expression: {self.expression_text(eid)}")]
        for position, argument in enumerate(occurrence.args):
            notes.append(
                DiagnosticNote(
                    message=(
                        f"Argument {position}: "
                        f"{self.expression_text(argument)} has type "
                        f"{operands[position]}"
                    ),
                    loc=self.expressions[argument].loc,
                )
            )
        current = eid
        while True:
            enclosing = next(
                (value for value in self.expressions if current in value.args),
                None,
            )
            if enclosing is None:
                break
            position = enclosing.args.index(current)
            notes.append(
                DiagnosticNote(
                    message=(
                        f"Used as argument {position} of "
                        f"{enclosing.op!r}: "
                        f"{self.expression_text(enclosing.id)}"
                    ),
                    loc=enclosing.loc,
                )
            )
            current = enclosing.id
        check = self.expression_checks.get(eid)
        return Diagnostic(
            code=code,
            message=message,
            loc=occurrence.loc,
            expression=eid,
            check_id=check.id if check is not None else None,
            notes=notes,
        )

    def read_rows(self, path: Path, columns: int) -> list[list[str]]:
        with path.open(newline="") as stream:
            rows = list(
                csv.reader(
                    stream,
                    delimiter="\t",
                    quoting=csv.QUOTE_NONE,
                )
            )
        for position, row in enumerate(rows, start=1):
            if len(row) != columns:
                raise ValueError(
                    f"Malformed Soufflé output {path}:{position}: "
                    f"expected {columns} columns, got {len(row)} in {row!r}"
                )
        return rows

    def run_souffle(self) -> None:
        for operation in self.operations.values():
            self.fact("operation", operation.name, operation.behavior)
        program = Path(__file__).with_suffix(".dl").read_text()
        additions = "\n".join(
            operation.rules() for operation in self.operations.values()
        )
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "model.dl"
            source.write_text(f"{program}\n{additions}\n")
            for name, rows in self.facts.items():
                text = "".join(f"{'\t'.join(row)}\n" for row in rows)
                (root / f"{name}.facts").write_text(text)
            local["souffle"][
                "-F",
                str(root),
                "-D",
                str(root),
                str(source),
            ]()
            for row in self.read_rows(root / "inferred.csv", 2):
                eid = int(row[0])
                inferred = parse_souffle_type(row[1])
                occurrence = self.expressions[eid]
                if occurrence.type is not None and occurrence.type != inferred:
                    self.fail(
                        "ambiguous_expression_type",
                        f"Expression {self.expression_text(eid)} received "
                        f"incompatible inferred types "
                        f"{occurrence.type.display()} and {inferred.display()}",
                        occurrence.loc,
                    )
                occurrence.type = inferred
            diagnostics = [
                self.diagnose(int(row[0]), row[1])
                for row in self.read_rows(root / "problem.csv", 3)
            ]
            if diagnostics:
                raise DslValidationError(diagnostics)
            missing = [value for value in self.expressions if value.type is None]
            if missing:
                diagnostics = [
                    Diagnostic(
                        code="unresolved_expression",
                        message=(
                            f"Expression {self.expression_text(value.id)} "
                            "has no inferred type and no local diagnostic"
                        ),
                        loc=value.loc,
                        expression=value.id,
                    )
                    for value in missing
                ]
                raise DslValidationError(diagnostics)

    def compile(self) -> ModelIR:
        self.collect_records()
        self.emit_model()
        self.emit_checks()
        self.run_souffle()
        return ModelIR(
            package=self.model.package,
            records=list(self.records.values()),
            expressions=self.expressions,
            checks=self.checks,
        )


@beartype
def get_dsl_model(
    package: str = "",
    operations: Sequence[Operation] | None = None,
) -> DslModel:
    return DslModel(package=package, operations=operations)

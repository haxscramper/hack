#!/usr/bin/env python
from __future__ import annotations

import csv
import enum
import inspect
import sys
import tempfile
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


@beartype
class SourceLoc(BaseModel):
    file: str
    line: int
    function: str


@beartype
def get_caller_loc(depth: int = 2) -> SourceLoc:
    frame: Optional[types.FrameType] = sys._getframe(1)
    this_file = frame.f_code.co_filename if frame else ""
    while frame is not None:
        fname = frame.f_code.co_filename
        if fname != this_file and not fname.startswith("<@beartype"):
            return SourceLoc(
                file=fname,
                line=frame.f_lineno,
                function=frame.f_code.co_name,
            )
        frame = frame.f_back
    fallback = sys._getframe(1)
    return SourceLoc(
        file=fallback.f_code.co_filename,
        line=fallback.f_lineno,
        function=fallback.f_code.co_name,
    )


@beartype
class QName(BaseModel):
    name: str
    scope: Optional[QName] = None
    params: List[QName] = PydanticField(default_factory=list)
    is_external: bool = False
    package: Optional[str] = None
    fixed_size: Optional[int] = None

    def to_id(self) -> str:
        prefix = f"{self.scope.to_id()}::" if self.scope else ""
        pkg = f"@{self.package}:" if self.is_external and self.package else ""
        params_str = (
            f"<{','.join(p.to_id() for p in self.params)}>" if self.params else ""
        )
        size_str = f"[{self.fixed_size}]" if self.fixed_size is not None else ""
        return f"{pkg}{prefix}{self.name}{params_str}{size_str}"


class ExprOp(str, enum.Enum):
    THIS = "this"
    ITER = "iter"
    CONST_INT = "const_int"
    CONST_FLOAT = "const_float"
    CONST_STR = "const_str"
    CONST_BOOL = "const_bool"
    GET_FIELD = "get_field"
    EQ = "eq"
    NE = "ne"
    LT = "lt"
    LE = "le"
    AND = "and"
    OR = "or"
    NOT = "not"
    ALL = "all"
    MAP = "map"
    STR_SIZE = "str_size"
    STR_REGEX = "str_regex"
    STR_FORMAT = "str_format"
    LIST_SIZE = "list_size"
    MAP_SIZE = "map_size"
    MAP_CONTAINS = "map_contains"
    OPT_HAS_VALUE = "opt_has_value"
    OPT_VALUE = "opt_value"
    CASE_IS = "case_is"
    CASE_WHICH = "case_which"
    ENUM_IS = "enum_is"
    NTH_GET = "nth_get"
    IS_DERIVED = "is_derived"


@beartype
class ExprNode(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    id: int
    op: ExprOp
    args: List[ExprNode] = PydanticField(default_factory=list)
    str_val: Optional[str] = None
    int_val: Optional[int] = None
    float_val: Optional[float] = None
    bool_val: Optional[bool] = None
    type_val: Optional[QName] = None
    loc: SourceLoc

    def __eq__(self, other: object) -> ExprNode:  # type: ignore[override]
        return make_expr(ExprOp.EQ, [self, wrap_expr(other, depth=2)], depth=2)

    def __ne__(self, other: object) -> ExprNode:  # type: ignore[override]
        return make_expr(ExprOp.NE, [self, wrap_expr(other, depth=2)], depth=2)

    def __lt__(self, other: object) -> ExprNode:
        return make_expr(ExprOp.LT, [self, wrap_expr(other, depth=2)], depth=2)

    def __le__(self, other: object) -> ExprNode:
        return make_expr(ExprOp.LE, [self, wrap_expr(other, depth=2)], depth=2)

    def __gt__(self, other: object) -> ExprNode:
        return make_expr(ExprOp.LT, [wrap_expr(other, depth=2), self], depth=2)

    def __ge__(self, other: object) -> ExprNode:
        return make_expr(ExprOp.LE, [wrap_expr(other, depth=2), self], depth=2)

    def __and__(self, other: object) -> ExprNode:
        return make_expr(ExprOp.AND, [self, wrap_expr(other, depth=2)], depth=2)

    def __or__(self, other: object) -> ExprNode:
        return make_expr(ExprOp.OR, [self, wrap_expr(other, depth=2)], depth=2)

    def __invert__(self) -> ExprNode:
        return make_expr(ExprOp.NOT, [self], depth=2)

    def __getitem__(self, key: str) -> ExprNode:
        return make_expr(ExprOp.GET_FIELD, [self], str_val=key, depth=2)

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

    def is_derived_from(self, target: Any) -> ExprNode:
        qname = resolve_type_annotation(target, None)
        return make_expr(ExprOp.IS_DERIVED, [self], type_val=qname, depth=2)


EXPR_COUNTER: List[int] = [0]


@beartype
def next_expr_id() -> int:
    val = EXPR_COUNTER[0]
    EXPR_COUNTER[0] += 1
    return val


@beartype
def make_expr(
    op: ExprOp,
    args: List[ExprNode] = [],
    str_val: Optional[str] = None,
    int_val: Optional[int] = None,
    float_val: Optional[float] = None,
    bool_val: Optional[bool] = None,
    type_val: Optional[QName] = None,
    depth: int = 2,
) -> ExprNode:
    return ExprNode(
        id=next_expr_id(),
        op=op,
        args=args,
        str_val=str_val,
        int_val=int_val,
        float_val=float_val,
        bool_val=bool_val,
        type_val=type_val,
        loc=get_caller_loc(depth + 1),
    )


@beartype
def wrap_expr(val: Any, depth: int = 2) -> ExprNode:
    match val:
        case ExprNode():
            return val
        case bool():
            return make_expr(ExprOp.CONST_BOOL, bool_val=val, depth=depth + 1)
        case int():
            return make_expr(ExprOp.CONST_INT, int_val=val, depth=depth + 1)
        case float():
            return make_expr(ExprOp.CONST_FLOAT, float_val=val, depth=depth + 1)
        case str():
            return make_expr(ExprOp.CONST_STR, str_val=val, depth=depth + 1)
        case _:
            raise TypeError(f"Cannot convert {val!r} of type {type(val)} into ExprNode")


@beartype
class StrAccessor:
    def __init__(self, target: Optional[ExprNode] = None) -> None:
        self.target = target

    def size(self) -> ExprNode:
        if self.target is None:
            raise ValueError("size() requires an instance target")
        return make_expr(ExprOp.STR_SIZE, [self.target], depth=2)

    def matches(self, pattern: str) -> ExprNode:
        if self.target is None:
            raise ValueError("matches() requires an instance target")
        return make_expr(ExprOp.STR_REGEX, [self.target], str_val=pattern, depth=2)

    def format(self, args: Sequence[Any]) -> ExprNode:
        if len(args) == 0:
            raise ValueError("format() requires at least a format string")
        wrapped = [wrap_expr(a, depth=2) for a in args]
        return make_expr(ExprOp.STR_FORMAT, wrapped, depth=2)


@beartype
class ListAccessor:
    def __init__(self, target: ExprNode) -> None:
        self.target = target

    def size(self) -> ExprNode:
        return make_expr(ExprOp.LIST_SIZE, [self.target], depth=2)


@beartype
class MapAccessor:
    def __init__(self, target: ExprNode) -> None:
        self.target = target

    def size(self) -> ExprNode:
        return make_expr(ExprOp.MAP_SIZE, [self.target], depth=2)

    def contains(self, key: Any) -> ExprNode:
        return make_expr(
            ExprOp.MAP_CONTAINS,
            [self.target, wrap_expr(key, depth=2)],
            depth=2,
        )


@beartype
class OptAccessor:
    def __init__(self, target: ExprNode) -> None:
        self.target = target

    def has_value(self) -> ExprNode:
        return make_expr(ExprOp.OPT_HAS_VALUE, [self.target], depth=2)

    def value(self) -> ExprNode:
        return make_expr(ExprOp.OPT_VALUE, [self.target], depth=2)


@beartype
class CaseAccessor:
    def __init__(self, target: ExprNode) -> None:
        self.target = target

    def is_case(self, name: str) -> ExprNode:
        return make_expr(ExprOp.CASE_IS, [self.target], str_val=name, depth=2)

    def which_case(self) -> ExprNode:
        return make_expr(ExprOp.CASE_WHICH, [self.target], depth=2)


@beartype
class EnumAccessor:
    def __init__(self, target: ExprNode) -> None:
        self.target = target

    def is_val(self, name: str) -> ExprNode:
        return make_expr(ExprOp.ENUM_IS, [self.target], str_val=name, depth=2)


@beartype
class NthAccessor:
    def __init__(self, target: ExprNode) -> None:
        self.target = target

    def get(self, index: int) -> ExprNode:
        return make_expr(ExprOp.NTH_GET, [self.target], int_val=index, depth=2)


@beartype
def op_and(*args: Any) -> ExprNode:
    if len(args) == 0:
        raise ValueError("op_and requires at least one argument")
    cur = wrap_expr(args[0], depth=2)
    for arg in args[1:]:
        cur = make_expr(ExprOp.AND, [cur, wrap_expr(arg, depth=2)], depth=2)
    return cur


@beartype
def op_or(*args: Any) -> ExprNode:
    if len(args) == 0:
        raise ValueError("op_or requires at least one argument")
    cur = wrap_expr(args[0], depth=2)
    for arg in args[1:]:
        cur = make_expr(ExprOp.OR, [cur, wrap_expr(arg, depth=2)], depth=2)
    return cur


@beartype
def op_not(arg: Any) -> ExprNode:
    return make_expr(ExprOp.NOT, [wrap_expr(arg, depth=2)], depth=2)


@beartype
def op_all(collection: ExprNode, predicate: ExprNode) -> ExprNode:
    return make_expr(ExprOp.ALL, [collection, predicate], depth=2)


@beartype
def op_map(collection: ExprNode, transform: ExprNode) -> ExprNode:
    return make_expr(ExprOp.MAP, [collection, transform], depth=2)


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
    pass


@beartype
class FieldSpec(BaseModel):
    doc: str = ""
    default: Optional[Any] = None
    required: bool = True
    check: Optional[ExprNode] = None
    loc: SourceLoc


@beartype
class CheckDef(BaseModel):
    id: str = ""
    cond: ExprNode
    message: ExprNode
    loc: SourceLoc


@beartype
class MethodArgDef(BaseModel):
    name: str
    type: QName
    default: Optional[Any] = None
    is_vararg: bool = False
    is_kwarg: bool = False


@beartype
class MethodDef(BaseModel):
    name: str
    args: List[MethodArgDef] = PydanticField(default_factory=list)
    return_type: QName
    type_params: List[str] = PydanticField(default_factory=list)
    is_static: bool = False
    is_constructor: bool = False
    backend: Dict[str, Any] = PydanticField(default_factory=dict)
    doc: str = ""
    loc: SourceLoc


@beartype
class FieldDef(BaseModel):
    name: str
    type: QName
    doc: str = ""
    default: Optional[Any] = None
    required: bool = True
    check: Optional[ExprNode] = None
    is_inline_variant: bool = False
    inline_cases: List[QName] = PydanticField(default_factory=list)
    loc: SourceLoc


@beartype
class EnumValueDef(BaseModel):
    name: str
    value: int
    doc: str = ""
    loc: SourceLoc


class RecordKind(str, enum.Enum):
    CLASS = "class"
    CASE = "case"
    ENUM = "enum"


@beartype
class RecordDef(BaseModel):
    kind: RecordKind
    name: QName
    doc: str = ""
    bases: List[QName] = PydanticField(default_factory=list)
    fields: List[FieldDef] = PydanticField(default_factory=list)
    enum_values: List[EnumValueDef] = PydanticField(default_factory=list)
    nested: List[RecordDef] = PydanticField(default_factory=list)
    variant_cases: List[QName] = PydanticField(default_factory=list)
    methods: List[MethodDef] = PydanticField(default_factory=list)
    checks: List[CheckDef] = PydanticField(default_factory=list)
    backend: Dict[str, Any] = PydanticField(default_factory=dict)
    loc: SourceLoc


@beartype
class ClassBuildContext:
    def __init__(self) -> None:
        self.variants: List[Any] = []
        self.checks: List[CheckDef] = []
        self.backend: Dict[str, Any] = {}


ACTIVE_CONTEXT_STACK: List[ClassBuildContext] = []


@beartype
def __variant__(*cases: Any) -> None:
    if len(ACTIVE_CONTEXT_STACK) == 0:
        raise RuntimeError("__variant__ called outside of class definition")
    ACTIVE_CONTEXT_STACK[-1].variants.extend(cases)


@beartype
def __check__(cond: Any, message: Union[str, ExprNode], id: str = "") -> None:
    if len(ACTIVE_CONTEXT_STACK) == 0:
        raise RuntimeError("__check__ called outside of class definition")
    ACTIVE_CONTEXT_STACK[-1].checks.append(
        CheckDef(
            id=id,
            cond=wrap_expr(cond, depth=2),
            message=wrap_expr(message, depth=2),
            loc=get_caller_loc(2),
        )
    )


@beartype
def __config__(backend: Dict[str, Any] = {}) -> None:
    if len(ACTIVE_CONTEXT_STACK) == 0:
        raise RuntimeError("__config__ called outside of class definition")
    ACTIVE_CONTEXT_STACK[-1].backend.update(backend)


@beartype
class ExternalTypeLookup:
    def __init__(self, package: str) -> None:
        self.package = package

    def __getitem__(self, type_path: str) -> QName:
        parts = type_path.split(".")
        cur_scope: Optional[QName] = None
        for part in parts[:-1]:
            cur_scope = QName(name=part, scope=cur_scope)
        return QName(
            name=parts[-1],
            scope=cur_scope,
            is_external=True,
            package=self.package,
        )


@beartype
def resolve_type_annotation(
    ann: Any,
    current_scope: Optional[QName],
    type_vars: Sequence[str] = (),
) -> QName:
    match ann:
        case QName():
            return ann
        case FixedArrayMeta():
            return QName(
                name="array",
                params=[
                    resolve_type_annotation(ann.elem_type, current_scope, type_vars)
                ],
                fixed_size=ann.size,
            )
        case None:
            return QName(name="none")
        case ForwardRef():
            return resolve_type_annotation(
                ann.__forward_arg__, current_scope, type_vars
            )
        case str():
            if ann in type_vars:
                return QName(name=ann)
            pkg = current_scope.package if current_scope else None
            parts = ann.split(".")
            if len(parts) == 1 and current_scope is not None:
                if current_scope.name == parts[0]:
                    return current_scope
                if (
                    current_scope.scope is not None
                    and current_scope.scope.name == parts[0]
                ):
                    return current_scope.scope
                return QName(name=parts[0], scope=current_scope.scope, package=pkg)
            sc: Optional[QName] = None
            for p in parts[:-1]:
                sc = QName(name=p, scope=sc, package=pkg)
            return QName(name=parts[-1], scope=sc, package=pkg)

    if ann is types.NoneType:
        return QName(name="none")
    if ann is int or ann is Int64:
        return QName(name="int64")
    if ann is Int8:
        return QName(name="int8")
    if ann is Int16:
        return QName(name="int16")
    if ann is Int32:
        return QName(name="int32")
    if ann is UInt8:
        return QName(name="uint8")
    if ann is UInt16:
        return QName(name="uint16")
    if ann is UInt32:
        return QName(name="uint32")
    if ann is UInt64:
        return QName(name="uint64")
    if ann is float or ann is Float64:
        return QName(name="float64")
    if ann is Float32:
        return QName(name="float32")
    if ann is str:
        return QName(name="string")
    if ann is bool:
        return QName(name="bool")
    if ann is Any:
        return QName(name="any")

    if hasattr(ann, "__dsl_qname__"):
        return ann.__dsl_qname__

    if hasattr(ann, "__name__") and ann.__name__ in type_vars:
        return QName(name=ann.__name__)

    origin = get_origin(ann)
    args = get_args(ann)

    if origin is Annotated:
        return resolve_type_annotation(args[0], current_scope, type_vars)

    if origin is list or origin is List:
        return QName(
            name="list",
            params=[resolve_type_annotation(args[0], current_scope, type_vars)],
        )

    if origin is dict or origin is Dict:
        return QName(
            name="map",
            params=[
                resolve_type_annotation(args[0], current_scope, type_vars),
                resolve_type_annotation(args[1], current_scope, type_vars),
            ],
        )

    if origin is tuple or origin is Tuple:
        return QName(
            name="tuple",
            params=[resolve_type_annotation(a, current_scope, type_vars) for a in args],
        )

    if origin is Union or origin is types.UnionType:
        non_none = [a for a in args if a is not types.NoneType]
        if len(non_none) == 1 and len(args) == 2:
            return QName(
                name="optional",
                params=[resolve_type_annotation(non_none[0], current_scope, type_vars)],
            )
        return QName(
            name="variant",
            params=[resolve_type_annotation(a, current_scope, type_vars) for a in args],
        )

    raise TypeError(f"Unsupported type annotation: {ann!r}")


@beartype
class MethodWrapper:
    def __init__(
        self, func: Callable[..., Any], backend: Dict[str, Any], loc: SourceLoc
    ) -> None:
        self.func = func
        self.backend = backend
        self.loc = loc


@beartype
def parse_method(
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

    ret_qname = (
        class_qname
        if is_constructor
        else resolve_type_annotation(annotations["return"], class_qname, type_params)
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
        if isinstance(raw_ann, str) and raw_ann in class_ns:
            raw_ann = class_ns[raw_ann]
        arg_qname = resolve_type_annotation(raw_ann, class_qname, type_params)
        default_val = (
            None if param.default is inspect.Parameter.empty else param.default
        )
        method_args.append(
            MethodArgDef(
                name=param.name,
                type=arg_qname,
                default=default_val,
                is_vararg=(param.kind == inspect.Parameter.VAR_POSITIONAL),
                is_kwarg=(param.kind == inspect.Parameter.VAR_KEYWORD),
            )
        )

    return MethodDef(
        name=name,
        args=method_args,
        return_type=ret_qname,
        type_params=type_params,
        is_static=is_static,
        is_constructor=is_constructor,
        backend=backend,
        doc=inspect.getdoc(func) or "",
        loc=loc,
    )


@beartype
def resolve_qualname_scope(qualname: str, model: DslModel) -> Optional[QName]:
    parts = qualname.split(".")
    if len(parts) <= 1:
        return None
    cur: Optional[QName] = None
    for part in parts[:-1]:
        if part == "<locals>":
            continue
        cur = QName(name=part, scope=cur, package=model.package)
    return cur


class DslMeta(type):
    @classmethod
    def __prepare__(
        mcs, name: str, bases: Tuple[type, ...], **kwargs: Any
    ) -> Dict[str, Any]:
        ACTIVE_CONTEXT_STACK.append(ClassBuildContext())
        return {}

    def __new__(
        mcs,
        name: str,
        bases: Tuple[type, ...],
        namespace: Dict[str, Any],
        **kwargs: Any,
    ) -> DslMeta:
        ctx = ACTIVE_CONTEXT_STACK.pop()
        cls_obj = super().__new__(mcs, name, bases, namespace)

        if namespace.get("__dsl_root__", False):
            return cls_obj

        model: DslModel = getattr(cls_obj, "__dsl_model__")
        rec_kind: RecordKind = getattr(cls_obj, "__dsl_kind__")
        qualname = namespace.get("__qualname__", name)
        scope = resolve_qualname_scope(qualname, model)
        qname = QName(name=name, scope=scope, package=model.package)
        setattr(cls_obj, "__dsl_qname__", qname)

        loc = get_caller_loc(1)
        try:
            _, line = inspect.getsourcelines(cls_obj)
            file = inspect.getsourcefile(cls_obj) or loc.file
            loc = SourceLoc(file=file, line=line, function=name)
        except OSError:
            pass

        base_qnames: List[QName] = []
        for b in bases:
            if "__dsl_root__" not in b.__dict__ and hasattr(b, "__dsl_qname__"):
                base_qnames.append(getattr(b, "__dsl_qname__"))

        nested_records: List[RecordDef] = []
        for attr_val in namespace.values():
            if isinstance(attr_val, DslMeta) and hasattr(attr_val, "__dsl_record__"):
                nested_rec: RecordDef = getattr(attr_val, "__dsl_record__")
                nested_records.append(nested_rec)

        annotations = namespace.get("__annotations__", {})
        fields: List[FieldDef] = []
        enum_values: List[EnumValueDef] = []

        if rec_kind == RecordKind.ENUM:
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
                raw_default = namespace.get(fname, None)
                fspec: Optional[FieldSpec] = None
                default_val: Optional[Any] = None
                required = True
                floc = loc

                if isinstance(raw_default, FieldSpec):
                    fspec = raw_default
                    default_val = fspec.default
                    required = fspec.required
                    floc = fspec.loc
                elif fname in namespace:
                    default_val = raw_default

                is_inline_variant = False
                inline_cases: List[QName] = []

                if get_origin(fann) is Annotated:
                    ann_args = get_args(fann)
                    for meta in ann_args[1:]:
                        if isinstance(meta, VariantMarker):
                            is_inline_variant = True
                            union_args = get_args(ann_args[0])
                            for ua in union_args:
                                inline_cases.append(resolve_type_annotation(ua, qname))

                resolved_fann = fann
                if isinstance(fann, str) and fann in namespace:
                    resolved_fann = namespace[fann]

                ftype = resolve_type_annotation(resolved_fann, qname)
                if ftype.name == "optional" and not isinstance(raw_default, FieldSpec):
                    required = False

                fields.append(
                    FieldDef(
                        name=fname,
                        type=ftype,
                        doc=fspec.doc if fspec else "",
                        default=default_val,
                        required=required,
                        check=fspec.check if fspec else None,
                        is_inline_variant=is_inline_variant,
                        inline_cases=inline_cases,
                        loc=floc,
                    )
                )

        methods: List[MethodDef] = []
        for attr_name, attr_val in namespace.items():
            if attr_name.startswith("__") and attr_name not in (
                "__init__",
                "__eq__",
            ):
                continue
            mdef = parse_method(attr_name, attr_val, qname, namespace)
            if mdef is not None:
                methods.append(mdef)

        variant_qnames = [resolve_type_annotation(v, qname) for v in ctx.variants]

        rec_def = RecordDef(
            kind=rec_kind,
            name=qname,
            doc=inspect.getdoc(cls_obj) or "",
            bases=base_qnames,
            fields=fields,
            enum_values=enum_values,
            nested=nested_records,
            variant_cases=variant_qnames,
            methods=methods,
            checks=ctx.checks,
            backend=ctx.backend,
            loc=loc,
        )
        setattr(cls_obj, "__dsl_record__", rec_def)
        if scope is None:
            model.records.append(rec_def)
        return cls_obj


SOUFFLE_PROGRAM = Path(__file__).with_suffix(".dl").read_text()


@beartype
class DslValidationError(Exception):
    pass


@beartype
class DslModel:
    def __init__(self, package: str = "") -> None:
        self.package = package
        self.records: List[RecordDef] = []
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

    @property
    def This(self) -> ExprNode:
        return make_expr(ExprOp.THIS)

    @property
    def Iter(self) -> ExprNode:
        return make_expr(ExprOp.ITER)

    def Field(
        self,
        doc: str = "",
        default: Optional[Any] = None,
        required: bool = True,
        check: Optional[ExprNode] = None,
    ) -> Any:
        return FieldSpec(
            doc=doc,
            default=default,
            required=required,
            check=check,
            loc=get_caller_loc(2),
        )

    def Variant(self) -> VariantMarker:
        return VariantMarker()

    def Method(
        self, backend: Dict[str, Any] = {}
    ) -> Callable[[Callable[..., Any]], MethodWrapper]:
        loc = get_caller_loc(2)

        def decorator(func: Callable[..., Any]) -> MethodWrapper:
            return MethodWrapper(func=func, backend=backend, loc=loc)

        return decorator

    def to_dict(self) -> Dict[str, Any]:
        return {
            "package": self.package,
            "records": [r.model_dump(mode="json") for r in self.records],
        }

    def validate_with_souffle(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)
            dl_file = tmp_path / "check.dl"
            dl_file.write_text(SOUFFLE_PROGRAM)

            facts: Dict[str, List[Tuple[Any, ...]]] = {
                "record": [],
                "inheritance": [],
                "field": [],
                "field_default_key": [],
                "variant_case": [],
                "inline_variant_case": [],
                "enum_entry": [],
                "type_info": [
                    ("int8", "int", "", ""),
                    ("int16", "int", "", ""),
                    ("int32", "int", "", ""),
                    ("int64", "int", "", ""),
                    ("uint8", "int", "", ""),
                    ("uint16", "int", "", ""),
                    ("uint32", "int", "", ""),
                    ("uint64", "int", "", ""),
                    ("float32", "float", "", ""),
                    ("float64", "float", "", ""),
                    ("string", "string", "", ""),
                    ("bool", "bool", "", ""),
                    ("any", "any", "", ""),
                    ("none", "none", "", ""),
                    ("list<any>", "list", "any", ""),
                ],
                "tuple_elem": [],
                "expr": [],
                "expr_arg": [],
                "root_check": [],
                "field_check": [],
            }

            registered_types: set[str] = {row[0] for row in facts["type_info"]}

            def register_qname(q: QName) -> str:
                tid = q.to_id()
                for p in q.params:
                    register_qname(p)
                if tid in registered_types:
                    return tid
                registered_types.add(tid)
                match q.name:
                    case "list":
                        facts["type_info"].append(
                            (tid, "list", q.params[0].to_id(), "")
                        )
                    case "array":
                        facts["type_info"].append(
                            (
                                tid,
                                "array",
                                q.params[0].to_id(),
                                str(q.fixed_size or 0),
                            )
                        )
                    case "map":
                        facts["type_info"].append(
                            (
                                tid,
                                "map",
                                q.params[0].to_id(),
                                q.params[1].to_id(),
                            )
                        )
                    case "optional":
                        facts["type_info"].append(
                            (tid, "optional", q.params[0].to_id(), "")
                        )
                    case "tuple":
                        facts["type_info"].append((tid, "tuple", "", ""))
                        for idx, pt in enumerate(q.params):
                            facts["tuple_elem"].append((tid, idx, pt.to_id()))
                    case "variant":
                        facts["type_info"].append((tid, "variant", "", ""))
                    case _:
                        facts["type_info"].append((tid, "record", "", ""))
                return tid

            def emit_expr(e: ExprNode) -> None:
                tval = register_qname(e.type_val) if e.type_val else ""
                facts["expr"].append(
                    (
                        e.id,
                        e.op.value,
                        e.str_val or "",
                        e.int_val or 0,
                        tval,
                        e.loc.file,
                        e.loc.line,
                    )
                )
                for idx, arg in enumerate(e.args):
                    facts["expr_arg"].append((e.id, idx, arg.id))
                    emit_expr(arg)

            def walk_record(r: RecordDef) -> None:
                rid = register_qname(r.name)
                facts["record"].append((rid, r.kind.value, r.loc.file, r.loc.line))
                for b in r.bases:
                    facts["inheritance"].append((rid, register_qname(b)))
                for ev in r.enum_values:
                    facts["enum_entry"].append((rid, ev.name))
                for vc in r.variant_cases:
                    vc_id = register_qname(vc)
                    facts["variant_case"].append((rid, vc_id))
                    facts["inline_variant_case"].append((rid, vc.name, vc_id))
                for f in r.fields:
                    ft_id = register_qname(f.type)
                    has_def = "true" if f.default is not None else "false"
                    req_str = "true" if f.required else "false"
                    facts["field"].append(
                        (
                            rid,
                            f.name,
                            ft_id,
                            req_str,
                            has_def,
                            f.loc.file,
                            f.loc.line,
                        )
                    )
                    if isinstance(f.default, dict):
                        for k in f.default.keys():
                            facts["field_default_key"].append(
                                (rid, f.name, str(k), f.loc.file, f.loc.line)
                            )
                    if f.is_inline_variant:
                        for ic in f.inline_cases:
                            ic_id = register_qname(ic)
                            facts["inline_variant_case"].append((rid, f.name, ic_id))
                            facts["variant_case"].append((ft_id, ic_id))
                    if f.check is not None:
                        emit_expr(f.check)
                        facts["field_check"].append((rid, f.name, f.check.id))
                        facts["root_check"].append((rid, f.check.id, "bool"))

                for chk in r.checks:
                    emit_expr(chk.cond)
                    emit_expr(chk.message)
                    facts["root_check"].append((rid, chk.cond.id, "bool"))
                    facts["root_check"].append((rid, chk.message.id, "string"))

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

            err_file = tmp_path / "validation_error.csv"
            if err_file.exists():
                errors: List[str] = []
                with err_file.open("r", newline="") as fobj:
                    reader = csv.reader(fobj, delimiter="\t")
                    for row in reader:
                        if len(row) == 3:
                            errors.append(f"{row[0]}:{row[1]} -> {row[2]}")
                if len(errors) != 0:
                    joined = "\n  ".join(errors)
                    raise DslValidationError(
                        f"Souffle Datalog validation failed with {len(errors)} error(s):\n  {joined}"
                    )


@beartype
def get_dsl_model(package: str = "") -> DslModel:
    return DslModel(package=package)

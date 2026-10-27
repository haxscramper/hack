#!/usr/bin/env python
from __future__ import annotations

import enum
import json

from beartype import beartype
from beartype.typing import Any
from loguru import logger
from pydantic import BaseModel, ConfigDict, Field

__all__ = [
    "CheckDef",
    "DefaultValue",
    "Diagnostic",
    "DiagnosticNote",
    "EnumValueDef",
    "ExprOccurrence",
    "FieldDef",
    "MethodArgDef",
    "MethodDef",
    "ModelIR",
    "Path",
    "PathSegment",
    "PathSegmentKind",
    "QName",
    "RecordDef",
    "RecordKind",
    "Severity",
    "SourceLoc",
    "TypeKind",
    "TypeRef",
    "VariantDef",
    "parse_souffle_type",
    "souffle_type",
]


class TypeKind(enum.StrEnum):
    INT8 = "I8"
    INT16 = "I16"
    INT32 = "I32"
    INT64 = "I64"
    UINT8 = "U8"
    UINT16 = "U16"
    UINT32 = "U32"
    UINT64 = "U64"
    FLOAT32 = "F32"
    FLOAT64 = "F64"
    STRING = "String"
    BOOL = "Bool"
    NONE = "NoneValue"
    ANY = "AnyValue"
    PATH = "PathValue"
    TYPE = "TypeValue"
    NAMED = "Named"
    PARAM = "Param"
    LIST = "List"
    ARRAY = "Array"
    MAP = "Map"
    TUPLE = "TupleValue"
    OPTIONAL = "OptionalValue"
    VARIANT = "Variant"
    LOCATED = "Located"
    TREE = "Tree"


@beartype
class SourceLoc(BaseModel):
    model_config = ConfigDict(frozen=True)

    file: str
    line: int
    function: str


@beartype
class QName(BaseModel):
    model_config = ConfigDict(frozen=True)

    package: str
    parts: tuple[str, ...]

    def to_id(self) -> str:
        return f"{self.package}:{'::'.join(self.parts)}"


@beartype
class TypeRef(BaseModel):
    model_config = ConfigDict(frozen=True)

    kind: TypeKind
    args: tuple[TypeRef, ...] = ()
    name: str = ""
    size: int = 0

    def display(self) -> str:
        match self.kind:
            case TypeKind.NAMED | TypeKind.PARAM:
                return self.name
            case TypeKind.ARRAY:
                return f"array[{self.args[0].display()}, {self.size}]"
            case TypeKind.LIST | TypeKind.OPTIONAL | TypeKind.LOCATED | TypeKind.TREE:
                return f"{self.kind.name.lower()}[{self.args[0].display()}]"
            case TypeKind.MAP:
                return f"map[{self.args[0].display()}, {self.args[1].display()}]"
            case TypeKind.TUPLE | TypeKind.VARIANT:
                values = ", ".join(value.display() for value in self.args)
                return f"{self.kind.name.lower()}[{values}]"
            case _:
                return self.kind.name.lower()


class PathSegmentKind(enum.StrEnum):
    FIELD = "field"
    INDEX = "index"
    KEY = "key"


@beartype
class PathSegment(BaseModel):
    model_config = ConfigDict(frozen=True)

    kind: PathSegmentKind
    value: str | int


@beartype
class Path(BaseModel):
    model_config = ConfigDict(frozen=True)

    segments: tuple[PathSegment, ...] = ()

    def field(self, name: str) -> Path:
        return Path(
            segments=(
                *self.segments,
                PathSegment(kind=PathSegmentKind.FIELD, value=name),
            )
        )

    def index(self, value: int) -> Path:
        return Path(
            segments=(
                *self.segments,
                PathSegment(kind=PathSegmentKind.INDEX, value=value),
            )
        )

    def key(self, value: str | int) -> Path:
        return Path(
            segments=(
                *self.segments,
                PathSegment(kind=PathSegmentKind.KEY, value=value),
            )
        )

    def display(self) -> str:
        result = "$"
        for segment in self.segments:
            match segment.kind:
                case PathSegmentKind.FIELD:
                    result += f"[{json.dumps(segment.value)}]"
                case PathSegmentKind.INDEX:
                    result += f"[{segment.value}]"
                case PathSegmentKind.KEY:
                    result += f"[{json.dumps(segment.value)}]"
        return result


class RecordKind(enum.StrEnum):
    CLASS = "class"
    CASE = "case"
    ENUM = "enum"


class Severity(enum.StrEnum):
    ERROR = "error"
    WARNING = "warning"


@beartype
class DefaultValue(BaseModel):
    value: Any


@beartype
class FieldDef(BaseModel):
    name: str
    type: TypeRef
    doc: str = ""
    default: DefaultValue | None = None
    loc: SourceLoc


@beartype
class EnumValueDef(BaseModel):
    name: str
    value: int
    doc: str = ""
    loc: SourceLoc


@beartype
class MethodArgDef(BaseModel):
    name: str
    type: TypeRef
    default: DefaultValue | None = None
    is_vararg: bool = False
    is_kwarg: bool = False


@beartype
class MethodDef(BaseModel):
    name: str
    args: list[MethodArgDef] = Field(default_factory=list)
    return_type: TypeRef
    type_params: list[str] = Field(default_factory=list)
    is_static: bool = False
    is_constructor: bool = False
    equality_fields: list[str] | None = None
    hash_fields: list[str] | None = None
    backend: dict[str, Any] = Field(default_factory=dict)
    loc: SourceLoc


@beartype
class VariantDef(BaseModel):
    field_name: str
    enum_type: TypeRef
    cases: list[TypeRef]
    tags: list[str]


@beartype
class ExprOccurrence(BaseModel):
    id: int
    op: str
    args: list[int] = Field(default_factory=list)
    text: str = ""
    value: Any = None
    target: TypeRef | None = None
    binder: str | None = None
    binding: int | None = None
    type: TypeRef | None = None
    loc: SourceLoc


@beartype
class CheckDef(BaseModel):
    id: str
    owner: str
    field_name: str | None = None
    condition: int
    message: int
    severity: Severity
    loc: SourceLoc


@beartype
class RecordDef(BaseModel):
    kind: RecordKind
    name: QName
    bases: list[TypeRef] = Field(default_factory=list)
    fields: list[FieldDef] = Field(default_factory=list)
    enum_values: list[EnumValueDef] = Field(default_factory=list)
    methods: list[MethodDef] = Field(default_factory=list)
    variants: list[VariantDef] = Field(default_factory=list)
    backend: dict[str, Any] = Field(default_factory=dict)
    loc: SourceLoc


@beartype
class DiagnosticNote(BaseModel):
    message: str
    loc: SourceLoc | None = None


@beartype
class Diagnostic(BaseModel):
    code: str
    message: str
    loc: SourceLoc
    expression: int | None = None
    check_id: str | None = None
    notes: list[DiagnosticNote] = Field(default_factory=list)

    def display(self) -> str:
        result = f"{self.loc.file}:{self.loc.line}: [{self.code}] {self.message}"
        for note in self.notes:
            location = (
                f"{note.loc.file}:{note.loc.line}: " if note.loc is not None else ""
            )
            result += f"\n  {location}{note.message}"
        return result


@beartype
class ModelIR(BaseModel):
    package: str
    records: list[RecordDef]
    expressions: list[ExprOccurrence]
    checks: list[CheckDef]


@beartype
def souffle_sequence(values: tuple[TypeRef, ...]) -> str:
    result = "$TNil"
    for value in reversed(values):
        result = f"$TCons({souffle_type(value)}, {result})"
    return result


@beartype
def souffle_type(value: TypeRef) -> str:
    constructor = f"${value.kind.value}"
    match value.kind:
        case TypeKind.NAMED | TypeKind.PARAM:
            return f"{constructor}({json.dumps(value.name)})"
        case TypeKind.TUPLE | TypeKind.VARIANT:
            return f"{constructor}({souffle_sequence(value.args)})"
        case TypeKind.ARRAY:
            return f"{constructor}({souffle_type(value.args[0])}, {value.size})"
        case _:
            if value.args:
                arguments = ", ".join(souffle_type(arg) for arg in value.args)
                return f"{constructor}({arguments})"
            return constructor


@beartype
class TypeParser:
    def __init__(self, text: str) -> None:
        self.text = text
        self.position = 0

    def spaces(self) -> None:
        while self.position < len(self.text) and self.text[self.position].isspace():
            self.position += 1

    def consume(self, expected: str) -> None:
        self.spaces()
        if not self.text.startswith(expected, self.position):
            raise ValueError(
                f"Cannot parse Soufflé type {self.text!r}: expected "
                f"{expected!r} at offset {self.position}"
            )
        self.position += len(expected)

    def constructor(self) -> str:
        self.consume("$")
        start = self.position
        while self.position < len(self.text):
            value = self.text[self.position]
            if not (value.isalnum() or value == "_"):
                break
            self.position += 1
        if start == self.position:
            raise ValueError(
                f"Cannot parse Soufflé type {self.text!r}: "
                f"missing constructor at offset {start}"
            )
        return self.text[start : self.position]

    def string(self) -> str:
        self.spaces()
        logger.info(self.text)
        value, length = json.JSONDecoder().raw_decode(self.text[self.position :])
        if not isinstance(value, str):
            raise TypeError(
                f"Expected a string in Soufflé type {self.text!r}, got {value!r}"
            )
        self.position += length
        return value

    def integer(self) -> int:
        self.spaces()
        start = self.position
        while self.position < len(self.text) and self.text[self.position].isdigit():
            self.position += 1
        return int(self.text[start : self.position])

    def sequence(self) -> tuple[TypeRef, ...]:
        name = self.constructor()
        match name:
            case "TNil":
                return ()
            case "TCons":
                self.consume("(")
                head = self.type()
                self.consume(",")
                tail = self.sequence()
                self.consume(")")
                return (head, *tail)
            case _:
                raise ValueError(
                    f"Unexpected type-list constructor {name!r} in {self.text!r}"
                )

    def type(self) -> TypeRef:
        name = self.constructor()
        kind = TypeKind(name)
        match kind:
            case TypeKind.NAMED | TypeKind.PARAM:
                self.consume("(")
                value = self.string()
                self.consume(")")
                return TypeRef(kind=kind, name=value)
            case TypeKind.TUPLE | TypeKind.VARIANT:
                self.consume("(")
                values = self.sequence()
                self.consume(")")
                return TypeRef(kind=kind, args=values)
            case TypeKind.ARRAY:
                self.consume("(")
                element = self.type()
                self.consume(",")
                size = self.integer()
                self.consume(")")
                return TypeRef(kind=kind, args=(element,), size=size)
            case TypeKind.LIST | TypeKind.OPTIONAL | TypeKind.LOCATED | TypeKind.TREE:
                self.consume("(")
                element = self.type()
                self.consume(")")
                return TypeRef(kind=kind, args=(element,))
            case TypeKind.MAP:
                self.consume("(")
                key = self.type()
                self.consume(",")
                value = self.type()
                self.consume(")")
                return TypeRef(kind=kind, args=(key, value))
            case _:
                return TypeRef(kind=kind)


@beartype
def parse_souffle_type(text: str) -> TypeRef:
    parser = TypeParser(text)
    result = parser.type()
    parser.spaces()
    if parser.position != len(text):
        raise ValueError(
            f"Unexpected trailing text in Soufflé type {text!r} "
            f"at offset {parser.position}"
        )
    return result

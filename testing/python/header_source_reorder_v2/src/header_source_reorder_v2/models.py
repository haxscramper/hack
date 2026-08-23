from __future__ import annotations

from dataclasses import dataclass
from enum import Enum


class Qualifier(Enum):
    CONST = "const"
    VOLATILE = "volatile"
    PTR = "ptr"
    LVALUE_REF = "lvalue_ref"
    RVALUE_REF = "rvalue_ref"
    NOEXCEPT = "noexcept"


class HeaderEntryKind(Enum):
    NAMESPACE = "namespace"
    CLASS = "class"
    STRUCT = "struct"
    UNION = "union"
    METHOD = "method"
    FUNCTION = "function"


class SourceBlockKind(Enum):
    FIXED = "fixed"
    DEFINITION = "definition"


@dataclass(frozen=True)
class ReducedType:
    base: str
    arguments: list[ReducedType]
    qualifiers: list[Qualifier]

    def canonical(self) -> str:
        arguments = ""

        if self.arguments:
            nested = ",".join(argument.canonical()
                              for argument in self.arguments)
            arguments = f"<{nested}>"

        qualifiers = ",".join(
            sorted(qualifier.value for qualifier in self.qualifiers))
        return f"{self.base}{arguments}[{qualifiers}]"


@dataclass(frozen=True)
class QualifiedName:
    parent_scopes: list[str]
    name: str
    parameters: list[ReducedType]
    qualifiers: list[Qualifier]
    return_type: ReducedType | None

    def path(self) -> str:
        return "::".join([*self.parent_scopes, self.name])

    def signature(self) -> str:
        parameters = ",".join(parameter.canonical()
                              for parameter in self.parameters)
        qualifiers = ",".join(
            sorted(qualifier.value for qualifier in self.qualifiers))
        return f"{self.path()}({parameters})[{qualifiers}]"

    def flattened_signature(self) -> str:
        return_type = (self.return_type.canonical()
                       if self.return_type is not None else "void")
        arguments = ",".join(parameter.canonical()
                             for parameter in self.parameters)

        if arguments:
            return f"_ArgsSignature<{return_type},{arguments}>"

        return f"_ArgsSignature<{return_type}>"


@dataclass(frozen=True)
class HeaderEntry:
    kind: HeaderEntryKind
    qualified_name: QualifiedName
    line: int


@dataclass(frozen=True)
class ParseScope:
    names: list[str]
    type_depth: int


@dataclass(frozen=True)
class SourceContext:
    scopes: tuple[str, ...]
    macro_conditions: tuple[str, ...]


@dataclass(frozen=True)
class SourceBlock:
    kind: SourceBlockKind
    start_byte: int
    end_byte: int
    start_line: int
    end_line: int
    context: SourceContext
    qualified_name: QualifiedName | None
    content: bytes


@dataclass(frozen=True)
class SortMismatch:
    line: int
    current_name: str
    expected_name: str


@dataclass(frozen=True)
class SortResult:
    content: bytes
    mismatches: list[SortMismatch]

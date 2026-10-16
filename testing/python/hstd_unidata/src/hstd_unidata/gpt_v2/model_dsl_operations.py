#!/usr/bin/env python
from __future__ import annotations

import enum
from abc import ABC, abstractmethod
from dataclasses import dataclass

from beartype import beartype

__all__ = [
    "BindingMode",
    "BuiltinOperation",
    "Operation",
    "RuleOperation",
    "standard_operations",
]


class BindingMode(enum.StrEnum):
    NONE = "none"
    COLLECTION = "collection"
    MATCH = "match"
    CASE = "case"


@beartype
class Operation(ABC):
    @property
    @abstractmethod
    def name(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def behavior(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def minimum(self) -> int:
        raise NotImplementedError

    @property
    @abstractmethod
    def maximum(self) -> int | None:
        raise NotImplementedError

    @property
    @abstractmethod
    def binding(self) -> BindingMode:
        raise NotImplementedError

    @abstractmethod
    def rules(self) -> str:
        raise NotImplementedError

    def validate_arity(self, count: int, location: str) -> None:
        if count < self.minimum:
            raise ValueError(
                f"{location}: operation {self.name!r} requires at least "
                f"{self.minimum} arguments, got {count}"
            )
        if self.maximum is not None and self.maximum < count:
            raise ValueError(
                f"{location}: operation {self.name!r} accepts at most "
                f"{self.maximum} arguments, got {count}"
            )


@beartype
class BuiltinOperation(Operation):
    def __init__(
        self,
        name: str,
        behavior: str,
        minimum: int,
        maximum: int | None,
        binding: BindingMode = BindingMode.NONE,
    ) -> None:
        self.operation_name = name
        self.operation_behavior = behavior
        self.minimum_arity = minimum
        self.maximum_arity = maximum
        self.binding_mode = binding

    @property
    def name(self) -> str:
        return self.operation_name

    @property
    def behavior(self) -> str:
        return self.operation_behavior

    @property
    def minimum(self) -> int:
        return self.minimum_arity

    @property
    def maximum(self) -> int | None:
        return self.maximum_arity

    @property
    def binding(self) -> BindingMode:
        return self.binding_mode

    def rules(self) -> str:
        return ""


@beartype
class RuleOperation(BuiltinOperation):
    def __init__(
        self,
        name: str,
        minimum: int,
        maximum: int | None,
        program: str,
        binding: BindingMode = BindingMode.NONE,
    ) -> None:
        super().__init__(
            name=name,
            behavior="custom",
            minimum=minimum,
            maximum=maximum,
            binding=binding,
        )
        self.program = program

    def rules(self) -> str:
        return self.program


@beartype
@dataclass(frozen=True)
class OperationSpec:
    name: str
    behavior: str
    minimum: int
    maximum: int | None
    binding: BindingMode = BindingMode.NONE


@beartype
def standard_operations() -> list[Operation]:
    specs = [
        OperationSpec("this", "this", 0, 0),
        OperationSpec("iter", "reference", 0, 0),
        OperationSpec("int", "int", 0, 0),
        OperationSpec("float", "float", 0, 0),
        OperationSpec("string", "string", 0, 0),
        OperationSpec("bool", "bool", 0, 0),
        OperationSpec("none", "none", 0, 0),
        OperationSpec("type", "type", 0, 0),
        OperationSpec("typeof", "typeof", 1, 1),
        OperationSpec("field", "field", 1, 1),
        OperationSpec("eq", "equality", 2, 2),
        OperationSpec("ne", "equality", 2, 2),
        OperationSpec("lt", "ordering", 2, 2),
        OperationSpec("le", "ordering", 2, 2),
        OperationSpec("and", "logical", 1, None),
        OperationSpec("or", "logical", 1, None),
        OperationSpec("not", "not", 1, 1),
        OperationSpec("str_size", "str_size", 1, 1),
        OperationSpec("str_regex", "str_regex", 1, 1),
        OperationSpec("str_format", "str_format", 1, None),
        OperationSpec("list_size", "list_size", 1, 1),
        OperationSpec("map_size", "map_size", 1, 1),
        OperationSpec("map_contains", "map_contains", 2, 2),
        OperationSpec("map_get", "map_get", 2, 2),
        OperationSpec("map_keys", "map_keys", 1, 1),
        OperationSpec("map_values", "map_values", 1, 1),
        OperationSpec("opt_has", "opt_has", 1, 1),
        OperationSpec("opt_value", "opt_value", 1, 1),
        OperationSpec("case_is", "case_is", 1, 1),
        OperationSpec("case_kind", "case_kind", 1, 1),
        OperationSpec("enum_is", "enum_is", 1, 1),
        OperationSpec("nth", "nth", 1, 1),
        OperationSpec("derived", "derived", 1, 1),
        OperationSpec("tuple", "tuple", 0, None),
        OperationSpec("walk", "walk", 1, 1),
        OperationSpec("path", "path", 1, 1),
        OperationSpec("value", "value", 1, 1),
        OperationSpec("slice", "slice", 3, 3),
        OperationSpec("all", "quantifier", 2, 2, BindingMode.COLLECTION),
        OperationSpec("exists", "quantifier", 2, 2, BindingMode.COLLECTION),
        OperationSpec("exists_one", "quantifier", 2, 2, BindingMode.COLLECTION),
        OperationSpec("map", "map", 2, 2, BindingMode.COLLECTION),
        OperationSpec("filter", "filter", 2, 2, BindingMode.COLLECTION),
        OperationSpec("filter_map", "filter_map", 2, 2, BindingMode.COLLECTION),
        OperationSpec("takewhile", "filter", 2, 2, BindingMode.COLLECTION),
        OperationSpec("group_by", "group_by", 2, 2, BindingMode.COLLECTION),
        OperationSpec("type_match", "type_match", 2, None, BindingMode.MATCH),
        OperationSpec("type_case", "type_case", 1, 1, BindingMode.CASE),
    ]
    return [
        BuiltinOperation(
            name=spec.name,
            behavior=spec.behavior,
            minimum=spec.minimum,
            maximum=spec.maximum,
            binding=spec.binding,
        )
        for spec in specs
    ]

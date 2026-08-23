from __future__ import annotations

import argparse
import json
import re
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

import tree_sitter_cpp
from beartype import beartype
from beartype.typing import Iterable, Iterator, Sequence
from loguru import logger
from tree_sitter import Language, Node, Parser, Tree

from header_source_reorder_v2.models import HeaderEntryKind, ParseScope, QualifiedName, Qualifier, ReducedType

COMMENT_NODE_TYPES = {
    "comment",
}

NAME_NODE_TYPES = {
    "identifier",
    "field_identifier",
    "operator_name",
    "destructor_name",
    "type_identifier",
}

PREPROCESSOR_NODE_TYPES = {
    "preproc_call",
    "preproc_def",
    "preproc_function_def",
    "preproc_if",
    "preproc_ifdef",
    "preproc_include",
}

TYPE_NODE_TYPES = {
    "class_specifier": HeaderEntryKind.CLASS,
    "struct_specifier": HeaderEntryKind.STRUCT,
    "union_specifier": HeaderEntryKind.UNION,
}

QUALIFIER_TOKENS = {
    "const": Qualifier.CONST,
    "volatile": Qualifier.VOLATILE,
    "*": Qualifier.PTR,
    "&": Qualifier.LVALUE_REF,
    "&&": Qualifier.RVALUE_REF,
    "noexcept": Qualifier.NOEXCEPT,
}


@beartype
def simplify_template_arguments(value: str) -> str:
    result: list[str] = []
    index = 0

    while index < len(value):
        if value[index] != "<":
            result.append(value[index])
            index += 1
            continue

        prefix = "".join(result)
        identifier_match = re.search(r"([A-Za-z_]\w*)$", prefix)

        if identifier_match is None or identifier_match.group(1) == "operator":
            result.append(value[index])
            index += 1
            continue

        depth = 1
        index += 1

        while index < len(value) and depth:
            match value[index]:
                case "<":
                    depth += 1
                case ">":
                    depth -= 1

            index += 1

        if depth:
            raise ValueError(
                f"Unbalanced template argument list in qualified name {value!r}"
            )

    return "".join(result)


@beartype
def node_text(source: bytes, node: Node) -> str:
    return source[node.start_byte:node.end_byte].decode("utf-8")


@beartype
def compact_text(value: str) -> str:
    return re.sub(r"\s+", "", value)


@beartype
def split_qualified_name(value: str) -> list[str]:
    value = compact_text(value).removeprefix("::")
    return [part for part in value.split("::") if part]


@beartype
def resolve_qualified_parts(
    scope_names: Sequence[str],
    raw_name: str,
) -> list[str]:
    raw_parts = [
        simplify_template_arguments(part)
        for part in split_qualified_name(raw_name)
    ]
    scope_parts = [simplify_template_arguments(part) for part in scope_names]
    overlap = 0

    for count in range(1, min(len(scope_parts), len(raw_parts)) + 1):
        if scope_parts[len(scope_parts) - count:] == raw_parts[:count]:
            overlap = count

    return [*scope_parts, *raw_parts[overlap:]]


@beartype
def iter_leaf_nodes(node: Node) -> Iterator[Node]:
    if not node.children:
        yield node
        return

    for nested in node.children:
        yield from iter_leaf_nodes(nested)


@beartype
def token_values(
        source: bytes,
        node: Node,
        excluded_ranges: Sequence[tuple[int, int]] = (),
) -> list[str]:
    result: list[str] = []

    for leaf in iter_leaf_nodes(node):
        if leaf.type in COMMENT_NODE_TYPES:
            continue

        excluded = any(start <= leaf.start_byte and leaf.end_byte <= end
                       for start, end in excluded_ranges)

        if not excluded:
            result.append(node_text(source, leaf))

    return result


@beartype
def canonical_tokens(tokens: Iterable[str]) -> str:
    return " ".join(token.strip() for token in tokens if token.strip())


@beartype
def direct_field_nodes(node: Node, field_name: str) -> list[Node]:
    result: list[Node] = []

    for index, nested in enumerate(node.children):
        if node.field_name_for_child(index) == field_name:
            result.append(nested)

    return result


@beartype
def find_declared_name_node(node: Node | None) -> Node | None:
    if node is None:
        return None

    if node.type in NAME_NODE_TYPES:
        return node

    if node.type in {
            "qualified_identifier",
            "scoped_identifier",
            "template_function",
            "template_method",
    }:
        return node

    declarator = node.child_by_field_name("declarator")

    if declarator is not None:
        return find_declared_name_node(declarator)

    name = node.child_by_field_name("name")

    if name is not None:
        return find_declared_name_node(name)

    for nested in node.named_children:
        result = find_declared_name_node(nested)

        if result is not None:
            return result

    return None


@beartype
def find_function_declarator(node: Node | None) -> Node | None:
    if node is None:
        return None

    if node.type == "function_declarator":
        return node

    declarator = node.child_by_field_name("declarator")

    if declarator is not None:
        result = find_function_declarator(declarator)

        if result is not None:
            return result

    for nested in node.named_children:
        if nested.type == "function_declarator":
            return nested

    return None


@beartype
def callable_declarators(node: Node) -> list[Node]:
    result: list[Node] = []

    for declarator in direct_field_nodes(node, "declarator"):
        function_declarator = find_function_declarator(declarator)

        if function_declarator is not None:
            result.append(function_declarator)

    return result


@beartype
def split_template_arguments(value: str) -> tuple[str, list[str]]:
    start = value.find("<")

    if start < 0 or not value.endswith(">"):
        return value, []

    base = value[:start]
    content = value[start + 1:-1]
    result: list[str] = []
    current: list[str] = []
    depth = 0

    for character in content:
        match character:
            case "<":
                depth += 1
                current.append(character)
            case ">":
                depth -= 1
                current.append(character)
            case "," if depth == 0:
                result.append("".join(current).strip())
                current = []
            case _:
                current.append(character)

    if depth:
        raise ValueError(
            f"Unbalanced template argument list in type {value!r}")

    if current:
        result.append("".join(current).strip())

    return base, result


@beartype
def reduced_type_from_canonical(value: str) -> ReducedType:
    tokens = value.split()
    qualifiers: list[Qualifier] = []
    base_tokens: list[str] = []

    for token in tokens:
        qualifier = QUALIFIER_TOKENS.get(token)

        if qualifier is None:
            base_tokens.append(token)
        else:
            qualifiers.append(qualifier)

    base_value = " ".join(base_tokens)
    base, argument_values = split_template_arguments(base_value)
    arguments = [
        reduced_type_from_canonical(argument) for argument in argument_values
    ]
    return ReducedType(
        base=base,
        arguments=arguments,
        qualifiers=qualifiers,
    )


@beartype
def parameter_type(source: bytes, parameter: Node) -> ReducedType:
    excluded_ranges: list[tuple[int, int]] = []
    declarator = parameter.child_by_field_name("declarator")
    name = find_declared_name_node(declarator)
    default_value = parameter.child_by_field_name("default_value")

    if name is not None:
        excluded_ranges.append((name.start_byte, name.end_byte))

    if default_value is not None:
        excluded_ranges.append(
            (default_value.start_byte, default_value.end_byte))

    tokens = token_values(source, parameter, excluded_ranges)

    if "=" in tokens:
        tokens = tokens[:tokens.index("=")]

    return reduced_type_from_canonical(canonical_tokens(tokens))


@beartype
def function_qualifiers(
    source: bytes,
    function_declarator: Node,
    parameters: Node,
) -> list[Qualifier]:
    qualifiers: list[Qualifier] = []

    for leaf in iter_leaf_nodes(function_declarator):
        if leaf.start_byte < parameters.end_byte:
            continue

        value = node_text(source, leaf)
        qualifier = QUALIFIER_TOKENS.get(value)

        if qualifier is not None:
            qualifiers.append(qualifier)

    return qualifiers


@beartype
def return_type(
    source: bytes,
    declaration: Node,
    function_declarator: Node,
) -> ReducedType | None:
    type_node = declaration.child_by_field_name("type")

    if type_node is None:
        return None

    declarator = function_declarator.child_by_field_name("declarator")
    excluded_ranges: list[tuple[int, int]] = []

    if declarator is not None:
        excluded_ranges.append(
            (declarator.start_byte, function_declarator.end_byte))

    tokens = token_values(source, declaration, excluded_ranges)
    type_tokens = token_values(source, type_node)
    canonical = canonical_tokens(type_tokens)

    if not canonical:
        canonical = canonical_tokens(tokens)

    return reduced_type_from_canonical(canonical)


@beartype
def parse_qualified_name(
    source: bytes,
    declaration: Node,
    function_declarator: Node,
    scope: ParseScope,
) -> QualifiedName:
    declarator = function_declarator.child_by_field_name("declarator")
    name_node = find_declared_name_node(declarator)

    if name_node is None:
        raise ValueError(f"Could not determine callable name at line "
                         f"{function_declarator.start_point.row + 1}")

    parts = resolve_qualified_parts(
        scope.names,
        node_text(source, name_node),
    )

    if not parts:
        raise ValueError(
            f"Callable at line {function_declarator.start_point.row + 1} "
            f"has an empty qualified name")

    parameters_node = function_declarator.child_by_field_name("parameters")

    if parameters_node is None:
        raise ValueError(
            f"Callable {'::'.join(parts)!r} at line "
            f"{function_declarator.start_point.row + 1} has no parameter list")

    parameters = [
        parameter_type(source, parameter)
        for parameter in parameters_node.named_children if parameter.type in {
            "parameter_declaration",
            "optional_parameter_declaration",
            "variadic_parameter",
        }
    ]

    return QualifiedName(
        parent_scopes=parts[:-1],
        name=parts[-1],
        parameters=parameters,
        qualifiers=function_qualifiers(
            source,
            function_declarator,
            parameters_node,
        ),
        return_type=return_type(
            source,
            declaration,
            function_declarator,
        ),
    )


@beartype
def parse_namespace_names(source: bytes, node: Node) -> list[str]:
    name = node.child_by_field_name("name")

    if name is None:
        return [f"(anonymous@{node.start_point.row + 1})"]

    return split_qualified_name(node_text(source, name))


def format_tree(node: Node, depth: int = 0) -> str:
    indent = "  " * depth
    start = node.start_point
    end = node.end_point
    location = (f"[{start.row + 1}:{start.column + 1}"
                f"-{end.row + 1}:{end.column + 1}]")

    if not node.is_named:
        return f"{indent}{json.dumps(node.type)} {location}"

    label = node.type
    if node.is_missing:
        label = f"MISSING {label}"

    if node.child_count == 0:
        return f"{indent}{label} {location}"

    lines = [f"{indent}{label} {location}"]

    for index, child in enumerate(node.children):
        formatted_child = format_tree(child, depth + 1)
        field_name = node.field_name_for_child(index)

        if field_name:
            child_indent = "  " * (depth + 1)
            formatted_child = (f"{child_indent}{field_name}:"
                               f"{formatted_child[len(child_indent):]}")

        lines.append(formatted_child)
    return "\n".join(lines)


@beartype
def make_scope(scope: ParseScope, names: Sequence[str],
               type_delta: int) -> ParseScope:
    return ParseScope(
        names=[*scope.names, *names],
        type_depth=scope.type_depth + type_delta,
    )

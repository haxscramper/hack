#!/usr/bin/env python

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
def create_parser() -> Parser:
    language = Language(tree_sitter_cpp.language())
    return Parser(language)


@beartype
def parse_file(parser: Parser, path: Path) -> tuple[bytes, Tree]:
    source = path.read_bytes()
    return source, parser.parse(source)


@beartype
def make_scope(scope: ParseScope, names: Sequence[str],
               type_delta: int) -> ParseScope:
    return ParseScope(
        names=[*scope.names, *names],
        type_depth=scope.type_depth + type_delta,
    )


@beartype
def header_entries(source: bytes, tree: Tree) -> list[HeaderEntry]:
    entries: list[HeaderEntry] = []
    seen: set[tuple[HeaderEntryKind, str]] = set()

    def append_entry(entry: HeaderEntry) -> None:
        key = (entry.kind, entry.qualified_name.signature())

        if key not in seen:
            seen.add(key)
            entries.append(entry)

    def visit(node: Node, scope: ParseScope) -> None:
        match node.type:
            case "namespace_definition":
                names = parse_namespace_names(source, node)
                resolved = resolve_qualified_parts(scope.names,
                                                   "::".join(names))
                qualified_name = QualifiedName(
                    parent_scopes=resolved[:-1],
                    name=resolved[-1],
                    parameters=[],
                    qualifiers=[],
                    return_type=None,
                )
                append_entry(
                    HeaderEntry(
                        kind=HeaderEntryKind.NAMESPACE,
                        qualified_name=qualified_name,
                        line=node.start_point.row + 1,
                    ))
                body = node.child_by_field_name("body")

                if body is not None:
                    visit(body, make_scope(scope, names, 0))

                return

            case "class_specifier" | "struct_specifier" | "union_specifier":
                body = node.child_by_field_name("body")

                if body is None:
                    return

                name = node.child_by_field_name("name")

                if name is None:
                    type_name = f"(anonymous-type@{node.start_point.row + 1})"
                else:
                    type_name = simplify_template_arguments(
                        compact_text(node_text(source, name)))

                qualified_name = QualifiedName(
                    parent_scopes=list(scope.names),
                    name=type_name,
                    parameters=[],
                    qualifiers=[],
                    return_type=None,
                )
                append_entry(
                    HeaderEntry(
                        kind=TYPE_NODE_TYPES[node.type],
                        qualified_name=qualified_name,
                        line=node.start_point.row + 1,
                    ))
                visit(body, make_scope(scope, [type_name], 1))
                return

            case "function_definition":
                if node.has_error:
                    return

                declarator = node.child_by_field_name("declarator")
                function_declarator = find_function_declarator(declarator)

                if function_declarator is None:
                    return

                qualified_name = parse_qualified_name(
                    source,
                    node,
                    function_declarator,
                    scope,
                )
                kind = (HeaderEntryKind.METHOD
                        if scope.type_depth else HeaderEntryKind.FUNCTION)
                append_entry(
                    HeaderEntry(
                        kind=kind,
                        qualified_name=qualified_name,
                        line=node.start_point.row + 1,
                    ))
                return

            case "declaration" | "field_declaration":
                if not node.has_error:
                    for function_declarator in callable_declarators(node):
                        if function_declarator.has_error:
                            continue

                        qualified_name = parse_qualified_name(
                            source,
                            node,
                            function_declarator,
                            scope,
                        )
                        kind = (HeaderEntryKind.METHOD if scope.type_depth else
                                HeaderEntryKind.FUNCTION)
                        append_entry(
                            HeaderEntry(
                                kind=kind,
                                qualified_name=qualified_name,
                                line=node.start_point.row + 1,
                            ))

                for nested in node.named_children:
                    if nested.type in TYPE_NODE_TYPES:
                        visit(nested, scope)

                return

            case "template_declaration":
                for nested in node.named_children:
                    if nested.type != "template_parameter_list":
                        visit(nested, scope)

                return

            case _:
                for nested in node.named_children:
                    visit(nested, scope)

    visit(tree.root_node, ParseScope(names=[], type_depth=0))
    return entries


@beartype
def preprocessor_contexts(source: bytes) -> list[tuple[str, ...]]:
    lines = source.decode("utf-8").splitlines(keepends=True)
    contexts: list[tuple[str, ...]] = []
    stack: list[str] = []
    directive_pattern = re.compile(
        r"^\s*#\s*(if|ifdef|ifndef|elif|else|endif)\b(.*)$")

    for line_number, line in enumerate(lines, start=1):
        contexts.append(tuple(stack))
        match_result = directive_pattern.match(line)

        if match_result is None:
            continue

        directive = match_result.group(1)
        expression = " ".join(match_result.group(2).strip().split())

        match directive:
            case "if":
                stack.append(f"if {expression}")
            case "ifdef":
                stack.append(f"ifdef {expression}")
            case "ifndef":
                stack.append(f"ifndef {expression}")
            case "elif":
                if not stack:
                    raise ValueError(
                        f"Unexpected #elif at source line {line_number}")

                stack[-1] = f"elif {expression}"
            case "else":
                if not stack:
                    raise ValueError(
                        f"Unexpected #else at source line {line_number}")

                stack[-1] = "else"
            case "endif":
                if not stack:
                    raise ValueError(
                        f"Unexpected #endif at source line {line_number}")

                stack.pop()

    if stack:
        conditions = ", ".join(stack)
        raise ValueError(
            f"Unclosed preprocessor conditions at end of source: {conditions}")

    contexts.append(tuple())
    return contexts


@beartype
def source_definition_blocks(source: bytes, tree: Tree) -> list[SourceBlock]:
    macro_contexts = preprocessor_contexts(source)
    definitions: list[SourceBlock] = []

    def visit(node: Node, scope: ParseScope) -> None:
        match node.type:
            case "namespace_definition":
                names = parse_namespace_names(source, node)
                body = node.child_by_field_name("body")

                if body is not None:
                    visit(body, make_scope(scope, names, 0))

                return

            case "class_specifier" | "struct_specifier" | "union_specifier":
                body = node.child_by_field_name("body")

                if body is None:
                    return

                name = node.child_by_field_name("name")

                if name is None:
                    type_name = f"(anonymous-type@{node.start_point.row + 1})"
                else:
                    type_name = simplify_template_arguments(
                        compact_text(node_text(source, name)))

                visit(body, make_scope(scope, [type_name], 1))
                return

            case "function_definition":
                if node.has_error:
                    return

                declarator = node.child_by_field_name("declarator")
                function_declarator = find_function_declarator(declarator)

                if (function_declarator is None
                        or function_declarator.has_error):
                    return

                qualified_name = parse_qualified_name(
                    source,
                    node,
                    function_declarator,
                    scope,
                )
                start_line = node.start_point.row + 1
                end_line = node.end_point.row + 1
                definitions.append(
                    SourceBlock(
                        kind=SourceBlockKind.DEFINITION,
                        start_byte=node.start_byte,
                        end_byte=node.end_byte,
                        start_line=start_line,
                        end_line=end_line,
                        context=SourceContext(
                            scopes=tuple(scope.names),
                            macro_conditions=macro_contexts[start_line - 1],
                        ),
                        qualified_name=qualified_name,
                        content=source[node.start_byte:node.end_byte],
                    ))
                return

            case _:
                for nested in node.named_children:
                    visit(nested, scope)

    visit(tree.root_node, ParseScope(names=[], type_depth=0))
    return sorted(definitions, key=lambda block: block.start_byte)


@beartype
def flat_source_blocks(
        source: bytes,
        definitions: Sequence[SourceBlock]) -> list[SourceBlock]:
    result: list[SourceBlock] = []
    offset = 0

    for definition in definitions:
        if offset < definition.start_byte:
            prefix = source[offset:definition.start_byte]
            start_line = source[:offset].count(b"\n") + 1
            end_line = start_line + prefix.count(b"\n")
            result.append(
                SourceBlock(
                    kind=SourceBlockKind.FIXED,
                    start_byte=offset,
                    end_byte=definition.start_byte,
                    start_line=start_line,
                    end_line=end_line,
                    context=definition.context,
                    qualified_name=None,
                    content=prefix,
                ))

        result.append(definition)
        offset = definition.end_byte

    if offset < len(source):
        suffix = source[offset:]
        start_line = source[:offset].count(b"\n") + 1
        end_line = start_line + suffix.count(b"\n")
        context = (definitions[-1].context if definitions else SourceContext(
            scopes=(), macro_conditions=()))
        result.append(
            SourceBlock(
                kind=SourceBlockKind.FIXED,
                start_byte=offset,
                end_byte=len(source),
                start_line=start_line,
                end_line=end_line,
                context=context,
                qualified_name=None,
                content=suffix,
            ))

    return result


@beartype
def header_ranks(entries: Sequence[HeaderEntry]) -> dict[str, int]:
    result: dict[str, int] = {}

    for entry in entries:
        if entry.kind not in {
                HeaderEntryKind.METHOD,
                HeaderEntryKind.FUNCTION,
        }:
            continue

        signature = entry.qualified_name.signature()

        if signature not in result:
            result[signature] = len(result)

    return result


@beartype
def sort_source(
    source: bytes,
    definitions: Sequence[SourceBlock],
    ranks: dict[str, int],
) -> SortResult:
    grouped_indexes: dict[SourceContext, list[int]] = {}

    for index, block in enumerate(definitions):
        if block.qualified_name is None:
            continue

        signature = block.qualified_name.signature()

        if signature not in ranks:
            continue

        grouped_indexes.setdefault(block.context, []).append(index)

    replacement_content: dict[int, bytes] = {}
    mismatches: list[SortMismatch] = []

    for indexes in grouped_indexes.values():
        ordered_indexes = sorted(
            indexes,
            key=lambda index: ranks[definitions[index].qualified_name.
                                    signature()],
        )
        ordered_blocks = [definitions[index] for index in ordered_indexes]

        for target_index, expected_block in zip(indexes, ordered_blocks):
            current_block = definitions[target_index]
            current_name = current_block.qualified_name
            expected_name = expected_block.qualified_name

            if current_name is None or expected_name is None:
                raise ValueError(
                    f"Definition block at line {current_block.start_line} "
                    f"does not have a qualified name")

            replacement_content[target_index] = expected_block.content

            if current_name.signature() != expected_name.signature():
                mismatches.append(
                    SortMismatch(
                        line=current_block.start_line,
                        current_name=current_name.signature(),
                        expected_name=expected_name.signature(),
                    ))

    result = bytearray()
    offset = 0

    for index, block in enumerate(definitions):
        result.extend(source[offset:block.start_byte])
        result.extend(replacement_content.get(index, block.content))
        offset = block.end_byte

    result.extend(source[offset:])
    return SortResult(content=bytes(result), mismatches=mismatches)


@beartype
def write_diagnostics(
    output_directory: Path,
    header_tree: Tree,
    source_tree: Tree,
    entries: Sequence[HeaderEntry],
    definitions: Sequence[SourceBlock],
) -> None:
    output_directory.mkdir(parents=True, exist_ok=True)

    (output_directory / "header-tree.txt").write_text(
        f"{format_tree(header_tree.root_node)}\n",
        encoding="utf-8",
    )
    (output_directory / "source-tree.txt").write_text(
        f"{format_tree(source_tree.root_node)}\n",
        encoding="utf-8",
    )

    header_lines = [
        f"{entry.line}: {entry.kind.value}: "
        f"{entry.qualified_name.signature()}" for entry in entries
    ]
    (output_directory / "header-qualified-names.txt").write_text(
        "\n".join(header_lines) + ("\n" if header_lines else ""),
        encoding="utf-8",
    )

    header_by_signature = {
        entry.qualified_name.signature(): entry
        for entry in entries
    }
    source_lines: list[str] = []

    for block in definitions:
        qualified_name = block.qualified_name

        if qualified_name is None:
            continue

        signature = qualified_name.signature()
        header_entry = header_by_signature.get(signature)
        location = f"{block.start_line}-{block.end_line}"

        if header_entry is None:
            match = "header match: not found"
        else:
            match = (f"header match: {header_entry.kind.value} "
                     f"at line {header_entry.line}")

        source_lines.append(f"{location}: {signature}: {match}")

    (output_directory / "source-blocks.txt").write_text(
        "\n".join(source_lines) + ("\n" if source_lines else ""),
        encoding="utf-8",
    )


@beartype
def write_diagnostics(
    output_directory: Path,
    header_tree: Tree,
    source_tree: Tree,
    entries: Sequence[HeaderEntry],
    definitions: Sequence[SourceBlock],
) -> None:
    output_directory.mkdir(parents=True, exist_ok=True)

    (output_directory / "header-tree.txt").write_text(
        f"{format_tree(header_tree.root_node)}\n",
        encoding="utf-8",
    )
    (output_directory / "source-tree.txt").write_text(
        f"{format_tree(source_tree.root_node)}\n",
        encoding="utf-8",
    )

    header_lines = [
        f"{entry.line}: {entry.kind.value}: "
        f"{entry.qualified_name.signature()}" for entry in entries
    ]
    (output_directory / "header-qualified-names.txt").write_text(
        "\n".join(header_lines) + ("\n" if header_lines else ""),
        encoding="utf-8",
    )

    header_by_signature = {
        entry.qualified_name.signature(): entry
        for entry in entries
    }
    source_lines: list[str] = []

    for block in definitions:
        qualified_name = block.qualified_name

        if qualified_name is None:
            continue

        signature = qualified_name.signature()
        header_entry = header_by_signature.get(signature)
        location = f"{block.start_line}-{block.end_line}"

        if header_entry is None:
            match = "header match: not found"
        else:
            match = (f"header match: {header_entry.kind.value} "
                     f"at line {header_entry.line}")

        source_lines.append(f"{location}: {signature}: {match}")

    (output_directory / "source-blocks.txt").write_text(
        "\n".join(source_lines) + ("\n" if source_lines else ""),
        encoding="utf-8",
    )


@beartype
def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=(
        "Sort C++ source definitions according to declaration order "
        "in the corresponding header"))
    parser.add_argument("header", type=Path)
    parser.add_argument("source", type=Path)
    parser.add_argument(
        "--rewrite",
        action="store_true",
        help="Overwrite the source file with the sorted result",
    )
    parser.add_argument(
        "--diagnostics-dir",
        type=Path,
        help="Write parser and qualified-name diagnostics to this directory",
    )

    return parser.parse_args()


@beartype
def main() -> int:
    arguments = parse_arguments()
    parser = create_parser()
    header_source, header_tree = parse_file(parser, arguments.header)
    source, source_tree = parse_file(parser, arguments.source)

    entries = header_entries(header_source, header_tree)
    ranks = header_ranks(entries)
    definitions = source_definition_blocks(source, source_tree)

    if arguments.diagnostics_dir is not None:
        write_diagnostics(
            arguments.diagnostics_dir,
            header_tree,
            source_tree,
            entries,
            definitions,
        )

    flat_source_blocks(source, definitions)
    result = sort_source(source, definitions, ranks)

    if result.content == source:
        logger.info(
            f"{arguments.source} is already in header declaration order")
        return 0

    for mismatch in result.mismatches:
        logger.error(
            f"{arguments.source}:{mismatch.line}: "
            f"{mismatch.current_name} should be {mismatch.expected_name}")

    if arguments.rewrite:
        arguments.source.write_bytes(result.content)
        logger.info(f"Rewrote {arguments.source} with "
                    f"{len(result.mismatches)} reordered definitions")
        return 0

    logger.error(f"{arguments.source} contains "
                 f"{len(result.mismatches)} out-of-order definitions")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())

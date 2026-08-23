from __future__ import annotations

import re
from beartype import beartype
from tree_sitter import Node, Tree

from header_source_reorder_v2.common_parse import compact_text, find_function_declarator, make_scope, node_text, parse_namespace_names, parse_qualified_name, simplify_template_arguments
from header_source_reorder_v2.models import ParseScope, SourceBlock, SourceBlockKind, SourceContext


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

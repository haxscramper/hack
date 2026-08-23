from __future__ import annotations

from beartype import beartype
from tree_sitter import Node, Tree

from header_source_reorder_v2.common_parse import TYPE_NODE_TYPES, callable_declarators, compact_text, find_function_declarator, make_scope, node_text, parse_namespace_names, parse_qualified_name, resolve_qualified_parts, simplify_template_arguments
from header_source_reorder_v2.models import HeaderEntry, HeaderEntryKind, ParseScope, QualifiedName


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

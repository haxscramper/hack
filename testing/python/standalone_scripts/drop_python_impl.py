import argparse
import ast
import tokenize
from pathlib import Path

import tree_sitter_python
from beartype import beartype
from tree_sitter import Language, Node, Parser

IMPORT_TYPES = frozenset(
    {
        "import_statement",
        "import_from_statement",
        "future_import_statement",
    }
)


@beartype
def strip_bodies(source: bytes, filename: str) -> bytes:
    parser = Parser(Language(tree_sitter_python.language()))
    root = parser.parse(source).root_node

    if root.has_error:
        pending = [root]
        while pending:
            node = pending.pop()
            if node.is_error or node.is_missing:
                row, column = node.start_point
                raise SyntaxError(
                    f"{filename}:{row + 1}:{column + 1}: "
                    f"tree-sitter found invalid or incomplete Python "
                    f"near {source[node.start_byte : node.end_byte][:80]!r}"
                )
            pending.extend(reversed(node.named_children))

    replacements: list[tuple[int, int, bytes]] = []
    deletions: list[tuple[int, int]] = []

    @beartype
    def remove_import(node: Node) -> None:
        start = node.start_byte
        end = node.end_byte
        following = node.next_sibling
        preceding = node.prev_sibling

        if following is not None and following.type == ";":
            end = following.end_byte
            while end < len(source) and source[end : end + 1] in (b" ", b"\t"):
                end += 1
        elif preceding is not None and preceding.type == ";":
            start = preceding.start_byte
        else:
            line_start = source.rfind(b"\n", 0, start) + 1
            line_end = source.find(b"\n", end)
            if line_end == -1:
                line_end = len(source)

            if (
                not source[line_start:start].strip()
                and not source[end:line_end].strip()
            ):
                start = line_start
                end = min(line_end + 1, len(source))

        deletions.append((start, end))

    @beartype
    def visit(node: Node) -> None:
        match node.type:
            case "function_definition":
                body = node.child_by_field_name("body")
                if body is None:
                    raise ValueError(
                        f"{filename}:{node.start_point.row + 1}: "
                        "function_definition has no body field"
                    )

                statements = [
                    nested for nested in body.named_children if nested.type != "comment"
                ]
                replacement = b"pass"

                if statements and statements[0].type == "expression_statement":
                    statement = statements[0]
                    text = source[statement.start_byte : statement.end_byte]
                    statement_ast = ast.parse(
                        text.decode("utf-8"),
                        filename=filename,
                        mode="exec",
                    ).body[0]

                    if (
                        isinstance(statement_ast, ast.Expr)
                        and isinstance(statement_ast.value, ast.Constant)
                        and isinstance(statement_ast.value.value, str)
                    ):
                        replacement = text

                replacements.append(
                    (
                        body.start_byte,
                        body.end_byte,
                        replacement,
                    )
                )

            case "block" | "module":
                statements = [
                    nested for nested in node.named_children if nested.type != "comment"
                ]
                needs_pass = (
                    node.type == "block"
                    and bool(statements)
                    and all(statement.type in IMPORT_TYPES for statement in statements)
                )

                for nested in node.named_children:
                    if nested.type in IMPORT_TYPES:
                        if needs_pass:
                            replacements.append(
                                (
                                    nested.start_byte,
                                    nested.end_byte,
                                    b"pass",
                                )
                            )
                            needs_pass = False
                        else:
                            remove_import(nested)
                    else:
                        visit(nested)

            case _:
                for nested in node.named_children:
                    visit(nested)

    visit(root)

    merged: list[tuple[int, int]] = []
    for start, end in sorted(deletions):
        if merged and start <= merged[-1][1]:
            previous_start, previous_end = merged[-1]
            merged[-1] = (previous_start, max(previous_end, end))
        else:
            merged.append((start, end))

    replacements.extend((start, end, b"") for start, end in merged)

    fragments: list[bytes] = []
    position = 0
    for start, end, replacement in sorted(replacements):
        fragments.append(source[position:start])
        fragments.append(replacement)
        position = end

    fragments.append(source[position:])
    return b"".join(fragments)


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(
        description="Remove Python imports and function bodies, retaining docstrings."
    )
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    arguments = parser.parse_args()

    with tokenize.open(arguments.input) as stream:
        source = stream.read().encode("utf-8")

    result = strip_bodies(source, str(arguments.input))
    arguments.output.write_bytes(result)


if __name__ == "__main__":
    main()

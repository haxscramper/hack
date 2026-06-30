#!/usr/bin/env python
"""Rewrite abbreviated Qt enum usages (e.g. Qt.DisplayRole) into their
fully-qualified form (e.g. Qt.ItemDataRole.DisplayRole) using .pyi stubs."""

import argparse
import ast
import sys


def base_name(node):
    """Return the dotted name of a class base expression."""
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return node.attr
    return ""


def is_enum_class(cls):
    for b in cls.bases:
        name = base_name(b)
        if "Enum" in name or "Flag" in name:
            return True
    return False


def collect_mapping(tree, mapping, collisions):
    """Populate `mapping[(container, member)] = enum_class` from a stub tree."""

    def walk(node, stack):
        for child in ast.iter_child_nodes(node):
            if isinstance(child, ast.ClassDef):
                if is_enum_class(child) and stack:
                    container = stack[-1]
                    enum_class = child.name
                    for stmt in child.body:
                        targets = []
                        if isinstance(stmt, ast.Assign):
                            targets = stmt.targets
                        elif isinstance(stmt, ast.AnnAssign):
                            targets = [stmt.target]
                        for tgt in targets:
                            if isinstance(tgt, ast.Name):
                                key = (container, tgt.id)
                                if key in mapping and mapping[
                                        key] != enum_class:
                                    collisions.add(key)
                                mapping[key] = enum_class
                walk(child, stack + [child.name])
            else:
                walk(child, stack)

    walk(tree, [])


def value_tail_name(node):
    """Return the trailing attribute/name of an expression, or None."""
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return node.attr
    return None


def line_offsets(source):
    offsets = [0]
    for line in source.splitlines(keepends=True):
        offsets.append(offsets[-1] + len(line))
    return offsets


def find_rewrites(tree, source, mapping, collisions):
    """Return a list of (start, end, new_text) edits for the target source."""
    offs = line_offsets(source)

    def pos(lineno, col):
        return offs[lineno - 1] + col

    edits = []

    for node in ast.walk(tree):
        if not isinstance(node, ast.Attribute):
            continue
        container = value_tail_name(node.value)
        if container is None:
            continue
        key = (container, node.attr)
        if key not in mapping or key in collisions:
            continue

        enum_class = mapping[key]
        value_src = ast.get_source_segment(source, node.value)
        if value_src is None:
            continue

        start = pos(node.value.lineno, node.value.col_offset)
        end = pos(node.end_lineno, node.end_col_offset)
        new_text = "{}.{}.{}".format(value_src, enum_class, node.attr)
        edits.append((start, end, new_text))

    return edits


def apply_edits(source, edits):
    for start, end, new_text in sorted(edits, key=lambda e: e[0],
                                       reverse=True):
        source = source[:start] + new_text + source[end:]
    return source


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("references",
                        nargs="+",
                        help="One or more .pyi stub files")
    parser.add_argument("-t",
                        "--target",
                        required=True,
                        help="Python file to rewrite")
    args = parser.parse_args()

    mapping = {}
    collisions = set()
    for ref in args.references:
        with open(ref, "r", encoding="utf-8") as f:
            collect_mapping(ast.parse(f.read(), filename=ref), mapping,
                            collisions)

    for key in sorted(collisions):
        print("warning: ambiguous member {}.{}, skipping".format(*key),
              file=sys.stderr)

    with open(args.target, "r", encoding="utf-8") as f:
        source = f.read()

    tree = ast.parse(source, filename=args.target)
    edits = find_rewrites(tree, source, mapping, collisions)
    result = apply_edits(source, edits)

    with open(args.target, "w", encoding="utf-8") as f:
        f.write(result)
    print("rewrote {} ({} replacements)".format(args.target, len(edits)),
          file=sys.stderr)


if __name__ == "__main__":
    main()

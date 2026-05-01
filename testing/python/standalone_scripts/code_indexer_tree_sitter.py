#!/usr/bin/env python
# /// script
# dependencies = [
#   "pydantic>=2",
#   "beartype>=0.18",
#   "click>=8",
#   "tree_sitter",
#   "tree_sitter_cpp",
#   "tree_sitter_python",
# ]
# ///

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path
from typing import Literal

from pydantic import BaseModel, Field
from tree_sitter import Language, Node, Parser
import tree_sitter_cpp
import tree_sitter_python


class DocInfo(BaseModel):
    brief: str | None = None
    full: str | None = None


class SourceRange(BaseModel):
    start_line: int | None = None
    end_line: int | None = None


class FieldInfo(BaseModel):
    kind: Literal["field"] = "field"
    name: str
    range: SourceRange
    doc: DocInfo | None = None


class SignalInfo(BaseModel):
    kind: Literal["signal"] = "signal"
    name: str
    range: SourceRange
    doc: DocInfo | None = None


class FunctionInfo(BaseModel):
    kind: Literal["function", "method"] = "function"
    name: str
    range: SourceRange
    doc: DocInfo | None = None


class ClassInfo(BaseModel):
    kind: Literal["class"] = "class"
    name: str
    range: SourceRange
    doc: DocInfo | None = None
    methods: list[FunctionInfo] = Field(default_factory=list)
    fields: list[FieldInfo] = Field(default_factory=list)
    signals: list[SignalInfo] = Field(default_factory=list)


class FileInfo(BaseModel):
    kind: Literal["file"] = "file"
    path: str
    range: SourceRange
    doc: DocInfo | None = None
    classes: list[ClassInfo] = Field(default_factory=list)
    functions: list[FunctionInfo] = Field(default_factory=list)


class DirectoryInfo(BaseModel):
    kind: Literal["directory"] = "directory"
    path: str
    range: SourceRange
    doc: DocInfo | None = None
    children: list["EntryInfo"] = Field(default_factory=list)


EntryInfo = DirectoryInfo | FileInfo

DirectoryInfo.model_rebuild()


class IndexRoot(BaseModel):
    root: str
    entries: list[EntryInfo]


CPP_LANGUAGE = Language(tree_sitter_cpp.language())
PYTHON_LANGUAGE = Language(tree_sitter_python.language())

CPP_SUFFIXES = {
    ".c",
    ".cc",
    ".cpp",
    ".cxx",
    ".h",
    ".hh",
    ".hpp",
    ".hxx",
}

PYTHON_SUFFIXES = {
    ".py",
}


def make_parser(language: Language) -> Parser:
    parser = Parser()
    parser.language = language
    return parser


def source_range_from_node(node: Node) -> SourceRange:
    return SourceRange(
        start_line=node.start_point[0] + 1,
        end_line=node.end_point[0] + 1,
    )


def node_text(node: Node, source: bytes) -> str:
    return source[node.start_byte:node.end_byte].decode("utf-8",
                                                        errors="replace")


def normalize_doc_lines(lines: list[str]) -> DocInfo | None:
    cleaned = [line.rstrip() for line in lines]
    while cleaned and not cleaned[0].strip():
        cleaned.pop(0)
    while cleaned and not cleaned[-1].strip():
        cleaned.pop()
    if not cleaned:
        return None

    full = "\n".join(cleaned).strip()
    if not full:
        return None

    brief = None
    for line in cleaned:
        stripped = line.strip()
        if stripped:
            brief = stripped
            break

    return DocInfo(brief=brief, full=full)


def clean_cpp_comment_text(raw: str) -> list[str]:
    lines = raw.splitlines()
    result: list[str] = []
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("//"):
            stripped = stripped[2:]
            if stripped.startswith("/"):
                stripped = stripped[1:]
            result.append(stripped.lstrip())
            continue

        stripped = stripped.removeprefix("/*")
        stripped = stripped.removesuffix("*/")
        stripped = stripped.strip()
        if stripped.startswith("*"):
            stripped = stripped[1:].lstrip()
        result.append(stripped)

    return result


def extract_leading_cpp_doc(parent: Node, target_index: int,
                            source: bytes) -> DocInfo | None:
    lines: list[str] = []
    idx = target_index - 1
    saw_comment = False

    while idx >= 0:
        node = parent.named_children[idx]
        if node.type == "comment":
            comment_lines = clean_cpp_comment_text(node_text(node, source))
            lines = comment_lines + lines
            saw_comment = True
            idx -= 1
            continue

        gap = parent.named_children[idx + 1].start_point[0] - node.end_point[0]
        if saw_comment and gap <= 1:
            idx -= 1
            continue
        break

    return normalize_doc_lines(lines)


def clean_python_docstring(raw: str) -> DocInfo | None:
    text = raw.strip()
    if text.startswith(('"""', "'''")) and text.endswith(
        ('"""', "'''")) and len(text) >= 6:
        text = text[3:-3]
    elif text.startswith(("'", '"')) and text.endswith(
        ("'", '"')) and len(text) >= 2:
        text = text[1:-1]

    lines = text.splitlines()
    if not lines:
        return None

    indent_candidates = [
        len(re.match(r"^[ \t]*", line).group(0)) for line in lines[1:]
        if line.strip()
    ]
    common_indent = min(indent_candidates) if indent_candidates else 0

    normalized: list[str] = []
    for i, line in enumerate(lines):
        if i == 0:
            normalized.append(line.strip())
        else:
            normalized.append(line[common_indent:].rstrip())

    return normalize_doc_lines(normalized)


def get_python_docstring_for_body(body: Node, source: bytes) -> DocInfo | None:
    if body.type != "block" or body.named_child_count == 0:
        return None

    first = body.named_children[0]
    if first.type != "expression_statement" or first.named_child_count != 1:
        return None

    expr = first.named_children[0]
    if expr.type not in {"string", "concatenated_string"}:
        return None

    return clean_python_docstring(node_text(expr, source))


def find_child_by_field_name(node: Node, field_name: str) -> Node | None:
    for child in node.children:
        if node.field_name_for_child(child.id) == field_name:
            return child
    return None


def cpp_extract_declarator_name(node: Node, source: bytes) -> str | None:
    if node.type == "identifier":
        return node_text(node, source)

    for child in node.children:
        result = cpp_extract_declarator_name(child, source)
        if result is not None:
            return result

    return None


def cpp_is_function_definition(node: Node) -> bool:
    return node.type == "function_definition"


def cpp_is_class_like(node: Node) -> bool:
    return node.type in {"class_specifier", "struct_specifier"}


def cpp_extract_class_name(node: Node, source: bytes) -> str | None:
    name_node = node.child_by_field_name("name")
    if name_node is not None:
        return node_text(name_node, source)
    return None


def cpp_member_is_signal(node: Node, source: bytes) -> bool:
    text = node_text(node, source)
    return "signals:" in text or "Q_SIGNAL" in text or "Q_SIGNALS" in text


def cpp_parse_function(
        node: Node, source: bytes,
        kind: Literal["function", "method"]) -> FunctionInfo | None:
    declarator = node.child_by_field_name("declarator")
    if declarator is None:
        return None

    name = cpp_extract_declarator_name(declarator, source)
    if name is None:
        return None

    return FunctionInfo(
        kind=kind,
        name=name,
        range=source_range_from_node(node),
        doc=None,
    )


def cpp_parse_field(node: Node, source: bytes) -> list[FieldInfo]:
    result: list[FieldInfo] = []
    for child in node.children:
        if child.type == "field_declaration":
            declarator = child.child_by_field_name("declarator")
            if declarator is not None:
                name = cpp_extract_declarator_name(declarator, source)
                if name is not None:
                    result.append(
                        FieldInfo(
                            name=name,
                            range=source_range_from_node(child),
                            doc=None,
                        ))
            else:
                for sub in child.children:
                    if sub.type in {"identifier", "field_identifier"}:
                        result.append(
                            FieldInfo(
                                name=node_text(sub, source),
                                range=source_range_from_node(child),
                                doc=None,
                            ))
    return result


def cpp_parse_class(node: Node, source: bytes) -> ClassInfo | None:
    name = cpp_extract_class_name(node, source)
    if name is None:
        return None

    info = ClassInfo(
        name=name,
        range=source_range_from_node(node),
        doc=None,
    )

    body = node.child_by_field_name("body")
    if body is None:
        for child in node.children:
            if child.type == "field_declaration_list":
                body = child
                break

    if body is None:
        return info

    body_named = body.named_children
    current_access = "private"
    pending_doc: DocInfo | None = None
    signal_section = False

    for idx, child in enumerate(body_named):
        if child.type == "access_specifier":
            access_text = node_text(child, source).strip().rstrip(":")
            current_access = access_text
            signal_section = access_text == "signals" or "signals" in access_text
            pending_doc = extract_leading_cpp_doc(body, idx, source)
            continue

        child_doc = extract_leading_cpp_doc(body, idx, source)

        if cpp_is_function_definition(child):
            fn = cpp_parse_function(child, source, "method")
            if fn is not None:
                fn.doc = child_doc
                if signal_section or cpp_member_is_signal(child, source):
                    info.signals.append(
                        SignalInfo(
                            name=fn.name,
                            range=fn.range,
                            doc=fn.doc,
                        ))
                else:
                    info.methods.append(fn)
            continue

        if child.type == "declaration":
            declarator = child.child_by_field_name("declarator")
            if declarator is not None:
                declarator_text = node_text(declarator, source)
                if "(" in declarator_text and ")" in declarator_text:
                    name = cpp_extract_declarator_name(declarator, source)
                    if name is not None:
                        fn = FunctionInfo(
                            kind="method",
                            name=name,
                            range=source_range_from_node(child),
                            doc=child_doc,
                        )
                        if signal_section or cpp_member_is_signal(
                                child, source):
                            info.signals.append(
                                SignalInfo(
                                    name=name,
                                    range=fn.range,
                                    doc=fn.doc,
                                ))
                        else:
                            info.methods.append(fn)
                    continue

            for field in cpp_parse_field(child, source):
                field.doc = child_doc
                info.fields.append(field)
            continue

        if child.type == "field_declaration":
            names: list[str] = []
            for sub in child.children:
                if sub.type in {"field_identifier", "identifier"}:
                    names.append(node_text(sub, source))
            for field_name in names:
                info.fields.append(
                    FieldInfo(
                        name=field_name,
                        range=source_range_from_node(child),
                        doc=child_doc,
                    ))

    return info


def index_cpp_file(path: Path, rel_path: str, parser: Parser) -> FileInfo:
    source = path.read_bytes()
    tree = parser.parse(source)
    root = tree.root_node

    file_info = FileInfo(
        path=rel_path,
        range=source_range_from_node(root),
        doc=None,
    )

    named = root.named_children
    for idx, child in enumerate(named):
        child_doc = extract_leading_cpp_doc(root, idx, source)

        if cpp_is_class_like(child):
            cls = cpp_parse_class(child, source)
            if cls is not None:
                cls.doc = child_doc
                file_info.classes.append(cls)
            continue

        if cpp_is_function_definition(child):
            fn = cpp_parse_function(child, source, "function")
            if fn is not None:
                fn.doc = child_doc
                file_info.functions.append(fn)

    return file_info


def python_function_name(node: Node, source: bytes) -> str | None:
    name_node = node.child_by_field_name("name")
    if name_node is None:
        return None
    return node_text(name_node, source)


def python_parse_class(node: Node, source: bytes) -> ClassInfo | None:
    name = python_function_name(node, source)
    if name is None:
        return None

    body = node.child_by_field_name("body")
    doc = get_python_docstring_for_body(body,
                                        source) if body is not None else None

    info = ClassInfo(
        name=name,
        range=source_range_from_node(node),
        doc=doc,
    )

    if body is None or body.type != "block":
        return info

    for child in body.named_children:
        if child.type == "function_definition":
            method_name = python_function_name(child, source)
            if method_name is None:
                continue
            method_body = child.child_by_field_name("body")
            method_doc = get_python_docstring_for_body(
                method_body, source) if method_body is not None else None
            info.methods.append(
                FunctionInfo(
                    kind="method",
                    name=method_name,
                    range=source_range_from_node(child),
                    doc=method_doc,
                ))
            continue

        if child.type == "expression_statement":
            continue

        if child.type == "assignment":
            left = child.child_by_field_name("left")
            if left is not None and left.type == "identifier":
                info.fields.append(
                    FieldInfo(
                        name=node_text(left, source),
                        range=source_range_from_node(child),
                        doc=None,
                    ))
            elif left is not None and left.type == "pattern_list":
                for target in left.named_children:
                    if target.type == "identifier":
                        info.fields.append(
                            FieldInfo(
                                name=node_text(target, source),
                                range=source_range_from_node(child),
                                doc=None,
                            ))

    return info


def index_python_file(path: Path, rel_path: str, parser: Parser) -> FileInfo:
    source = path.read_bytes()
    tree = parser.parse(source)
    root = tree.root_node

    file_info = FileInfo(
        path=rel_path,
        range=source_range_from_node(root),
        doc=get_python_docstring_for_body(root, source)
        if root.type == "module" else None,
    )

    for child in root.named_children:
        if child.type == "function_definition":
            name = python_function_name(child, source)
            if name is None:
                continue
            body = child.child_by_field_name("body")
            file_info.functions.append(
                FunctionInfo(
                    kind="function",
                    name=name,
                    range=source_range_from_node(child),
                    doc=get_python_docstring_for_body(body, source)
                    if body is not None else None,
                ))
            continue

        if child.type == "class_definition":
            cls = python_parse_class(child, source)
            if cls is not None:
                file_info.classes.append(cls)

    return file_info


def index_file(path: Path, root: Path, cpp_parser: Parser,
               python_parser: Parser) -> FileInfo | None:
    rel_path = str(path.relative_to(root))
    suffix = path.suffix.lower()

    if suffix in CPP_SUFFIXES:
        return index_cpp_file(path, rel_path, cpp_parser)

    if suffix in PYTHON_SUFFIXES:
        return index_python_file(path, rel_path, python_parser)

    return None


def directory_range(path: Path) -> SourceRange:
    return SourceRange(start_line=None, end_line=None)


def build_directory_tree(path: Path, root: Path, cpp_parser: Parser,
                         python_parser: Parser) -> list[EntryInfo]:
    entries: list[EntryInfo] = []

    for child in sorted(path.iterdir(), key=lambda p:
                        (not p.is_dir(), p.name)):
        if child.name == ".git":
            continue

        if child.is_dir():
            dir_info = DirectoryInfo(
                path=str(child.relative_to(root)),
                range=directory_range(child),
                doc=None,
                children=build_directory_tree(child, root, cpp_parser,
                                              python_parser),
            )
            if dir_info.children:
                entries.append(dir_info)
            continue

        file_info = index_file(child, root, cpp_parser, python_parser)
        if file_info is not None:
            entries.append(file_info)

    return entries


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("root", type=Path)
    parser.add_argument("-o", "--output", type=Path, required=True)
    args = parser.parse_args()

    root = args.root.resolve()
    cpp_parser = make_parser(CPP_LANGUAGE)
    python_parser = make_parser(PYTHON_LANGUAGE)

    index = IndexRoot(
        root=str(root),
        entries=build_directory_tree(root, root, cpp_parser, python_parser),
    )

    args.output.write_text(
        index.model_dump_json(indent=2),
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()

#!/usr/bin/env python
# /// script
# dependencies = [
#   "sqlalchemy>=2",
#   "tree_sitter",
#   "tree_sitter_cpp",
#   "tree_sitter_python",
# ]
# ///

from __future__ import annotations

import argparse
import logging
import subprocess
import time
from dataclasses import dataclass, field
from pathlib import Path

from sqlalchemy import ForeignKey, create_engine, text
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column
from tree_sitter import Language, Node, Parser
import tree_sitter_cpp
import tree_sitter_python
import re

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


class Base(DeclarativeBase):
    pass


class IndexEntry(Base):
    __tablename__ = "entries"

    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    parent_id: Mapped[int | None] = mapped_column(ForeignKey("entries.id"),
                                                  nullable=True)

    kind: Mapped[str] = mapped_column(nullable=False)
    language: Mapped[str | None] = mapped_column(nullable=True)

    name: Mapped[str | None] = mapped_column(nullable=True)
    path: Mapped[str | None] = mapped_column(nullable=True)
    qualified_name: Mapped[str | None] = mapped_column(nullable=True)

    start_line: Mapped[int | None] = mapped_column(nullable=True)
    end_line: Mapped[int | None] = mapped_column(nullable=True)

    type_text: Mapped[str | None] = mapped_column(nullable=True)
    signature_text: Mapped[str | None] = mapped_column(nullable=True)
    signature_source: Mapped[str | None] = mapped_column(nullable=True)

    doc_brief: Mapped[str | None] = mapped_column(nullable=True)
    doc_full: Mapped[str | None] = mapped_column(nullable=True)


class FunctionArgument(Base):
    __tablename__ = "function_arguments"

    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    entry_id: Mapped[int] = mapped_column(ForeignKey("entries.id"),
                                          nullable=False)
    position: Mapped[int] = mapped_column(nullable=False)

    name: Mapped[str | None] = mapped_column(nullable=True)
    type_text: Mapped[str | None] = mapped_column(nullable=True)
    default_value: Mapped[str | None] = mapped_column(nullable=True)
    kind: Mapped[str | None] = mapped_column(nullable=True)


@dataclass
class DocInfo:
    brief: str | None = None
    full: str | None = None


@dataclass
class ParsedArgument:
    name: str | None
    type_text: str | None = None
    default_value: str | None = None
    kind: str | None = None


@dataclass
class ParsedEntry:
    kind: str
    path: str
    name: str | None = None
    language: str | None = None
    qualified_name: str | None = None
    start_line: int | None = None
    end_line: int | None = None
    doc: DocInfo | None = None
    type_text: str | None = None
    signature_text: str | None = None
    signature_source: str | None = None
    arguments: list[ParsedArgument] = field(default_factory=list)
    children: list["ParsedEntry"] = field(default_factory=list)


def make_parser(language: Language) -> Parser:
    parser = Parser()
    parser.language = language
    return parser


def node_text(node: Node, source: bytes, normalize: bool = True) -> str:
    text = source[node.start_byte:node.end_byte].decode("utf-8",
                                                        errors="replace")
    if normalize:
        text = re.sub(r"\r\n?|\n", "", text)
        text = re.sub(r"[ \t]*\n[ \t]*", "\n", text)
        text = re.sub(r" {2,}", " ", text)
        assert "\n" not in text
    return text


def source_lines(node: Node) -> tuple[int | None, int | None]:
    return node.start_point[0] + 1, node.end_point[0] + 1


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
        if line.strip():
            brief = line.strip()
            brief = brief.replace("\\brief", "")
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
            lines = clean_cpp_comment_text(node_text(node, source)) + lines
            saw_comment = True
            idx -= 1
            continue

        if saw_comment:
            gap = parent.named_children[idx +
                                        1].start_point[0] - node.end_point[0]
            if gap <= 1:
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

    indent_candidates: list[int] = []
    for line in lines[1:]:
        if line.strip():
            indent_candidates.append(len(line) - len(line.lstrip(" \t")))
    common_indent = min(indent_candidates) if indent_candidates else 0

    normalized: list[str] = []
    for i, line in enumerate(lines):
        if i == 0:
            normalized.append(line.strip())
        else:
            normalized.append(line[common_indent:].rstrip())

    return normalize_doc_lines(normalized)


def get_python_docstring_for_statement_list(node: Node,
                                            source: bytes) -> DocInfo | None:
    if node.named_child_count == 0:
        return None

    first = node.named_children[0]
    if first.type != "expression_statement" or first.named_child_count != 1:
        return None

    expr = first.named_children[0]
    if expr.type not in {"string", "concatenated_string"}:
        return None

    return clean_python_docstring(node_text(expr, source))


def find_first_descendant(node: Node, target_types: set[str]) -> Node | None:
    stack = [node]
    while stack:
        cur = stack.pop()
        if cur.type in target_types:
            return cur
        for child in reversed(cur.children):
            stack.append(child)
    return None


def cpp_extract_declarator_name(node: Node, source: bytes) -> str | None:
    if node.type in {"identifier", "field_identifier", "type_identifier"}:
        return node_text(node, source)

    for child in node.children:
        result = cpp_extract_declarator_name(child, source)
        if result is not None:
            return result
    return None


def cpp_parse_arguments_from_declarator(
        declarator: Node,
        source: bytes) -> tuple[list[ParsedArgument], str | None]:
    parameter_list = find_first_descendant(declarator, {"parameter_list"})
    if parameter_list is None:
        return [], None

    args: list[ParsedArgument] = []
    for child in parameter_list.named_children:
        if child.type not in {
                "parameter_declaration", "optional_parameter_declaration"
        }:
            continue

        type_node = child.child_by_field_name("type")
        declarator_node = child.child_by_field_name("declarator")
        default_node = child.child_by_field_name("default_value")

        arg_name = cpp_extract_declarator_name(
            declarator_node, source) if declarator_node is not None else None
        arg_type = node_text(type_node,
                             source).strip() if type_node is not None else None
        arg_default = node_text(
            default_node, source).strip() if default_node is not None else None

        args.append(
            ParsedArgument(
                name=arg_name,
                type_text=arg_type,
                default_value=arg_default,
                kind=None,
            ))

    declarator_text = node_text(declarator, source)
    params_text = node_text(parameter_list, source)
    suffix = params_text
    idx = declarator_text.find(params_text)
    if idx != -1:
        trailing = declarator_text[idx + len(params_text):].strip()
        if trailing:
            suffix = f"{params_text} {trailing}"

    return args, suffix


def cpp_parse_function_entry(
    node: Node,
    source: bytes,
    kind: str,
    qualified_prefix: list[str],
    doc: DocInfo | None,
    rel_path: str,
) -> ParsedEntry | None:
    declarator = node.child_by_field_name("declarator")
    if declarator is None:
        return None

    name = cpp_extract_declarator_name(declarator, source)
    if name is None:
        return None

    type_node = node.child_by_field_name("type")
    return_type = node_text(type_node,
                            source).strip() if type_node is not None else None
    args, signature_suffix = cpp_parse_arguments_from_declarator(
        declarator, source)
    start_line, end_line = source_lines(node)
    qn = "::".join(qualified_prefix + [name])

    return ParsedEntry(
        kind=kind,
        name=name,
        language="cpp",
        qualified_name=qn,
        start_line=start_line,
        end_line=end_line,
        doc=doc,
        type_text=return_type,
        signature_text=signature_suffix,
        signature_source=node_text(declarator, source).strip(),
        arguments=args,
        path=rel_path,
    )


def cpp_collect_field_names(node: Node, source: bytes) -> list[str]:
    names: list[str] = []
    stack = [node]
    while stack:
        cur = stack.pop()
        if cur.type in {"field_identifier", "identifier"}:
            names.append(node_text(cur, source))
        for child in reversed(cur.children):
            stack.append(child)
    unique = list(dict.fromkeys(names))
    return unique


def cpp_parse_class(
    node: Node,
    source: bytes,
    qualified_prefix: list[str],
    doc: DocInfo | None,
    rel_path: str,
) -> ParsedEntry | None:
    name_node = node.child_by_field_name("name")
    if name_node is None:
        return None

    class_name = node_text(name_node, source).strip()
    start_line, end_line = source_lines(node)
    qn = "::".join(qualified_prefix + [class_name])

    entry = ParsedEntry(
        kind="class",
        name=class_name,
        language="cpp",
        qualified_name=qn,
        start_line=start_line,
        end_line=end_line,
        doc=doc,
        path=rel_path,
    )

    body = node.child_by_field_name("body")
    if body is None:
        return entry

    signal_section = False
    for idx, child in enumerate(body.named_children):
        child_doc = extract_leading_cpp_doc(body, idx, source)

        if child.type == "access_specifier":
            access_text = node_text(child, source).strip().rstrip(":")
            signal_section = access_text == "signals" or "signals" in access_text
            continue

        if child.type == "function_definition":
            method = cpp_parse_function_entry(
                child,
                source,
                "signal" if signal_section else "method",
                qualified_prefix + [class_name],
                child_doc,
                rel_path=rel_path,
            )
            if method is not None:
                entry.children.append(method)
            continue

        if child.type == "declaration":
            declarator = child.child_by_field_name("declarator")
            if declarator is not None:
                has_params = find_first_descendant(
                    declarator, {"parameter_list"}) is not None
                if has_params:
                    method = cpp_parse_function_entry(
                        child,
                        source,
                        "signal" if signal_section else "method",
                        qualified_prefix + [class_name],
                        child_doc,
                        rel_path=rel_path,
                    )
                    if method is not None:
                        entry.children.append(method)
                    continue

            type_node = child.child_by_field_name("type")
            field_type = node_text(
                type_node, source).strip() if type_node is not None else None
            field_names = cpp_collect_field_names(child, source)
            for fname in field_names:
                fstart, fend = source_lines(child)
                entry.children.append(
                    ParsedEntry(
                        kind="field",
                        name=fname,
                        language="cpp",
                        qualified_name="::".join(qualified_prefix +
                                                 [class_name, fname]),
                        start_line=fstart,
                        end_line=fend,
                        doc=child_doc,
                        type_text=field_type,
                        path=rel_path,
                    ))
            continue

        if child.type == "field_declaration":
            type_node = child.child_by_field_name("type")
            field_type = node_text(
                type_node, source).strip() if type_node is not None else None
            field_names = cpp_collect_field_names(child, source)
            for fname in field_names:
                fstart, fend = source_lines(child)
                entry.children.append(
                    ParsedEntry(
                        kind="field",
                        name=fname,
                        language="cpp",
                        qualified_name="::".join(qualified_prefix +
                                                 [class_name, fname]),
                        start_line=fstart,
                        end_line=fend,
                        doc=child_doc,
                        type_text=field_type,
                        path=rel_path,
                    ))

    return entry


def cpp_parse_scope(
    node: Node,
    source: bytes,
    out: list[ParsedEntry],
    qualified_prefix: list[str],
    rel_path: str,
) -> None:
    for idx, child in enumerate(node.named_children):
        child_doc = extract_leading_cpp_doc(node, idx, source)

        if child.type == "namespace_definition":
            name_node = child.child_by_field_name("name")
            if name_node is None:
                continue
            ns_name = node_text(name_node, source).strip()
            ns_qn = "::".join(qualified_prefix + [ns_name])
            ns_start, ns_end = source_lines(child)

            ns_entry = ParsedEntry(
                kind="namespace",
                name=ns_name,
                language="cpp",
                qualified_name=ns_qn,
                start_line=ns_start,
                end_line=ns_end,
                doc=child_doc,
                path=rel_path,
            )

            body = child.child_by_field_name("body")
            if body is not None:
                cpp_parse_scope(
                    body,
                    source,
                    ns_entry.children,
                    qualified_prefix + [ns_name],
                    rel_path=rel_path,
                )

            out.append(ns_entry)
            continue

        if child.type in {"class_specifier", "struct_specifier"}:
            cls = cpp_parse_class(
                child,
                source,
                qualified_prefix,
                child_doc,
                rel_path=rel_path,
            )
            if cls is not None:
                out.append(cls)
            continue

        if child.type == "function_definition":
            fn = cpp_parse_function_entry(
                child,
                source,
                "function",
                qualified_prefix,
                child_doc,
                rel_path=rel_path,
            )
            if fn is not None:
                out.append(fn)
            continue

        if child.type == "declaration":
            declarator = child.child_by_field_name("declarator")
            if declarator is None:
                continue
            has_params = find_first_descendant(declarator,
                                               {"parameter_list"}) is not None
            if not has_params:
                continue
            fn = cpp_parse_function_entry(
                child,
                source,
                "function",
                qualified_prefix,
                child_doc,
                rel_path=rel_path,
            )
            if fn is not None:
                out.append(fn)


def python_extract_function_name(node: Node, source: bytes) -> str | None:
    name_node = node.child_by_field_name("name")
    if name_node is None:
        return None
    return node_text(name_node, source)


def python_parse_arguments(params_node: Node | None,
                           source: bytes) -> list[ParsedArgument]:
    if params_node is None:
        return []

    args: list[ParsedArgument] = []
    pos = 0
    for child in params_node.named_children:
        arg = ParsedArgument(name=None)

        if child.type == "identifier":
            arg.name = node_text(child, source)
            arg.kind = "positional"
        elif child.type == "typed_parameter":
            n = child.child_by_field_name("name")
            t = child.child_by_field_name("type")
            arg.name = node_text(n, source) if n is not None else None
            arg.type_text = node_text(t, source) if t is not None else None
            arg.kind = "positional"
        elif child.type == "default_parameter":
            n = child.child_by_field_name("name")
            v = child.child_by_field_name("value")
            arg.name = node_text(n, source) if n is not None else None
            arg.default_value = node_text(v, source) if v is not None else None
            arg.kind = "positional"
        elif child.type == "typed_default_parameter":
            n = child.child_by_field_name("name")
            t = child.child_by_field_name("type")
            v = child.child_by_field_name("value")
            arg.name = node_text(n, source) if n is not None else None
            arg.type_text = node_text(t, source) if t is not None else None
            arg.default_value = node_text(v, source) if v is not None else None
            arg.kind = "positional"
        elif child.type == "list_splat_pattern":
            arg.name = node_text(child, source).lstrip("*")
            arg.kind = "vararg"
        elif child.type == "dictionary_splat_pattern":
            arg.name = node_text(child, source).lstrip("*")
            arg.kind = "kwvararg"
        else:
            continue

        if arg.name is not None:
            args.append(arg)
            pos += 1

    return args


def python_parse_function(
    node: Node,
    source: bytes,
    kind: str,
    qualified_prefix: list[str],
    rel_path: str,
) -> ParsedEntry | None:
    name = python_extract_function_name(node, source)
    if name is None:
        return None

    body = node.child_by_field_name("body")
    params = node.child_by_field_name("parameters")
    return_type = node.child_by_field_name("return_type")
    doc = get_python_docstring_for_statement_list(
        body, source) if body is not None else None
    args = python_parse_arguments(params, source)

    params_text = node_text(params, source) if params is not None else "()"
    return_text = f" -> {node_text(return_type, source)}" if return_type is not None else ""
    signature = f"{params_text}{return_text}"

    qn = ".".join(qualified_prefix + [name])
    start_line, end_line = source_lines(node)

    return ParsedEntry(
        kind=kind,
        name=name,
        language="python",
        qualified_name=qn,
        start_line=start_line,
        end_line=end_line,
        doc=doc,
        type_text=node_text(return_type, source)
        if return_type is not None else None,
        signature_text=signature,
        signature_source=f"{name}{signature}",
        arguments=args,
        path=rel_path,
    )


def python_parse_class(
    node: Node,
    source: bytes,
    qualified_prefix: list[str],
    rel_path: str,
) -> ParsedEntry | None:
    name = python_extract_function_name(node, source)
    if name is None:
        return None

    body = node.child_by_field_name("body")
    doc = get_python_docstring_for_statement_list(
        body, source) if body is not None else None
    start_line, end_line = source_lines(node)
    qn = ".".join(qualified_prefix + [name])

    cls = ParsedEntry(
        kind="class",
        name=name,
        language="python",
        qualified_name=qn,
        start_line=start_line,
        end_line=end_line,
        doc=doc,
        path=rel_path,
    )

    if body is None:
        return cls

    for child in body.named_children:
        if child.type in {"function_definition", "async_function_definition"}:
            method = python_parse_function(
                child,
                source,
                "method",
                qualified_prefix + [name],
                rel_path=rel_path,
            )
            if method is not None:
                cls.children.append(method)
            continue

        if child.type in {"assignment", "typed_assignment"}:
            left = child.child_by_field_name("left")
            tnode = child.child_by_field_name("type")
            type_text = node_text(tnode, source) if tnode is not None else None
            if left is None:
                continue

            targets: list[str] = []
            if left.type == "identifier":
                targets.append(node_text(left, source))
            elif left.type in {"pattern_list", "tuple_pattern"}:
                for sub in left.named_children:
                    if sub.type == "identifier":
                        targets.append(node_text(sub, source))

            fstart, fend = source_lines(child)
            for target in targets:
                cls.children.append(
                    ParsedEntry(
                        kind="field",
                        name=target,
                        language="python",
                        qualified_name=".".join(qualified_prefix +
                                                [name, target]),
                        start_line=fstart,
                        end_line=fend,
                        doc=None,
                        type_text=type_text,
                        path=rel_path,
                    ))

    return cls


def index_cpp_file(path: Path, rel_path: str, parser: Parser) -> ParsedEntry:
    source = path.read_bytes()
    tree = parser.parse(source)
    root = tree.root_node
    start_line, end_line = source_lines(root)

    file_entry = ParsedEntry(
        kind="file",
        name=path.name,
        language=None,
        path=rel_path,
        qualified_name=rel_path,
        start_line=start_line,
        end_line=end_line,
    )

    cpp_parse_scope(root, source, file_entry.children, [], rel_path=rel_path)
    return file_entry


def index_python_file(path: Path, rel_path: str,
                      parser: Parser) -> ParsedEntry:
    source = path.read_bytes()
    tree = parser.parse(source)
    root = tree.root_node
    start_line, end_line = source_lines(root)

    file_entry = ParsedEntry(
        kind="file",
        name=path.name,
        language=None,
        path=rel_path,
        qualified_name=rel_path,
        start_line=start_line,
        end_line=end_line,
        doc=get_python_docstring_for_statement_list(root, source)
        if root.type == "module" else None,
    )

    for child in root.named_children:
        if child.type in {"function_definition", "async_function_definition"}:
            fn = python_parse_function(child,
                                       source,
                                       "function", [],
                                       rel_path=rel_path)
            if fn is not None:
                file_entry.children.append(fn)
            continue

        if child.type == "class_definition":
            cls = python_parse_class(child, source, [], rel_path=rel_path)
            if cls is not None:
                file_entry.children.append(cls)

    return file_entry


def index_file(path: Path, rel_path: str, cpp_parser: Parser,
               python_parser: Parser) -> ParsedEntry:
    suffix = path.suffix.lower()
    if suffix in CPP_SUFFIXES:
        return index_cpp_file(path, rel_path, cpp_parser)
    else:
        return index_python_file(path, rel_path, python_parser)


def gather_git_files(root: Path) -> list[Path]:
    output = subprocess.check_output(
        ["git", "-C",
         str(root), "ls-files", "-co", "--exclude-standard"],
        text=True,
    )
    result: list[Path] = []
    for line in output.splitlines():
        rel = line.strip()
        if not rel:
            continue
        suffix = Path(rel).suffix.lower()
        if suffix in CPP_SUFFIXES or suffix in PYTHON_SUFFIXES:
            result.append(Path(rel))
    result.sort()
    return result


def format_eta(seconds: float) -> str:
    total = int(max(0, seconds))
    h, rem = divmod(total, 3600)
    m, s = divmod(rem, 60)
    return f"{h:02d}:{m:02d}:{s:02d}"


def persist_entry(session: Session, parent_id: int | None,
                  entry: ParsedEntry) -> int:
    row = IndexEntry(
        parent_id=parent_id,
        kind=entry.kind,
        language=entry.language,
        name=entry.name,
        path=entry.path,
        qualified_name=entry.qualified_name,
        start_line=entry.start_line,
        end_line=entry.end_line,
        type_text=entry.type_text,
        signature_text=entry.signature_text,
        signature_source=entry.signature_source,
        doc_brief=entry.doc.brief if entry.doc else None,
        doc_full=entry.doc.full if entry.doc else None,
    )
    session.add(row)
    session.flush()

    for idx, arg in enumerate(entry.arguments):
        session.add(
            FunctionArgument(
                entry_id=row.id,
                position=idx,
                name=arg.name,
                type_text=arg.type_text,
                default_value=arg.default_value,
                kind=arg.kind,
            ))

    for child in entry.children:
        persist_entry(session, row.id, child)

    return row.id


def create_flat_view(session: Session) -> None:
    session.execute(text("DROP VIEW IF EXISTS entry_flat_view"))
    session.execute(
        text("""
            CREATE VIEW entry_flat_view AS
            SELECT
                e.id AS entry_id,
                e.kind AS kind,
                e.language AS language,
                e.path AS path,
                e.qualified_name AS qualified_name,
                CASE
                    WHEN e.kind IN ('function', 'method', 'signal') THEN
                        COALESCE(e.qualified_name, e.name, '') || COALESCE(e.signature_text, '')
                    WHEN e.kind = 'field' THEN
                        COALESCE(e.qualified_name, e.name, ''))
                    WHEN e.kind = 'class' THEN
                        COALESCE(e.qualified_name, e.name, '')
                    WHEN e.kind = 'namespace' THEN
                        COALESCE(e.qualified_name, e.name, '')
                    WHEN e.kind = 'file' THEN
                        COALESCE(e.path, e.name, '')
                    WHEN e.kind = 'directory' THEN
                        COALESCE(e.path, e.name, '')
                    ELSE
                        COALESCE(e.qualified_name, e.path, e.name, '')
                END AS flat_representation,
                e.type_text AS type_text,
                e.signature_text AS signature_text,
                e.signature_source AS signature_source,
                e.doc_brief AS doc_brief,
                e.doc_full AS doc_full,
                e.start_line AS start_line
            FROM entries e
            """))


def main() -> None:
    logging.basicConfig(level=logging.INFO,
                        format="%(asctime)s %(levelname)s %(message)s")

    argp = argparse.ArgumentParser()
    argp.add_argument("root", type=Path)
    argp.add_argument("--output", type=Path, required=False)
    args = argp.parse_args()

    root = args.root.resolve()
    output = args.output.resolve(
    ) if args.output is not None else root / ".haxscramper-code-index.sqlite"
    output.unlink(missing_ok=True)

    files = gather_git_files(root)
    total = len(files)

    cpp_parser = make_parser(CPP_LANGUAGE)
    python_parser = make_parser(PYTHON_LANGUAGE)

    engine = create_engine(f"sqlite+pysqlite:///{output}")
    Base.metadata.create_all(engine)

    with Session(engine) as session:
        root_dir = ParsedEntry(
            kind="directory",
            name=root.name,
            path=".",
            qualified_name=".",
            start_line=None,
            end_line=None,
        )
        root_id = persist_entry(session, None, root_dir)

        dir_ids: dict[Path, int] = {Path("."): root_id}
        dirs_to_create: set[Path] = set()

        for rel in files:
            parent = rel.parent
            while str(parent) != ".":
                dirs_to_create.add(parent)
                parent = parent.parent

        for d in sorted(dirs_to_create, key=lambda p: (len(p.parts), str(p))):
            parent = d.parent if str(d.parent) != "" else Path(".")
            parent_id = dir_ids[parent]
            dir_entry = ParsedEntry(
                kind="directory",
                name=d.name,
                path=str(d),
                qualified_name=str(d),
                start_line=None,
                end_line=None,
            )
            dir_ids[d] = persist_entry(session, parent_id, dir_entry)

        start = time.monotonic()
        processed = 0

        for rel in files:
            abs_path = root / rel
            parsed_file = index_file(abs_path, str(rel), cpp_parser,
                                     python_parser)
            parent_id = dir_ids.get(rel.parent, root_id)
            persist_entry(session, parent_id, parsed_file)

            processed += 1
            remaining = total - processed
            elapsed = time.monotonic() - start
            avg = elapsed / processed
            eta = avg * remaining
            logging.info("[%d/%d] ETA %s %s", processed, total,
                         format_eta(eta), rel)

        create_flat_view(session)
        session.commit()


if __name__ == "__main__":
    main()

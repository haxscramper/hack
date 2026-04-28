#!/usr/bin/env python
# /// script
# dependencies = [
#   "pydantic>=2",
#   "beartype>=0.18",
#   "click>=8",
# ]
# ///

from __future__ import annotations

import ast
import json
from pathlib import Path
from typing import Literal

import click
from beartype import beartype
from pydantic import BaseModel, Field


# ----------------------------
# Models
# ----------------------------

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


# ----------------------------
# Helpers
# ----------------------------

@beartype
def split_docstring(doc: str | None) -> DocInfo | None:
    if not doc:
        return None

    lines = doc.strip().splitlines()
    if not lines:
        return None

    brief_lines: list[str] = []
    rest_lines: list[str] = []
    in_rest = False

    for line in lines:
        if not in_rest:
            if line.strip() == "":
                in_rest = True
            else:
                brief_lines.append(line)
        else:
            rest_lines.append(line)

    brief = "\n".join(brief_lines).strip() or None
    full = doc.strip() or None

    return DocInfo(brief=brief, full=full)


@beartype
def node_range(node: ast.AST) -> SourceRange:
    return SourceRange(
        start_line=getattr(node, "lineno", None),
        end_line=getattr(node, "end_lineno", getattr(node, "lineno", None)),
    )


@beartype
def is_signal_call(expr: ast.AST) -> bool:
    if not isinstance(expr, ast.Call):
        return False

    func = expr.func
    if isinstance(func, ast.Name):
        return func.id in {"Signal", "pyqtSignal"}
    if isinstance(func, ast.Attribute):
        return func.attr in {"Signal", "pyqtSignal"}
    return False


@beartype
def get_target_names(target: ast.AST) -> list[str]:
    if isinstance(target, ast.Name):
        return [target.id]
    if isinstance(target, (ast.Tuple, ast.List)):
        result: list[str] = []
        for elt in target.elts:
            result.extend(get_target_names(elt))
        return result
    return []


@beartype
def extract_doc_from_body_following_assign(body: list[ast.stmt], index: int) -> DocInfo | None:
    next_index = index + 1
    if next_index >= len(body):
        return None

    next_stmt = body[next_index]
    if (
        isinstance(next_stmt, ast.Expr)
        and isinstance(next_stmt.value, ast.Constant)
        and isinstance(next_stmt.value.value, str)
    ):
        return split_docstring(next_stmt.value.value)
    return None


# ----------------------------
# AST analyzers
# ----------------------------

@beartype
def analyze_function(node: ast.FunctionDef | ast.AsyncFunctionDef, kind: Literal["function", "method"]) -> FunctionInfo:
    return FunctionInfo(
        kind=kind,
        name=node.name,
        range=node_range(node),
        doc=split_docstring(ast.get_docstring(node, clean=False)),
    )


@beartype
def analyze_class(node: ast.ClassDef) -> ClassInfo:
    class_info = ClassInfo(
        name=node.name,
        range=node_range(node),
        doc=split_docstring(ast.get_docstring(node, clean=False)),
    )

    for idx, stmt in enumerate(node.body):
        if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
            class_info.methods.append(analyze_function(stmt, "method"))

        elif isinstance(stmt, ast.Assign):
            for target in stmt.targets:
                for name in get_target_names(target):
                    doc = extract_doc_from_body_following_assign(node.body, idx)
                    if is_signal_call(stmt.value):
                        class_info.signals.append(
                            SignalInfo(
                                name=name,
                                range=node_range(stmt),
                                doc=doc,
                            )
                        )
                    else:
                        class_info.fields.append(
                            FieldInfo(
                                name=name,
                                range=node_range(stmt),
                                doc=doc,
                            )
                        )

        elif isinstance(stmt, ast.AnnAssign):
            names = get_target_names(stmt.target)
            doc = extract_doc_from_body_following_assign(node.body, idx)
            for name in names:
                if stmt.value is not None and is_signal_call(stmt.value):
                    class_info.signals.append(
                        SignalInfo(
                            name=name,
                            range=node_range(stmt),
                            doc=doc,
                        )
                    )
                else:
                    class_info.fields.append(
                        FieldInfo(
                            name=name,
                            range=node_range(stmt),
                            doc=doc,
                        )
                    )

    return class_info


@beartype
def analyze_file(file_path: Path, root: Path) -> FileInfo:
    rel_path = file_path.relative_to(root).as_posix()
    source = file_path.read_text(encoding="utf-8")
    tree = ast.parse(source, filename=str(file_path))

    file_info = FileInfo(
        path=rel_path,
        range=SourceRange(start_line=1, end_line=len(source.splitlines()) or 1),
        doc=split_docstring(ast.get_docstring(tree, clean=False)),
    )

    for stmt in tree.body:
        if isinstance(stmt, ast.ClassDef):
            file_info.classes.append(analyze_class(stmt))
        elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
            file_info.functions.append(analyze_function(stmt, "function"))

    return file_info


@beartype
def find_directory_doc(dir_path: Path) -> DocInfo | None:
    init_py = dir_path / "__init__.py"
    if not init_py.is_file():
        return None

    try:
        source = init_py.read_text(encoding="utf-8")
        tree = ast.parse(source, filename=str(init_py))
        return split_docstring(ast.get_docstring(tree, clean=False))
    except Exception:
        return None


@beartype
def directory_line_range(dir_path: Path) -> SourceRange:
    init_py = dir_path / "__init__.py"
    if init_py.is_file():
        try:
            source = init_py.read_text(encoding="utf-8")
            line_count = len(source.splitlines()) or 1
            return SourceRange(start_line=1, end_line=line_count)
        except Exception:
            pass
    return SourceRange(start_line=None, end_line=None)


@beartype
def analyze_directory(dir_path: Path, root: Path) -> DirectoryInfo:
    rel_path = "." if dir_path == root else dir_path.relative_to(root).as_posix()

    directory_info = DirectoryInfo(
        path=rel_path,
        range=directory_line_range(dir_path),
        doc=find_directory_doc(dir_path),
    )

    children: list[EntryInfo] = []

    for child in sorted(dir_path.iterdir(), key=lambda p: (p.is_file(), p.name.lower())):
        if child.name.startswith("."):
            continue

        if child.is_dir():
            children.append(analyze_directory(child, root))
        elif child.is_file() and child.suffix == ".py":
            try:
                children.append(analyze_file(child, root))
            except SyntaxError:
                # Skip syntactically invalid Python files
                continue
            except UnicodeDecodeError:
                continue

    directory_info.children = children
    return directory_info


@beartype
def build_index(root: Path) -> IndexRoot:
    root = root.resolve()
    return IndexRoot(
        root=root.as_posix(),
        entries=[analyze_directory(root, root)],
    )


# ----------------------------
# CLI
# ----------------------------

@click.command()
@click.argument("input_dir", type=click.Path(exists=True, file_okay=False, path_type=Path))
@click.argument("output_file", type=click.Path(dir_okay=False, path_type=Path))
def main(input_dir: Path, output_file: Path) -> None:
    """
    Parse INPUT_DIR recursively and write a JSON code index to OUTPUT_FILE.
    """
    index = build_index(input_dir)
    output_file.write_text(
        json.dumps(index.model_dump(mode="json"), indent=2, ensure_ascii=False),
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()

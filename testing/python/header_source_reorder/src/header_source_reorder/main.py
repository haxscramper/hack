#!/usr/bin/env python
import argparse

import logging

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)
log = logging.getLogger(__name__)

logging.getLogger("graphviz").setLevel(logging.ERROR)

import html

import re
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from pydantic import BaseModel, ConfigDict, Field, field_serializer

import graphviz
import igraph
import pytest
from beartype import beartype
from beartype.typing import Dict, List, Optional, Sequence, Set, Tuple
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import CppLexer
from tree_sitter import Language, Node, Parser
import tree_sitter_cpp

CPP_HEADERS: Set[str] = {
    ".h",
    ".hh",
    ".hpp",
    ".hxx",
    ".h++",
    ".ipp",
    ".tpp",
    ".inl",
    ".inc",
}
CPP_SOURCES: Set[str] = {
    ".c",
    ".cc",
    ".cpp",
    ".cxx",
    ".c++",
    ".cu",
}
CPP_ALL: Set[str] = set(CPP_HEADERS) | set(CPP_SOURCES)
CAPTURE_TYPES: Set[str] = {
    "function_definition",
    "declaration",
    "template_declaration",
    "type_definition",
    "using_declaration",
    "alias_declaration",
    "concept_definition",
    "linkage_specification",
}


class CodeBlock(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    id: str = Field(description="Stable unique block identifier.")
    path: Path = Field(description="Path of file containing block.")
    key: str = Field(description="Semantic symbol key used for matching.")
    line_range: slice = Field(description="0-based half-open line range.")
    text: str = Field(description="Exact block source text.")
    scope: str = Field(description="Namespace scope string.")
    original_index: int = Field(description="Original order index in file.")
    is_include_block: bool = Field(
        description="Whether this block is include-group.")
    is_header_file: bool = Field(description="Whether file is header.")
    is_protected: bool = Field(description="Inside clang-format off region.")

    @field_serializer("line_range", when_used="json")
    def serialize_line_range(self, value: slice) -> dict:
        return {"start": value.start, "stop": value.stop, "step": value.step}


class ParsedFile(BaseModel):
    path: Path = Field(description="Parsed file path.")
    text: str = Field(description="Original file text.")
    lines: list[str] = Field(description="Original file lines with endings.")
    blocks: list[CodeBlock] = Field(
        description="Extracted reorderable blocks.")


class RepositoryParse(BaseModel):
    root: Path = Field(description="Repository root path.")
    files: list[ParsedFile] = Field(description="All parsed files.")


@dataclass
class RepositoryGraph:
    graph: igraph.Graph = field(metadata={"doc": "Directed dependency graph."})
    blocks: List[CodeBlock] = field(
        metadata={"doc": "Blocks in vertex index order."})
    id_to_index: Dict[str, int] = field(
        metadata={"doc": "Map from block id to graph vertex index."})


@beartype
def _make_parser() -> Parser:
    language = Language(tree_sitter_cpp.language())
    return Parser(language)


@beartype
def _run_git_ls_files(root: Path) -> List[Path]:
    cmd = [
        "git",
        "-C",
        str(root),
        "ls-files",
        "--cached",
        "--others",
        "--exclude-standard",
    ]
    result = subprocess.run(cmd, check=True, capture_output=True, text=True)
    files: List[Path] = []
    for line in result.stdout.splitlines():
        if line.strip() == "":
            continue
        files.append(root / line.strip())
    return files


@beartype
def _is_relevant_cpp(path: Path) -> bool:
    return path.suffix.lower() in CPP_ALL


@beartype
def _clang_format_off_ranges(lines: Sequence[str]) -> List[slice]:
    ranges: List[slice] = []
    active_start: Optional[int] = None
    idx: int = 0
    while idx < len(lines):
        line = lines[idx]
        if "/* clang-format off */" in line:
            active_start = idx
        if "/* clang-format on */" in line and active_start is not None:
            ranges.append(slice(active_start, idx + 1))
            active_start = None
        idx += 1
    if active_start is not None:
        ranges.append(slice(active_start, len(lines)))
    return ranges


@beartype
def _slice_overlaps_any(target: slice, ranges: Sequence[slice]) -> bool:
    for item in ranges:
        if target.stop <= item.start:
            continue
        if item.stop <= target.start:
            continue
        return True
    return False


@beartype
def _leading_comment_adjust(lines: Sequence[str], start_line: int) -> int:
    line_idx: int = start_line
    in_block: bool = False
    while 0 < line_idx:
        candidate = lines[line_idx - 1]
        stripped = candidate.strip()
        if stripped == "":
            line_idx -= 1
            continue
        if stripped.endswith("*/"):
            in_block = True
            line_idx -= 1
            continue
        if in_block:
            if "/*" in stripped:
                in_block = False
            line_idx -= 1
            continue
        if stripped.startswith("//"):
            line_idx -= 1
            continue
        break
    return line_idx


@beartype
def _extract_symbol_from_text(node_type: str, text: str, scope: str) -> str:
    normalized = " ".join(text.replace("\n", " ").split())
    if node_type == "template_declaration":
        spec_match = re.search(
            r"([A-Za-z_]\w*(?:::[A-Za-z_]\w*)*)\s*<([^>]*)>\s*\(", normalized)
        if spec_match is not None:
            local = f"{spec_match.group(1)}<{spec_match.group(2).strip()}>"
            if "::" in local:
                return local
            if scope == "":
                return local
            return f"{scope}::{local}"
    class_match = re.search(r"\b(class|struct|enum)\s+([A-Za-z_]\w*)",
                            normalized)
    if class_match is not None:
        local = class_match.group(2)
        if scope == "":
            return local
        return f"{scope}::{local}"
    func_match = re.search(
        r"([A-Za-z_]\w*(?:::[A-Za-z_]\w*)*(?:<[^>]+>)?)\s*\(", normalized)
    if func_match is not None:
        local = func_match.group(1)
        if "::" in local:
            return local
        if scope == "":
            return local
        return f"{scope}::{local}"
    name_match = re.search(r"([A-Za-z_]\w*)", normalized)
    if name_match is not None:
        local = name_match.group(1)
        if scope == "":
            return local
        return f"{scope}::{local}"
    if scope == "":
        return normalized
    return f"{scope}::{normalized}"


@beartype
def _namespace_name(node: Node, lines: Sequence[str]) -> str:
    name_node = node.child_by_field_name("name")
    if name_node is None:
        return f"__anon_ns_line_{node.start_point[0] + 1}"
    line = lines[name_node.start_point[0]]
    return line[name_node.start_point[1]:name_node.end_point[1]]


@beartype
def _collect_blocks_from_node(
    node: Node,
    path: Path,
    is_header: bool,
    lines: Sequence[str],
    protected_ranges: Sequence[slice],
    scope_stack: Sequence[str],
    block_counter_start: int,
) -> Tuple[List[CodeBlock], int]:
    blocks: List[CodeBlock] = []
    block_counter: int = block_counter_start
    if node.type == "namespace_definition":
        ns_name = _namespace_name(node, lines)
        new_scope = list(scope_stack)
        new_scope.append(ns_name)
        body = node.child_by_field_name("body")
        if body is not None:
            idx: int = 0
            while idx < len(body.named_children):
                sub = body.named_children[idx]
                sub_blocks, block_counter = _collect_blocks_from_node(
                    sub,
                    path,
                    is_header,
                    lines,
                    protected_ranges,
                    new_scope,
                    block_counter,
                )
                blocks.extend(sub_blocks)
                idx += 1
        return blocks, block_counter
    if node.type in CAPTURE_TYPES:
        start_line = node.start_point[0]
        end_line = node.end_point[0] + 1
        adjusted_line = _leading_comment_adjust(lines, start_line)
        line_range = slice(adjusted_line, end_line)
        if _slice_overlaps_any(line_range, protected_ranges):
            return blocks, block_counter
        text = "".join(lines[line_range.start:line_range.stop])
        scope = "::".join(scope_stack)
        key = _extract_symbol_from_text(node.type, text, scope)
        block_id = f"{path.as_posix()}::{block_counter}"
        block = CodeBlock(
            id=block_id,
            path=path,
            key=key,
            line_range=line_range,
            text=text,
            scope=scope,
            original_index=block_counter,
            is_include_block=False,
            is_header_file=is_header,
            is_protected=False,
        )
        blocks.append(block)
        block_counter += 1
        return blocks, block_counter
    if node.type in {
            "translation_unit", "declaration_list", "linkage_specification"
    }:
        idx = 0
        while idx < len(node.named_children):
            sub = node.named_children[idx]
            sub_blocks, block_counter = _collect_blocks_from_node(
                sub,
                path,
                is_header,
                lines,
                protected_ranges,
                scope_stack,
                block_counter,
            )
            blocks.extend(sub_blocks)
            idx += 1
    return blocks, block_counter


@beartype
def parse_cpp_file(path: Path, parser: Parser) -> ParsedFile:
    logging.getLogger("main.parser").debug(f"Parsing file {path}", )
    data = path.read_bytes()
    text = data.decode("utf-8")
    lines = text.splitlines(keepends=True)
    tree = parser.parse(data)
    if tree.root_node.has_error:
        node_repr = str(tree.root_node)
        raise ValueError("Parse failed with syntax errors: {}\n{}".format(
            path,
            node_repr,
        ))
    protected_ranges = _clang_format_off_ranges(lines)
    root = tree.root_node

    include_nodes: List[Node] = []
    for idx in range(len(root.named_children)):
        child = root.named_children[idx]
        if child.type in ["preproc_include", "preproc_call"]:
            include_nodes.append(child)

    blocks: List[CodeBlock] = []
    counter: int = 0

    if 0 < len(include_nodes):
        first = include_nodes[0]
        last = include_nodes[-1]
        start_line = first.start_point[0]
        end_line = last.end_point[0] + 1
        adjusted_line = _leading_comment_adjust(lines, start_line)
        include_range = slice(adjusted_line, end_line)
        if not _slice_overlaps_any(include_range, protected_ranges):
            include_text = "".join(
                lines[include_range.start:include_range.stop])
            include_block = CodeBlock(
                id=f"{path.as_posix()}//includes",
                path=path,
                key="__includes__",
                line_range=include_range,
                text=include_text,
                scope="",
                original_index=counter,
                is_include_block=True,
                is_header_file=path.suffix.lower() in CPP_HEADERS,
                is_protected=False,
            )
            blocks.append(include_block)
            counter += 1

    protected_index: int = 0
    while protected_index < len(protected_ranges):
        protected_range = protected_ranges[protected_index]
        protected_text = "".join(
            lines[protected_range.start:protected_range.stop])
        protected_block = CodeBlock(
            id=f"{path.as_posix()}//protected//{protected_index}",
            path=path,
            key=f"__clang_format_block__//{protected_index}",
            line_range=protected_range,
            text=protected_text,
            scope="",
            original_index=counter,
            is_include_block=False,
            is_header_file=path.suffix.lower() in CPP_HEADERS,
            is_protected=True,
        )
        blocks.append(protected_block)
        counter += 1
        protected_index += 1

    idx = 0
    while idx < len(root.named_children):
        child = root.named_children[idx]
        if child.type == "preproc_include":
            idx += 1
            continue
        child_blocks, counter = _collect_blocks_from_node(
            child,
            path,
            path.suffix.lower() in CPP_HEADERS,
            lines,
            protected_ranges,
            [],
            counter,
        )
        blocks.extend(child_blocks)
        idx += 1

    blocks.sort(key=lambda b: b.line_range.start)
    return ParsedFile(path=path, text=text, lines=lines, blocks=blocks)


@beartype
def _rebuild_with_replacements(parsed: ParsedFile,
                               replacement: Dict[str, str]) -> str:
    blocks = sorted(parsed.blocks, key=lambda b: b.line_range.start)
    result_parts: List[str] = []
    cursor: int = 0
    idx: int = 0
    while idx < len(blocks):
        block = blocks[idx]
        result_parts.extend(parsed.lines[cursor:block.line_range.start])
        result_parts.append(replacement[block.id])
        cursor = block.line_range.stop
        idx += 1
    result_parts.extend(parsed.lines[cursor:])
    return "".join(result_parts)


@beartype
def rebuild_identity(parsed: ParsedFile) -> str:
    replacement: Dict[str, str] = {}
    for block in parsed.blocks:
        replacement[block.id] = block.text
    return _rebuild_with_replacements(parsed, replacement)


@beartype
def parse_repository(root: Path) -> RepositoryParse:
    parser = _make_parser()
    files = _run_git_ls_files(root)
    parsed_files: List[ParsedFile] = []
    for path in files:
        if not _is_relevant_cpp(path):
            continue
        parsed_files.append(parse_cpp_file(path, parser))
    return RepositoryParse(root=root, files=parsed_files)


@beartype
def build_repository_graph(repo: RepositoryParse) -> RepositoryGraph:
    blocks: List[CodeBlock] = []
    for parsed in repo.files:
        blocks.extend(parsed.blocks)
    graph = igraph.Graph(directed=True)
    graph.add_vertices(len(blocks))
    id_to_index: Dict[str, int] = {}
    for idx in range(len(blocks)):
        block = blocks[idx]
        id_to_index[block.id] = idx
        graph.vs[idx]["id"] = block.id
        graph.vs[idx]["key"] = block.key
        graph.vs[idx]["path"] = str(block.path.relative_to(repo.root))
        graph.vs[idx]["text"] = block.text
        graph.vs[idx]["line_start"] = block.line_range.start
        graph.vs[idx]["line_stop"] = block.line_range.stop

    file_to_blocks: Dict[Path, List[CodeBlock]] = {}
    for block in blocks:
        file_to_blocks.setdefault(block.path, []).append(block)
    for path in file_to_blocks:
        file_to_blocks[path].sort(key=lambda b: b.line_range.start)

    edges: Set[Tuple[int, int]] = set()
    for path, file_blocks in file_to_blocks.items():
        include_block: Optional[CodeBlock] = None
        for block in file_blocks:
            if block.is_include_block:
                include_block = block
                break
        if include_block is not None:
            for block in file_blocks:
                if block.id == include_block.id:
                    continue
                edges.add(
                    (id_to_index[include_block.id], id_to_index[block.id]))

    header_keys: Set[str] = set()
    for block in blocks:
        if block.is_header_file and not block.is_include_block:
            header_keys.add(block.key)

    for path, file_blocks in file_to_blocks.items():
        locals_only: List[CodeBlock] = []
        for block in file_blocks:
            if block.is_include_block:
                continue
            if block.key in header_keys:
                continue
            locals_only.append(block)
        locals_only.sort(key=lambda b: b.original_index)
        i: int = 0
        while i + 1 < len(locals_only):
            left = locals_only[i]
            right = locals_only[i + 1]
            edges.add((id_to_index[left.id], id_to_index[right.id]))
            i += 1

    key_to_source_blocks: Dict[str, List[CodeBlock]] = {}
    key_to_header_blocks_by_file: Dict[Path, List[CodeBlock]] = {}
    for block in blocks:
        if block.is_include_block:
            continue
        if block.is_header_file:
            key_to_header_blocks_by_file.setdefault(block.path,
                                                    []).append(block)
        else:
            key_to_source_blocks.setdefault(block.key, []).append(block)

    for header_path in key_to_header_blocks_by_file:
        header_blocks = key_to_header_blocks_by_file[header_path]
        header_blocks.sort(key=lambda b: b.original_index)
        i = 0
        while i + 1 < len(header_blocks):
            left = header_blocks[i]
            right = header_blocks[i + 1]
            left_sources = key_to_source_blocks.get(left.key, [])
            right_sources = key_to_source_blocks.get(right.key, [])
            for src_left in left_sources:
                for src_right in right_sources:
                    edges.add(
                        (id_to_index[src_left.id], id_to_index[src_right.id]))
            i += 1

    if 0 < len(edges):
        graph.add_edges(sorted(list(edges)))
    return RepositoryGraph(graph=graph, blocks=blocks, id_to_index=id_to_index)


@beartype
def _stable_topological_order_for_file(
        parsed: ParsedFile, repo_graph: RepositoryGraph) -> List[CodeBlock]:
    file_blocks = sorted(parsed.blocks, key=lambda b: b.line_range.start)
    file_vertex_indices: List[int] = []
    for block in file_blocks:
        file_vertex_indices.append(repo_graph.id_to_index[block.id])

    file_graph = repo_graph.graph.induced_subgraph(file_vertex_indices)
    local_order = file_graph.topological_sorting(mode="out")

    ordered: List[CodeBlock] = []
    for local_idx in local_order:
        global_idx = file_vertex_indices[local_idx]
        ordered.append(repo_graph.blocks[global_idx])

    if len(ordered) != len(file_blocks):
        raise ValueError(f"Topological ordering failed for {parsed.path}")
    return ordered


@beartype
def _write_reordered_file(parsed: ParsedFile,
                          ordered_blocks: Sequence[CodeBlock]) -> None:
    replacement: Dict[str, str] = {}
    ordered_iter: List[CodeBlock] = list(ordered_blocks)
    by_slot = sorted(parsed.blocks, key=lambda b: b.line_range.start)
    idx = 0
    while idx < len(by_slot):
        slot = by_slot[idx]
        replacement[slot.id] = ordered_iter[idx].text
        idx += 1
    before_non_empty = {
        line
        for line in parsed.text.splitlines() if line.strip() != ""
    }
    rebuilt = _rebuild_with_replacements(parsed, replacement)
    after_non_empty = {
        line
        for line in rebuilt.splitlines() if line.strip() != ""
    }
    if before_non_empty != after_non_empty:
        raise ValueError(f"Line set mismatch after write for {parsed.path}")
    log.debug(f"Writing file {parsed.path}", )
    parsed.path.write_text(rebuilt, encoding="utf-8")


@beartype
def sort_repository(root: Path, graph_png: Optional[Path]) -> RepositoryGraph:
    repo = parse_repository(root)
    repo_graph = build_repository_graph(repo)
    if graph_png is not None:
        visualize_repository_graph(repo, repo_graph, graph_png)

    for parsed in repo.files:
        if parsed.path.suffix.lower() not in CPP_SOURCES:
            continue
        ordered_blocks = _stable_topological_order_for_file(parsed, repo_graph)
        _write_reordered_file(parsed, ordered_blocks)

    return repo_graph


@beartype
def _highlight_html(code: str) -> str:
    from html import escape
    code = escape(code)
    highlighted = code.replace("\n", "<BR ALIGN=\"LEFT\"/>")
    return highlighted


@beartype
def visualize_repository_graph(repo: RepositoryParse,
                               repo_graph: RepositoryGraph,
                               output_png: Path) -> None:
    output_png.parent.mkdir(parents=True, exist_ok=True)
    dot = graphviz.Digraph("repo_graph", format="png")
    dot.attr(rankdir="LR")
    file_to_nodes: Dict[Path, List[CodeBlock]] = {}
    for block in repo_graph.blocks:
        file_to_nodes.setdefault(block.path, []).append(block)
    cluster_index: int = 0
    for path in sorted(file_to_nodes.keys(), key=lambda p: p.as_posix()):
        sub = graphviz.Digraph(name=f"cluster_{cluster_index}")
        sub.attr(label=str(path.relative_to(repo.root)))
        sub.attr(style="rounded")
        nodes = sorted(file_to_nodes[path], key=lambda b: b.line_range.start)
        for block in nodes:
            title = html.escape(
                f"{path.relative_to(repo.root)} {block.line_range.start}-{block.line_range.stop}"
            )
            body = _highlight_html(block.text)
            label = ("<"
                     "<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLPADDING=\"4\">"
                     f"<TR><TD><B>{title}</B></TD></TR>"
                     f"<TR><TD ALIGN=\"LEFT\">{body}</TD></TR>"
                     "</TABLE>"
                     ">")
            sub.node(block.id, label=label, shape="plain")
        dot.subgraph(sub)
        cluster_index += 1
    for edge in repo_graph.graph.es:
        src = repo_graph.blocks[edge.source].id
        dst = repo_graph.blocks[edge.target].id
        dot.edge(src, dst)
    rendered_base = output_png.with_suffix("")
    try:
        dot.render(filename=str(rendered_base), cleanup=True)

    except graphviz.backend.execute.CalledProcessError:
        raise RuntimeError(str(dot)) from None


@beartype
def _cli() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("root", type=Path)
    parser.add_argument("--graph-png", type=Path, default=None)
    args = parser.parse_args()
    sort_repository(args.root, args.graph_png)


if __name__ == "__main__":
    _cli()

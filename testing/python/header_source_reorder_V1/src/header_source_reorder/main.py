#!/usr/bin/env python
import argparse

import logging

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)
logging.getLogger("graphviz").setLevel(logging.ERROR)

log = logging.getLogger(__name__)

import html

import re
import subprocess
from pathlib import Path

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
from header_source_reorder.types import CodeBlock, ParsedFile, RepositoryGraph, RepositoryParse
from header_source_reorder.parse import parse_cpp_file
from header_source_reorder.common import CPP_ALL, CPP_SOURCES


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

from beartype import beartype
from beartype.typing import Dict, List, Optional, Sequence, Set, Tuple
import re

import html

import re
import subprocess
from pathlib import Path

from beartype import beartype
from beartype.typing import Dict, List, Optional, Sequence, Set, Tuple
from tree_sitter import Language, Node, Parser
from header_source_reorder.types import CodeBlock, ParsedFile, RepositoryGraph, RepositoryParse
from header_source_reorder.common import CAPTURE_TYPES, CPP_HEADERS
import logging


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

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
from header_source_reorder.types import CodeBlock, ParsedFile, QualType, BlockType
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
def _namespace_name(node: Node, lines: Sequence[str]) -> str:
    name_node = node.child_by_field_name("name")
    if name_node is None:
        return f"__anon_ns_line_{node.start_point[0] + 1}"
    line = lines[name_node.start_point[0]]
    return line[name_node.start_point[1]:name_node.end_point[1]].strip()


@beartype
def _parse_param_type(raw: str) -> QualType:
    token = " ".join(raw.strip().split())
    qualifiers: List[QualType] = []
    if "const" in token.split():
        qualifiers.append(QualType(name="const"))
    if "volatile" in token.split():
        qualifiers.append(QualType(name="volatile"))
    if "*" in token:
        qualifiers.append(QualType(name="ptr"))
    if "&" in token:
        qualifiers.append(QualType(name="ref"))
    cleaned = token.replace("const", "").replace("volatile", "")
    cleaned = cleaned.replace("*", " ").replace("&", " ")
    cleaned = " ".join(cleaned.split())
    if cleaned == "":
        cleaned = "void"
    return QualType(name=cleaned, parameters=tuple(qualifiers))


@beartype
def _extract_qualified_name(
    node_type: str,
    text: str,
    scope_stack: Sequence[str],
    file_namespace: str,
) -> Optional[QualType]:
    normalized = " ".join(text.replace("\n", " ").split())
    if node_type in {"preproc_include", "preproc_call", "preproc_def"}:
        return None
    function_match = re.search(
        r"([~A-Za-z_]\w*(?:::[A-Za-z_]\w*)*)\s*\(([^)]*)\)", normalized)
    if function_match is not None:
        full_name = function_match.group(1)
        params_raw = function_match.group(2).strip()
        parts = full_name.split("::")
        name = parts[-1]
        parents: List[QualType] = [QualType(name=file_namespace)]
        idx = 0
        while idx < len(scope_stack):
            parents.append(QualType(name=scope_stack[idx]))
            idx += 1
        idx = 0
        while idx + 1 < len(parts):
            parents.append(QualType(name=parts[idx]))
            idx += 1
        params: List[QualType] = []
        if params_raw != "":
            raw_items = [item.strip() for item in params_raw.split(",")]
            idx = 0
            while idx < len(raw_items):
                params.append(_parse_param_type(raw_items[idx]))
                idx += 1
        return QualType(name=name,
                        parent_namespaces=tuple(parents),
                        parameters=tuple(params))
    type_match = re.search(
        r"\b(enum class|enum|struct|class|union)\s+([A-Za-z_]\w*)", normalized)
    if type_match is not None:
        parents = [QualType(name=file_namespace)]
        idx = 0
        while idx < len(scope_stack):
            parents.append(QualType(name=scope_stack[idx]))
            idx += 1
        return QualType(name=type_match.group(2),
                        parent_namespaces=tuple(parents))
    decl_match = re.search(r"([A-Za-z_]\w*)\s*;", normalized)
    if decl_match is not None:
        parents = [QualType(name=file_namespace)]
        idx = 0
        while idx < len(scope_stack):
            parents.append(QualType(name=scope_stack[idx]))
            idx += 1
        return QualType(name=decl_match.group(1),
                        parent_namespaces=tuple(parents))
    return None


@beartype
def _make_block(
    path: Path,
    lines: Sequence[str],
    line_range: slice,
    block_type: BlockType,
    counter: int,
    scope_stack: Sequence[str],
    node_type: str,
) -> CodeBlock:
    text = "".join(lines[line_range.start:line_range.stop])
    scope = "::".join(scope_stack)
    file_namespace = path.name
    qualified_name = _extract_qualified_name(node_type, text, scope_stack,
                                             file_namespace)
    key = qualified_name.name if qualified_name is not None else f"__local__{counter}"
    return CodeBlock(
        id=f"{path.as_posix()}::{counter}",
        path=path,
        key=key,
        line_range=line_range,
        text=text,
        scope=scope,
        original_index=counter,
        block_type=block_type,
        qualified_name=qualified_name
        if block_type != BlockType.LOCAL_ENTRY else None,
    )


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
    block_counter = block_counter_start
    if node.type == "namespace_definition":
        body = node.child_by_field_name("body")
        if body is None:
            line_range = slice(node.start_point[0], node.end_point[0] + 1)
            blocks.append(
                _make_block(
                    path,
                    lines,
                    line_range,
                    BlockType.LOCAL_ENTRY,
                    block_counter,
                    scope_stack,
                    node.type,
                ))
            return blocks, block_counter + 1
        child_count = len(body.named_children)
        if node.start_point[0] == node.end_point[0] and 1 < child_count:
            logging.getLogger("main.parser").warning(
                "Multiple elements on one line in namespace at %s:%s",
                path.as_posix(),
                node.start_point[0] + 1,
            )
            line_range = slice(node.start_point[0], node.end_point[0] + 1)
            blocks.append(
                _make_block(
                    path,
                    lines,
                    line_range,
                    BlockType.LOCAL_ENTRY,
                    block_counter,
                    scope_stack,
                    node.type,
                ))
            return blocks, block_counter + 1
        ns_name = _namespace_name(node, lines)
        open_range = slice(node.start_point[0],
                           min(node.start_point[0] + 1, len(lines)))
        if not _slice_overlaps_any(open_range, protected_ranges):
            open_block = _make_block(
                path,
                lines,
                open_range,
                BlockType.NAMESPACE_OPEN,
                block_counter,
                scope_stack + [ns_name],
                node.type,
            )
            blocks.append(open_block)
            block_counter += 1
        idx = 0
        while idx < child_count:
            sub = body.named_children[idx]
            sub_blocks, block_counter = _collect_blocks_from_node(
                sub,
                path,
                is_header,
                lines,
                protected_ranges,
                list(scope_stack) + [ns_name],
                block_counter,
            )
            blocks.extend(sub_blocks)
            idx += 1
        close_line = body.end_point[0]
        close_range = slice(close_line, min(close_line + 1, len(lines)))
        if not _slice_overlaps_any(close_range, protected_ranges):
            close_block = _make_block(
                path,
                lines,
                close_range,
                BlockType.NAMESPACE_CLOSE,
                block_counter,
                scope_stack + [ns_name],
                node.type,
            )
            blocks.append(close_block)
            block_counter += 1
        return blocks, block_counter
    start_line = node.start_point[0]
    end_line = node.end_point[0] + 1
    adjusted_line = _leading_comment_adjust(lines, start_line)
    line_range = slice(adjusted_line, end_line)
    if _slice_overlaps_any(line_range, protected_ranges):
        return blocks, block_counter
    if node.type in {"preproc_include", "preproc_call", "preproc_def"}:
        blocks.append(
            _make_block(
                path,
                lines,
                line_range,
                BlockType.PREPROCESSOR_BLOCK,
                block_counter,
                scope_stack,
                node.type,
            ))
        return blocks, block_counter + 1
    declaration_types = {
        "declaration",
        "field_declaration",
        "function_definition",
        "class_specifier",
        "struct_specifier",
        "union_specifier",
        "enum_specifier",
        "enumerator",
        "template_declaration",
    }
    if node.type in declaration_types:
        if is_header:
            block_type = BlockType.DECLARATION
        else:
            if node.type == "function_definition":
                block_type = BlockType.DEFINITION
            else:
                block_type = BlockType.LOCAL_ENTRY
        blocks.append(
            _make_block(
                path,
                lines,
                line_range,
                block_type,
                block_counter,
                scope_stack,
                node.type,
            ))
        block_counter += 1
        if is_header and node.type in {
                "class_specifier", "struct_specifier", "union_specifier",
                "enum_specifier"
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
    if len(node.named_children) == 0:
        blocks.append(
            _make_block(
                path,
                lines,
                line_range,
                BlockType.LOCAL_ENTRY,
                block_counter,
                scope_stack,
                node.type,
            ))
        return blocks, block_counter + 1
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
def _merge_adjacent_local_entries(
        blocks: Sequence[CodeBlock]) -> List[CodeBlock]:
    merged: List[CodeBlock] = []
    idx = 0
    while idx < len(blocks):
        current = blocks[idx]
        if len(merged) == 0:
            merged.append(current)
            idx += 1
            continue
        prev = merged[-1]
        if (prev.block_type == BlockType.LOCAL_ENTRY
                and current.block_type == BlockType.LOCAL_ENTRY
                and prev.path == current.path
                and current.line_range.start <= prev.line_range.stop):
            merged[-1] = CodeBlock(
                id=prev.id,
                path=prev.path,
                key=prev.key,
                line_range=slice(prev.line_range.start,
                                 current.line_range.stop),
                text=prev.text + current.text,
                scope=prev.scope,
                original_index=prev.original_index,
                block_type=BlockType.LOCAL_ENTRY,
                qualified_name=None,
            )
            idx += 1
            continue
        merged.append(current)
        idx += 1
    return merged


@beartype
def parse_cpp_file(path: Path, parser: Parser) -> ParsedFile:
    logging.getLogger("main.parser").debug(f"Parsing file {path}")
    data = path.read_bytes()
    text = data.decode("utf-8")
    lines = text.splitlines(keepends=True)
    tree = parser.parse(data)
    if tree.root_node.has_error:
        raise ValueError(
            f"Parse failed with syntax errors: {path}\n{tree.root_node}")
    protected_ranges = _clang_format_off_ranges(lines)
    root = tree.root_node
    is_header = path.suffix.lower() in CPP_HEADERS
    blocks: List[CodeBlock] = []
    counter = 0
    idx = 0
    while idx < len(protected_ranges):
        block = _make_block(
            path,
            lines,
            protected_ranges[idx],
            BlockType.CLANG_FORMAT_BLOCK,
            counter,
            [],
            "clang_format",
        )
        block = CodeBlock(
            id=block.id,
            path=block.path,
            key=block.key,
            line_range=block.line_range,
            text=block.text,
            scope=block.scope,
            original_index=block.original_index,
            block_type=BlockType.CLANG_FORMAT_BLOCK,
            qualified_name=None,
        )
        blocks.append(block)
        counter += 1
        idx += 1
    idx = 0
    while idx < len(root.named_children):
        child = root.named_children[idx]
        child_blocks, counter = _collect_blocks_from_node(
            child,
            path,
            is_header,
            lines,
            protected_ranges,
            [],
            counter,
        )
        blocks.extend(child_blocks)
        idx += 1
    blocks.sort(key=lambda item: item.line_range.start)
    blocks = _merge_adjacent_local_entries(blocks)
    return ParsedFile(path=path, text=text, lines=lines, blocks=blocks)

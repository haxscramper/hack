#!/usr/bin/env python

from __future__ import annotations

import argparse
from difflib import SequenceMatcher
import json
import re
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

import tree_sitter_cpp
from beartype import beartype
from beartype.typing import Iterable, Iterator, Sequence
from loguru import logger
from tree_sitter import Language, Node, Parser, Tree

from header_source_reorder_v2.common_parse import format_tree
from header_source_reorder_v2.extract_header_entries import header_entries
from header_source_reorder_v2.extract_source_entries import source_definition_blocks
from header_source_reorder_v2.models import HeaderEntry, HeaderEntryKind, QualifiedName, SortMismatch, SortResult, SourceBlock, SourceBlockKind, SourceContext


@beartype
def create_parser() -> Parser:
    language = Language(tree_sitter_cpp.language())
    return Parser(language)


@beartype
def parse_file(parser: Parser, path: Path) -> tuple[bytes, Tree]:
    source = path.read_bytes()
    return source, parser.parse(source)


@beartype
def flat_source_blocks(
        source: bytes,
        definitions: Sequence[SourceBlock]) -> list[SourceBlock]:
    result: list[SourceBlock] = []
    offset = 0

    for definition in definitions:
        if offset < definition.start_byte:
            prefix = source[offset:definition.start_byte]
            start_line = source[:offset].count(b"\n") + 1
            end_line = start_line + prefix.count(b"\n")
            result.append(
                SourceBlock(
                    kind=SourceBlockKind.FIXED,
                    start_byte=offset,
                    end_byte=definition.start_byte,
                    start_line=start_line,
                    end_line=end_line,
                    context=definition.context,
                    qualified_name=None,
                    content=prefix,
                ))

        result.append(definition)
        offset = definition.end_byte

    if offset < len(source):
        suffix = source[offset:]
        start_line = source[:offset].count(b"\n") + 1
        end_line = start_line + suffix.count(b"\n")
        context = (definitions[-1].context if definitions else SourceContext(
            scopes=(), macro_conditions=()))
        result.append(
            SourceBlock(
                kind=SourceBlockKind.FIXED,
                start_byte=offset,
                end_byte=len(source),
                start_line=start_line,
                end_line=end_line,
                context=context,
                qualified_name=None,
                content=suffix,
            ))

    return result


@beartype
def header_ranks(entries: Sequence[HeaderEntry]) -> dict[str, int]:
    result: dict[str, int] = {}

    for entry in entries:
        if entry.kind not in {
                HeaderEntryKind.METHOD,
                HeaderEntryKind.FUNCTION,
        }:
            continue

        signature = entry.qualified_name.signature()

        if signature not in result:
            result[signature] = len(result)

    return result


@beartype
def render_assumed_mismatch(
    source_name: QualifiedName,
    header_name: QualifiedName,
) -> str:
    details: list[str] = []

    for index, (source_parameter,
                header_parameter) in enumerate(zip(source_name.parameters,
                                                   header_name.parameters),
                                               start=1):
        source_type = source_parameter.canonical()
        header_type = header_parameter.canonical()

        if source_type == header_type:
            continue

        matcher = SequenceMatcher(
            None,
            source_type,
            header_type,
            autojunk=False,
        )

        for operation, source_start, source_end, header_start, header_end in (
                matcher.get_opcodes()):
            if operation == "equal":
                continue

            source_fragment = source_type[source_start:source_end].strip()
            header_fragment = header_type[header_start:header_end].strip()

            if operation == "insert":
                remainder = header_type[header_end:]
                next_name = re.search(r"[A-Za-z_]\w*", remainder)
                location = (f" before `{next_name.group(0)}`"
                            if next_name is not None else "")
                details.append(f"argument {index}: source is missing "
                               f"`{header_fragment}`{location}")
            elif operation == "delete":
                details.append(f"argument {index}: source has unexpected "
                               f"`{source_fragment}`")
            else:
                details.append(
                    f"argument {index}: source has `{source_fragment}`, "
                    f"header expects `{header_fragment}`")

    if source_name.qualifiers != header_name.qualifiers:
        source_qualifiers = ",".join(
            sorted(qualifier.value for qualifier in source_name.qualifiers))
        header_qualifiers = ",".join(
            sorted(qualifier.value for qualifier in header_name.qualifiers))
        details.append(
            f"method qualifiers: source has `[{source_qualifiers}]`, "
            f"header expects `[{header_qualifiers}]`")

    if not details:
        details.append(f"qualified scope: source has `{source_name.path()}`, "
                       f"header expects `{header_name.path()}`")

    return "; ".join(details)


@beartype
def warn_missing_header_methods(
    source_path: Path,
    entries: Sequence[HeaderEntry],
    definitions: Sequence[SourceBlock],
) -> None:
    methods = [
        entry for entry in entries if entry.kind == HeaderEntryKind.METHOD
    ]
    header_signatures = {entry.qualified_name.signature() for entry in methods}

    for block in definitions:
        source_name = block.qualified_name

        if source_name is None:
            continue

        if source_name.signature() in header_signatures:
            continue

        candidates: list[HeaderEntry] = []

        for entry in methods:
            header_name = entry.qualified_name

            if header_name.name != source_name.name:
                continue

            if len(header_name.parameters) != len(source_name.parameters):
                continue

            source_scopes = source_name.parent_scopes
            header_scopes = header_name.parent_scopes

            if len(source_scopes) > len(header_scopes):
                continue

            if header_scopes[-len(source_scopes):] != source_scopes:
                continue

            candidates.append(entry)

        if not candidates:
            continue

        assumed_entry = max(
            candidates,
            key=lambda entry: SequenceMatcher(
                None,
                source_name.signature(),
                entry.qualified_name.signature(),
                autojunk=False,
            ).ratio(),
        )
        assumed_name = assumed_entry.qualified_name
        mismatch = render_assumed_mismatch(source_name, assumed_name)

        logger.warning(f"{source_path}:{block.start_line}-{block.end_line}: "
                       f"{source_name.signature()} has no exact header match; "
                       f"possible declaration: {assumed_name.signature()} "
                       f"at header line {assumed_entry.line}\n"
                       f"Assumed mismatch: {mismatch}")


@beartype
def sort_source(
    source: bytes,
    definitions: Sequence[SourceBlock],
    ranks: dict[str, int],
) -> SortResult:
    grouped_indexes: dict[SourceContext, list[int]] = {}

    for index, block in enumerate(definitions):
        if block.qualified_name is None:
            continue

        signature = block.qualified_name.signature()

        if signature not in ranks:
            continue

        grouped_indexes.setdefault(block.context, []).append(index)

    replacement_content: dict[int, bytes] = {}
    mismatches: list[SortMismatch] = []

    for indexes in grouped_indexes.values():
        ordered_indexes = sorted(
            indexes,
            key=lambda index: ranks[definitions[index].qualified_name.
                                    signature()],
        )
        ordered_blocks = [definitions[index] for index in ordered_indexes]

        for target_index, expected_block in zip(indexes, ordered_blocks):
            current_block = definitions[target_index]
            current_name = current_block.qualified_name
            expected_name = expected_block.qualified_name

            if current_name is None or expected_name is None:
                raise ValueError(
                    f"Definition block at line {current_block.start_line} "
                    f"does not have a qualified name")

            replacement_content[target_index] = expected_block.content

            if current_name.signature() != expected_name.signature():
                mismatches.append(
                    SortMismatch(
                        line=current_block.start_line,
                        current_name=current_name.signature(),
                        expected_name=expected_name.signature(),
                    ))

    result = bytearray()
    offset = 0

    for index, block in enumerate(definitions):
        result.extend(source[offset:block.start_byte])
        result.extend(replacement_content.get(index, block.content))
        offset = block.end_byte

    result.extend(source[offset:])
    return SortResult(content=bytes(result), mismatches=mismatches)


@beartype
def write_diagnostics(
    output_directory: Path,
    header_tree: Tree,
    source_tree: Tree,
    entries: Sequence[HeaderEntry],
    definitions: Sequence[SourceBlock],
) -> None:
    output_directory.mkdir(parents=True, exist_ok=True)

    (output_directory / "header-tree.txt").write_text(
        f"{format_tree(header_tree.root_node)}\n",
        encoding="utf-8",
    )
    (output_directory / "source-tree.txt").write_text(
        f"{format_tree(source_tree.root_node)}\n",
        encoding="utf-8",
    )

    header_lines = [
        f"{entry.line}: {entry.kind.value}: "
        f"{entry.qualified_name.signature()}" for entry in entries
    ]
    (output_directory / "header-qualified-names.txt").write_text(
        "\n".join(header_lines) + ("\n" if header_lines else ""),
        encoding="utf-8",
    )

    header_by_signature = {
        entry.qualified_name.signature(): entry
        for entry in entries
    }
    source_lines: list[str] = []

    for block in definitions:
        qualified_name = block.qualified_name

        if qualified_name is None:
            continue

        signature = qualified_name.signature()
        header_entry = header_by_signature.get(signature)
        location = f"{block.start_line}-{block.end_line}"

        if header_entry is None:
            match = "header match: not found"
        else:
            match = (f"header match: {header_entry.kind.value} "
                     f"at line {header_entry.line}")

        source_lines.append(f"{location}: {signature}: {match}")

    (output_directory / "source-blocks.txt").write_text(
        "\n".join(source_lines) + ("\n" if source_lines else ""),
        encoding="utf-8",
    )


@beartype
def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=(
        "Sort C++ source definitions according to declaration order "
        "in the corresponding header"))
    parser.add_argument("header", type=Path)
    parser.add_argument("source", type=Path)
    parser.add_argument(
        "--rewrite",
        action="store_true",
        help="Overwrite the source file with the sorted result",
    )
    parser.add_argument(
        "--diagnostics-dir",
        type=Path,
        help="Write parser and qualified-name diagnostics to this directory",
    )

    return parser.parse_args()


@beartype
def main() -> int:
    arguments = parse_arguments()
    parser = create_parser()
    header_source, header_tree = parse_file(parser, arguments.header)
    source, source_tree = parse_file(parser, arguments.source)

    entries = header_entries(header_source, header_tree)
    definitions = source_definition_blocks(source, source_tree)

    warn_missing_header_methods(
        arguments.source,
        entries,
        definitions,
    )

    ranks = header_ranks(entries)

    if arguments.diagnostics_dir is not None:
        write_diagnostics(
            arguments.diagnostics_dir,
            header_tree,
            source_tree,
            entries,
            definitions,
        )

    flat_source_blocks(source, definitions)
    result = sort_source(source, definitions, ranks)

    if result.content == source:
        logger.info(
            f"{arguments.source} is already in header declaration order")
        return 0

    for mismatch in result.mismatches:
        logger.error(
            f"{arguments.source}:{mismatch.line}: "
            f"{mismatch.current_name} should be {mismatch.expected_name}")

    if arguments.rewrite:
        arguments.source.write_bytes(result.content)
        logger.info(f"Rewrote {arguments.source} with "
                    f"{len(result.mismatches)} reordered definitions")
        return 0

    logger.error(f"{arguments.source} contains "
                 f"{len(result.mismatches)} out-of-order definitions")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())

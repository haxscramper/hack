#!/usr/bin/env python

from __future__ import annotations

import argparse
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
from header_source_reorder_v2.models import HeaderEntry, HeaderEntryKind, SortMismatch, SortResult, SourceBlock, SourceBlockKind, SourceContext


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
    ranks = header_ranks(entries)
    definitions = source_definition_blocks(source, source_tree)

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

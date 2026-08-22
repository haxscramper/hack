#!/usr/bin/env python
"""Group subtrees by tag and emit simplified JSON summaries."""

import argparse
import json
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, Optional
from loguru import logger

import betterproto2
from gen import orgproto as proto

from utils import *


@dataclass
class _SubtreeFacts:
    """Intermediate extracted data for one subtree before grouping."""

    summary: SubtreeSummary
    tag_paths: list[list[str]]


@beartype
def tag_matches(tag_paths: list[list[str]], query: list[str]) -> bool:
    return any(path[:len(query)] == query for path in tag_paths)


@beartype
def extract_subtree(subtree: proto.Subtree, now: datetime) -> _SubtreeFacts:
    summary = extract_subtree_summary(subtree, now)
    return _SubtreeFacts(summary=summary, tag_paths=summary.tag_paths)


@beartype
def collect_subtrees(node: proto.AnyNode) -> list[proto.Subtree]:
    """Recursively gather all Subtree messages from an AnyNode tree."""
    result: list[Any] = []
    kind, value = betterproto2.which_one_of(node, "kind")
    if "subtree" in str(kind).lower():
        result.append(value)

    for nested in getattr(value, "subnodes", []):
        result.extend(collect_subtrees(nested))
    return result


@beartype
def group_by_tags(roots: list[proto.AnyNode],
                  queries: list[list[str]]) -> list[list[SubtreeSummary]]:
    now = datetime.now(tz=timezone.utc)
    subtrees: list[proto.Subtree] = []
    for root in roots:
        subtrees.extend(collect_subtrees(root))

    if queries:
        groups: list[list[SubtreeSummary]] = [[] for _ in queries]
        for subtree in subtrees:
            facts = extract_subtree(subtree, now)
            for index, query in enumerate(queries):
                if tag_matches(facts.tag_paths, query):
                    groups[index].append(facts.summary)
                    break
        return groups

    else:
        return [[extract_subtree(s, now).summary for s in subtrees]]


@beartype
def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "input",
        type=Path,
        help="Binary protobuf file with a top-level AnyNode/document")
    parser.add_argument("--tags",
                        nargs="+",
                        required=False,
                        default=list(),
                        help="Tag queries, nested parts separated by ##")
    parser.add_argument("--output", type=Path, default=None)
    args = parser.parse_args()

    queries = [tag.split("##") for tag in args.tags]
    for query in queries:
        if any(not part for part in query):
            raise ValueError(f"Tag query has an empty component: {query}")

    data = args.input.read_bytes()
    logger.info(f"{len(data)} size")
    document = proto.AnyNode().parse(data)

    groups = group_by_tags([document], queries)
    payload = [[summary.model_dump() for summary in group] for group in groups]

    text = json.dumps(payload, indent=1)
    if args.output:
        args.output.write_text(text)
        logger.info(f"Wrote {len(groups)} groups to {args.output}")
    else:
        sys.stdout.write(text + "\n")


if __name__ == "__main__":
    main()

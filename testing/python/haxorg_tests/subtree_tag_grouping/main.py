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
from pydantic import BaseModel

import betterproto2
from gen import orgproto as proto


class SubtreeSummary(BaseModel):
    title: str
    clocked_seconds: int
    created: Optional[str]
    deadline: Optional[str]
    closed: Optional[str]
    tags: list[list[str]]
    last_clocked: Optional[str]
    todo: Optional[str]
    effort_minutes: Optional[int]
    priority: Optional[str]
    scheduled: Optional[str]
    scheduled_delta_seconds: Optional[int]


@dataclass
class _SubtreeFacts:
    """Intermediate extracted data for one subtree before grouping."""

    summary: SubtreeSummary
    tag_paths: list[list[str]]


@beartype
def user_time_to_datetime(user_time: Any) -> Optional[datetime]:
    """Convert hstd.UserTime to datetime, assuming a wrapped Timestamp field."""
    if user_time is None:
        return None
    stamp = getattr(user_time, "time", None)
    if stamp is None:
        return None
    seconds = getattr(stamp, "seconds", None)
    if seconds is None:
        return None
    return datetime.fromtimestamp(seconds, tz=timezone.utc)


@beartype
def to_iso(user_time: Any) -> Optional[str]:
    moment = user_time_to_datetime(user_time)
    return moment.isoformat() if moment else None


@beartype
def paragraph_text(paragraph: proto.Paragraph) -> str:
    """Flatten a paragraph into plain text, dropping markup nodes."""
    parts: list[str] = []

    def walk(node: proto.AnyNode) -> None:
        kind, value = betterproto2.which_one_of(node, "kind")
        match value:
            case proto.Word():
                parts.append(value.text)
            case proto.Space():
                parts.append(" ")
            case proto.Newline():
                parts.append("\n")
            case proto.Punctuation():
                parts.append(value.text)
            case proto.RawText():
                parts.append(value.text)
            case proto.Paragraph():
                for nested in value.subnodes:
                    walk(nested)
            case _:
                for nested in getattr(value, "subnodes", []):
                    walk(nested)

    for subnode in paragraph.subnodes:
        walk(subnode)
    return "".join(parts).strip()


@beartype
def expand_hashtag(text: proto.HashTagText) -> list[list[str]]:
    """Expand a HashTagText into all prefix paths, one per nested subtag."""
    if not text.subtags:
        return [[text.head]]
    result: list[list[str]] = []
    for subtag in text.subtags:
        for nested_path in expand_hashtag(subtag):
            result.append([text.head, *nested_path])
    return result


@beartype
def tag_matches(tag_paths: list[list[str]], query: list[str]) -> bool:
    return any(path[:len(query)] == query for path in tag_paths)


@beartype
def extract_effort(properties: list[proto.NamedProperty]) -> Optional[int]:
    for prop in properties:
        kind, value = betterproto2.which_one_of(prop.data, "kind")
        if kind == "effort":
            return value.hours * 60 + value.minutes
    return None


@beartype
def extract_created(properties: list[proto.NamedProperty]) -> Optional[str]:
    for prop in properties:
        kind, value = betterproto2.which_one_of(prop.data, "kind")
        if kind == "customvalue" and value.name.upper() == "CREATED":
            return value.value
    return None


@beartype
def clock_stats(logbook: list[Any]) -> tuple[int, Optional[str]]:
    """Total clocked seconds and the most recent clock end time."""
    total = 0
    last: Optional[datetime] = None
    for entry in logbook:
        kind, value = betterproto2.which_one_of(entry.head.log, "kind")
        if kind != "clock":
            continue
        start = user_time_to_datetime(value.from_)
        end = user_time_to_datetime(value.to)
        if start and end:
            total += int((end - start).total_seconds())
            if last is None or last < end:
                last = end
    return total, last.isoformat() if last else None


@beartype
def extract_subtree(subtree: proto.Subtree, now: datetime) -> _SubtreeFacts:
    tag_paths: list[list[str]] = []
    for tag in subtree.tags:
        tag_paths.extend(expand_hashtag(tag.text))

    clocked, last_clocked = clock_stats(subtree.logbook)

    scheduled = user_time_to_datetime(subtree.scheduled)
    delta = int((scheduled - now).total_seconds()) if scheduled else None

    summary = SubtreeSummary(
        title=paragraph_text(subtree.title),
        clocked_seconds=clocked,
        created=extract_created(subtree.properties),
        deadline=to_iso(subtree.deadline),
        closed=to_iso(subtree.closed),
        tags=tag_paths,
        last_clocked=last_clocked,
        todo=subtree.todo or None,
        effort_minutes=extract_effort(subtree.properties),
        priority=subtree.priority or None,
        scheduled=scheduled.isoformat() if scheduled else None,
        scheduled_delta_seconds=delta,
    )
    return _SubtreeFacts(summary=summary, tag_paths=tag_paths)


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

    groups: list[list[SubtreeSummary]] = [[] for _ in queries]
    for subtree in subtrees:
        facts = extract_subtree(subtree, now)
        for index, query in enumerate(queries):
            if tag_matches(facts.tag_paths, query):
                groups[index].append(facts.summary)
                break
    return groups


@beartype
def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "input",
        type=Path,
        help="Binary protobuf file with a top-level AnyNode/document")
    parser.add_argument("tags",
                        nargs="+",
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

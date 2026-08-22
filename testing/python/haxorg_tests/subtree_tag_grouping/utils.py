from pydantic import BaseModel
from beartype.typing import Any, Optional
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, Optional
from gen import orgproto as proto
import betterproto2


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
            case proto.BigIdent():
                parts.append(value.text)
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
    total = 0
    last: Optional[datetime] = None

    for entry in logbook:
        kind, value = betterproto2.which_one_of(entry.head.log, "kind")
        if kind != "clock":
            continue

        start = user_time_to_datetime(value.from_)
        end = user_time_to_datetime(value.to)
        if start is not None and end is not None:
            total += int((end - start).total_seconds())
            if last is None or last < end:
                last = end

    return total, last.isoformat() if last else None


@beartype
def extract_subtree_summary(subtree: proto.Subtree,
                            now: datetime) -> SubtreeSummary:
    tag_paths: list[list[str]] = []
    for tag in subtree.tags:
        tag_paths.extend(expand_hashtag(tag.text))

    clocked, last_clocked = clock_stats(subtree.logbook)
    scheduled = user_time_to_datetime(subtree.scheduled)
    delta = int((scheduled - now).total_seconds()) if scheduled else None

    return SubtreeSummary(
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

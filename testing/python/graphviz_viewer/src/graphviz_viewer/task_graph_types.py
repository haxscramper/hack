from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum, auto

from beartype.typing import Optional


class NodeKind(Enum):
    SUBTREE = auto()
    PARAGRAPH = auto()


class TodoState(Enum):
    TODO = auto()
    NEXT = auto()
    WIP = auto()
    DONE = auto()


class Admonition(Enum):
    NOTE = auto()
    WARNING = auto()
    IMPORTANT = auto()


class EdgeKind(Enum):
    NESTED = auto()
    BLOCKED = auto()
    RELATED = auto()


class GroupKind(Enum):
    GRAPH_ROOT = auto()
    LANE = auto()
    BAND = auto()
    CALENDAR_ROOT = auto()
    CALENDAR_DAY = auto()


class ViewKind(Enum):
    GRAPH = auto()
    CALENDAR = auto()


class ElementKind(Enum):
    GROUP = auto()
    NODE = auto()
    EDGE = auto()


@dataclass(frozen=True)
class Size:
    width: float
    height: float


@dataclass(frozen=True)
class Point:
    x: float
    y: float


@dataclass(frozen=True)
class Rect:
    x: float
    y: float
    width: float
    height: float

    def translated(self, point: Point) -> Rect:
        return Rect(
            x=self.x + point.x,
            y=self.y + point.y,
            width=self.width,
            height=self.height,
        )


@dataclass(frozen=True)
class CalendarAllocation:
    start: datetime
    end: datetime


@dataclass
class InputNode:
    unique_id: str
    kind: NodeKind
    rich_text: str
    content_size: Size
    lane: str
    todo_state: Optional[TodoState] = None
    admonition: Optional[Admonition] = None
    created: Optional[datetime] = None
    deadline: Optional[datetime] = None
    scheduled: Optional[datetime] = None
    tags: list[tuple[str, ...]] = field(default_factory=list)
    effort_min: Optional[int] = None
    calendar_allocations: list[CalendarAllocation] = field(
        default_factory=list)


@dataclass(frozen=True)
class InputEdge:
    unique_id: str
    source_id: str
    target_id: str
    kind: EdgeKind


@dataclass
class InputCollection:
    nodes: list[InputNode]
    edges: list[InputEdge]


@dataclass
class NodeRect:
    unique_id: str
    source_id: str
    size: Size
    geometry: Optional[Rect] = None
    calendar_start: Optional[datetime] = None
    calendar_end: Optional[datetime] = None


@dataclass
class LayoutEdge:
    unique_id: str
    source_rect_id: str
    target_rect_id: str
    kind: EdgeKind
    points: list[Point] = field(default_factory=list)


@dataclass
class Group:
    unique_id: str
    label: str
    kind: GroupKind
    nested_groups: list[Group] = field(default_factory=list)
    nodes: list[NodeRect] = field(default_factory=list)
    edges: list[LayoutEdge] = field(default_factory=list)
    geometry: Optional[Rect] = None


@dataclass
class SemanticViews:
    graph: Group
    calendar: Group


@dataclass
class PipelineResult:
    source: InputCollection
    views: SemanticViews

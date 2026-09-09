from dataclasses import dataclass
from enum import Enum
from uuid import uuid4

from pydantic import BaseModel, Field


class DiffSide(str, Enum):
    OLD = "old"
    NEW = "new"


class DiffLineKind(str, Enum):
    CONTEXT = "context"
    ADDITION = "addition"
    DELETION = "deletion"
    HUNK = "hunk"


class DiffPresentation(str, Enum):
    UNIFIED = "unified"
    SPLIT = "split"


class CommentPlacement(str, Enum):
    INLINE = "inline"
    MARGIN = "margin"


class AnnotationAnchor(BaseModel):
    path: str
    side: DiffSide
    line_start: int
    line_end: int
    snapshot_commit_oid: str | None
    blob_oid: str
    selected_text: str
    context_before: list[str] = Field(default_factory=list)
    context_after: list[str] = Field(default_factory=list)


class ReviewComment(BaseModel):
    id: str = Field(default_factory=lambda: str(uuid4()))
    text: str = ""


class AnnotationThread(BaseModel):
    id: str = Field(default_factory=lambda: str(uuid4()))
    anchor: AnnotationAnchor
    comments: list[ReviewComment] = Field(
        default_factory=lambda: [ReviewComment()])


class ReviewDocument(BaseModel):
    version: int = 1
    repository_path: str
    base_revision: str
    base_commit_oid: str
    threads: list[AnnotationThread] = Field(default_factory=list)


@dataclass(frozen=True)
class DiffRow:
    kind: DiffLineKind
    old_line_number: int | None
    new_line_number: int | None
    old_text: str | None
    new_text: str | None
    header: str | None = None


@dataclass(frozen=True)
class DiffFile:
    display_path: str
    old_path: str
    new_path: str
    additions: int
    deletions: int
    rows: tuple[DiffRow, ...]


@dataclass(frozen=True)
class LayoutBlock:
    row_index: int | None
    thread_id: str | None
    top: int
    height: int


@dataclass(frozen=True)
class MarginPlacement:
    thread_id: str
    anchor_y: int
    top: int
    height: int

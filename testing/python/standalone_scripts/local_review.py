#!/usr/bin/env python

import argparse
import bisect
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from uuid import uuid4

import pygit2
from beartype import beartype
from beartype.typing import Callable, Sequence
from pydantic import BaseModel, Field
from PyQt6.QtCore import QModelIndex, QPoint, QRect, QSize, Qt, QTimer
from PyQt6.QtGui import (
    QAction,
    QCloseEvent,
    QColor,
    QFont,
    QFontMetrics,
    QMouseEvent,
    QPainter,
    QPaintEvent,
    QPen,
    QResizeEvent,
    QStandardItem,
    QStandardItemModel,
)
from PyQt6.QtWidgets import (
    QAbstractScrollArea,
    QApplication,
    QButtonGroup,
    QFrame,
    QHBoxLayout,
    QMainWindow,
    QPlainTextEdit,
    QPushButton,
    QSizePolicy,
    QSplitter,
    QTreeView,
    QVBoxLayout,
    QWidget,
)


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


@beartype
def decode_blob(blob: pygit2.Blob) -> str:
    return bytes(blob.data).decode("utf-8", errors="replace")


@beartype
def normalize_diff_content(content: str | bytes) -> str:
    match content:
        case bytes():
            value = content.decode("utf-8", errors="replace")
        case str():
            value = content
    return value.rstrip("\r\n")


@beartype
def resolve_commit(
    repository: pygit2.Repository,
    revision: str,
) -> pygit2.Commit:
    try:
        value = repository.revparse_single(revision)
    except KeyError as error:
        raise ValueError(
            f"Git revision {revision!r} does not exist in {repository.workdir!r}"
        ) from error

    try:
        return value.peel(pygit2.Commit)
    except ValueError as error:
        raise ValueError(
            f"Git revision {revision!r} does not resolve to a commit"
        ) from error


@beartype
def repository_head(repository: pygit2.Repository) -> pygit2.Commit:
    if repository.head_is_unborn:
        raise ValueError(
            f"Repository {repository.workdir!r} has an unborn HEAD")

    value = repository[repository.head.target]
    if not isinstance(value, pygit2.Commit):
        raise TypeError(
            f"Repository HEAD {repository.head.target} is not a commit")

    return value


@beartype
def tree_blob(
    repository: pygit2.Repository,
    commit: pygit2.Commit,
    path: str,
) -> pygit2.Blob:
    try:
        entry = commit.tree[path]
    except KeyError as error:
        raise ValueError(
            f"Path {path!r} does not exist in commit {commit.id}") from error

    value = repository[entry.id]
    if not isinstance(value, pygit2.Blob):
        raise TypeError(
            f"Path {path!r} in commit {commit.id} is not a file blob")

    return value


@beartype
def line_context(
    lines: Sequence[str],
    line_start: int,
    line_end: int,
) -> tuple[list[str], list[str]]:
    before_start = max(0, line_start - 4)
    before = list(lines[before_start:line_start - 1])
    after = list(lines[line_end:line_end + 3])
    return before, after


@beartype
class AnnotationStore:

    def __init__(
        self,
        path: Path,
        document: ReviewDocument,
    ) -> None:
        self.path = path
        self.document = document

    def save(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        temporary_path = self.path.with_suffix(f"{self.path.suffix}.temporary")
        data = self.document.model_dump_json(indent=2)
        temporary_path.write_text(f"{data}\n", encoding="utf-8")
        temporary_path.replace(self.path)

    def find_thread(self, thread_id: str) -> AnnotationThread:
        for thread in self.document.threads:
            if thread.id == thread_id:
                return thread

        raise ValueError(f"Annotation thread {thread_id!r} does not exist")

    def find_comment(
        self,
        thread_id: str,
        comment_id: str,
    ) -> ReviewComment:
        thread = self.find_thread(thread_id)
        for comment in thread.comments:
            if comment.id == comment_id:
                return comment

        raise ValueError(
            f"Comment {comment_id!r} does not exist in thread {thread_id!r}")

    def update_comment(
        self,
        thread_id: str,
        comment_id: str,
        text: str,
    ) -> None:
        comment = self.find_comment(thread_id, comment_id)
        comment.text = text
        self.save()

    def append_reply(self, thread_id: str) -> ReviewComment:
        thread = self.find_thread(thread_id)
        comment = ReviewComment()
        thread.comments.append(comment)
        self.save()
        return comment

    def remove_thread(self, thread_id: str) -> None:
        matching = [
            index for index, thread in enumerate(self.document.threads)
            if thread.id == thread_id
        ]
        if not matching:
            raise ValueError(f"Annotation thread {thread_id!r} does not exist")

        self.document.threads.pop(matching[0])
        self.save()


@beartype
class GitReviewRepository:

    def __init__(
        self,
        path: Path,
        base_revision: str,
        stored_base_oid: str | None,
    ) -> None:
        repository_path = pygit2.discover_repository(str(path))
        if repository_path is None:
            raise ValueError(f"Path {path} is not inside a Git repository")

        self.repository = pygit2.Repository(repository_path)
        if self.repository.workdir is None:
            raise ValueError(
                f"Repository {repository_path!r} has no working tree")

        self.worktree_path = Path(self.repository.workdir).resolve()
        self.base_revision = base_revision
        head = repository_head(self.repository)

        if stored_base_oid is None:
            configured_base = resolve_commit(
                self.repository,
                base_revision,
            )
            merge_base_oid = self.repository.merge_base(
                configured_base.id,
                head.id,
            )
            if merge_base_oid is None:
                raise ValueError(
                    f"Revision {base_revision!r} and HEAD {head.id} have no merge base"
                )
            self.base_commit_oid = merge_base_oid
        else:
            self.base_commit_oid = pygit2.Oid(hex=stored_base_oid)

        base_value = self.repository[self.base_commit_oid]
        if not isinstance(base_value, pygit2.Commit):
            raise TypeError(
                f"Stored base object {self.base_commit_oid} is not a commit")

        self.base_commit = base_value
        self.head_oid = head.id

    def refresh_head(self) -> bool:
        head = repository_head(self.repository)
        if head.id == self.head_oid:
            return False

        self.head_oid = head.id
        return True

    def status_is_current(self, path: str) -> bool:
        status = self.repository.status_file(path)
        return status == pygit2.enums.FileStatus.CURRENT

    def load_diff(self) -> list[DiffFile]:
        flags = (pygit2.enums.DiffOption.INCLUDE_UNTRACKED
                 | pygit2.enums.DiffOption.RECURSE_UNTRACKED_DIRS)
        diff = self.repository.diff(
            self.base_commit.tree,
            None,
            flags=flags,
            context_lines=3,
            interhunk_lines=0,
        )

        files: list[DiffFile] = []
        for patch in diff:
            rows: list[DiffRow] = []
            additions = 0
            deletions = 0

            for hunk in patch.hunks:
                rows.append(
                    DiffRow(
                        kind=DiffLineKind.HUNK,
                        old_line_number=None,
                        new_line_number=None,
                        old_text=None,
                        new_text=None,
                        header=hunk.header.rstrip("\r\n"),
                    ))

                for line in hunk.lines:
                    text = normalize_diff_content(line.content)
                    match line.origin:
                        case "+":
                            kind = DiffLineKind.ADDITION
                            additions += 1
                            old_text = None
                            new_text = text
                        case "-":
                            kind = DiffLineKind.DELETION
                            deletions += 1
                            old_text = text
                            new_text = None
                        case " ":
                            kind = DiffLineKind.CONTEXT
                            old_text = text
                            new_text = text
                        case _:
                            continue

                    old_line_number = line.old_lineno if 0 < line.old_lineno else None
                    new_line_number = line.new_lineno if 0 < line.new_lineno else None
                    rows.append(
                        DiffRow(
                            kind=kind,
                            old_line_number=old_line_number,
                            new_line_number=new_line_number,
                            old_text=old_text,
                            new_text=new_text,
                        ))

            old_path = patch.delta.old_file.path
            new_path = patch.delta.new_file.path
            display_path = new_path or old_path
            files.append(
                DiffFile(
                    display_path=display_path,
                    old_path=old_path,
                    new_path=new_path,
                    additions=additions,
                    deletions=deletions,
                    rows=tuple(rows),
                ))

        return sorted(files, key=lambda value: value.display_path)

    def source_for_anchor(
        self,
        diff_file: DiffFile,
        side: DiffSide,
    ) -> tuple[str, str, str | None, list[str]]:
        match side:
            case DiffSide.OLD:
                path = diff_file.old_path
                blob = tree_blob(
                    self.repository,
                    self.base_commit,
                    path,
                )
                text = decode_blob(blob)
                return path, str(blob.id), str(
                    self.base_commit.id), (text.splitlines())
            case DiffSide.NEW:
                path = diff_file.new_path
                worktree_file = self.worktree_path / path
                if worktree_file.exists():
                    data = worktree_file.read_bytes()
                    blob_oid = str(pygit2.hash(data))
                    snapshot_oid = (str(self.head_oid)
                                    if self.status_is_current(path) else None)
                    return (
                        path,
                        blob_oid,
                        snapshot_oid,
                        data.decode(
                            "utf-8",
                            errors="replace",
                        ).splitlines(),
                    )

                head = repository_head(self.repository)
                blob = tree_blob(self.repository, head, path)
                return (
                    path,
                    str(blob.id),
                    str(head.id),
                    decode_blob(blob).splitlines(),
                )

    def reconcile_pending(
        self,
        document: ReviewDocument,
    ) -> bool:
        head = repository_head(self.repository)
        changed = False

        for thread in document.threads:
            anchor = thread.anchor
            if anchor.snapshot_commit_oid is not None:
                continue
            if anchor.side != DiffSide.NEW:
                continue

            try:
                blob = tree_blob(
                    self.repository,
                    head,
                    anchor.path,
                )
            except ValueError:
                continue

            if str(blob.id) == anchor.blob_oid:
                anchor.snapshot_commit_oid = str(head.id)
                changed = True
                continue

            lines = decode_blob(blob).splitlines()
            selected_lines = anchor.selected_text.splitlines()
            candidate_lines = self.find_relocation_candidates(
                lines,
                selected_lines,
            )
            relocated = self.select_relocation(
                lines,
                candidate_lines,
                selected_lines,
                anchor.context_before,
                anchor.context_after,
            )
            if relocated is None:
                continue

            anchor.line_start = relocated
            anchor.line_end = relocated + len(selected_lines) - 1
            anchor.blob_oid = str(blob.id)
            anchor.snapshot_commit_oid = str(head.id)
            before, after = line_context(
                lines,
                anchor.line_start,
                anchor.line_end,
            )
            anchor.context_before = before
            anchor.context_after = after
            changed = True

        return changed

    def find_relocation_candidates(
        self,
        lines: Sequence[str],
        selected_lines: Sequence[str],
    ) -> list[int]:
        if not selected_lines:
            return []

        width = len(selected_lines)
        result: list[int] = []
        limit = len(lines) - width + 1
        for index in range(max(0, limit)):
            if list(lines[index:index + width]) == list(selected_lines):
                result.append(index + 1)

        return result

    def select_relocation(
        self,
        lines: Sequence[str],
        candidates: Sequence[int],
        selected_lines: Sequence[str],
        context_before: Sequence[str],
        context_after: Sequence[str],
    ) -> int | None:
        if len(candidates) == 1:
            return candidates[0]
        if not candidates:
            return None

        scores: list[tuple[int, int]] = []
        width = len(selected_lines)
        for candidate in candidates:
            before_start = max(0, candidate - 1 - len(context_before))
            before = list(lines[before_start:candidate - 1])
            after_start = candidate - 1 + width
            after = list(lines[after_start:after_start + len(context_after)])
            score = 0
            for expected, actual in zip(
                    reversed(context_before),
                    reversed(before),
            ):
                if expected != actual:
                    break
                score += 1
            for expected, actual in zip(context_after, after):
                if expected != actual:
                    break
                score += 1
            scores.append((score, candidate))

        scores.sort(reverse=True)
        if len(scores) == 1:
            return scores[0][1]
        if scores[1][0] < scores[0][0]:
            return scores[0][1]
        return None


@beartype
class FileTreeModel(QStandardItemModel):
    PATH_ROLE = int(Qt.ItemDataRole.UserRole) + 1

    def __init__(self) -> None:
        super().__init__()
        self.setHorizontalHeaderLabels(["File", "+", "-"])

    def replace_files(self, files: Sequence[DiffFile]) -> None:
        self.clear()
        self.setHorizontalHeaderLabels(["File", "+", "-"])
        root = self.invisibleRootItem()
        directories: dict[str, tuple[QStandardItem, QStandardItem,
                                     QStandardItem]] = {}

        for diff_file in files:
            parts = Path(diff_file.display_path).parts
            parent = root
            accumulated: list[str] = []

            for part in parts[:-1]:
                accumulated.append(part)
                directory_path = "/".join(accumulated)
                existing = directories.get(directory_path)
                if existing is None:
                    name_item = QStandardItem(part)
                    additions_item = QStandardItem("0")
                    deletions_item = QStandardItem("0")
                    name_item.setEditable(False)
                    additions_item.setEditable(False)
                    deletions_item.setEditable(False)
                    parent.appendRow(
                        [name_item, additions_item, deletions_item])
                    existing = (
                        name_item,
                        additions_item,
                        deletions_item,
                    )
                    directories[directory_path] = existing

                additions_item = existing[1]
                deletions_item = existing[2]
                additions_item.setText(
                    str(int(additions_item.text()) + diff_file.additions))
                deletions_item.setText(
                    str(int(deletions_item.text()) + diff_file.deletions))
                parent = existing[0]

            name_item = QStandardItem(parts[-1])
            additions_item = QStandardItem(str(diff_file.additions))
            deletions_item = QStandardItem(str(diff_file.deletions))
            name_item.setData(
                diff_file.display_path,
                self.PATH_ROLE,
            )
            name_item.setEditable(False)
            additions_item.setEditable(False)
            deletions_item.setEditable(False)
            parent.appendRow([name_item, additions_item, deletions_item])


@beartype
class ThreadWidget(QFrame):

    def __init__(
        self,
        thread: AnnotationThread,
        update_callback: Callable[[str, str, str], None],
        reply_callback: Callable[[str], None],
        remove_callback: Callable[[str], None],
        layout_callback: Callable[[], None],
        parent: QWidget,
    ) -> None:
        super().__init__(parent)
        self.thread = thread
        self.update_callback = update_callback
        self.reply_callback = reply_callback
        self.remove_callback = remove_callback
        self.layout_callback = layout_callback

        self.setFrameShape(QFrame.Shape.StyledPanel)
        self.setAutoFillBackground(True)
        self.setSizePolicy(
            QSizePolicy.Policy.Fixed,
            QSizePolicy.Policy.Fixed,
        )

        layout = QVBoxLayout(self)
        layout.setContentsMargins(8, 8, 8, 8)
        layout.setSpacing(6)

        for comment in thread.comments:
            editor = QPlainTextEdit(self)
            editor.setPlaceholderText("Write a review comment")
            editor.setPlainText(comment.text)
            editor.setFixedHeight(self.editor_height(comment.text))
            editor.textChanged.connect(
                lambda editor=editor, comment_id=comment.id: self.
                editor_changed(editor, comment_id))
            layout.addWidget(editor)

        action_layout = QHBoxLayout()
        action_layout.addStretch(1)

        reply_button = QPushButton("Reply", self)
        reply_button.clicked.connect(self.reply_clicked)
        action_layout.addWidget(reply_button)

        remove_button = QPushButton("Delete thread", self)
        remove_button.clicked.connect(self.remove_clicked)
        action_layout.addWidget(remove_button)

        layout.addLayout(action_layout)

    def editor_height(self, text: str) -> int:
        line_count = max(1, len(text.splitlines()))
        return max(72, min(220, 34 + line_count * 20))

    def preferred_height(self) -> int:
        editor_height = sum(
            self.editor_height(comment.text)
            for comment in self.thread.comments)
        spacing = 6 * len(self.thread.comments)
        return 54 + editor_height + spacing

    def editor_changed(
        self,
        editor: QPlainTextEdit,
        comment_id: str,
    ) -> None:
        text = editor.toPlainText()
        editor.setFixedHeight(self.editor_height(text))
        self.update_callback(
            self.thread.id,
            comment_id,
            text,
        )
        self.setFixedHeight(self.preferred_height())
        self.layout_callback()

    def reply_clicked(self, checked: bool) -> None:
        self.reply_callback(self.thread.id)

    def remove_clicked(self, checked: bool) -> None:
        self.remove_callback(self.thread.id)


@beartype
class DiffView(QAbstractScrollArea):
    line_height = 24
    hunk_height = 30
    line_number_width = 58
    margin_gap = 10
    margin_width = 410

    def __init__(
        self,
        repository: GitReviewRepository,
        store: AnnotationStore,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self.repository = repository
        self.store = store
        self.diff_file: DiffFile | None = None
        self.presentation = DiffPresentation.UNIFIED
        self.placement = CommentPlacement.INLINE
        self.blocks: list[LayoutBlock] = []
        self.block_tops: list[int] = []
        self.row_tops: dict[int, int] = {}
        self.margin_placements: dict[str, MarginPlacement] = {}
        self.visible_widgets: dict[str, ThreadWidget] = {}
        self.selected_row: int | None = None
        self.selected_side = DiffSide.NEW
        self.hovered_row: int | None = None

        self.font_value = QFont("monospace")
        self.font_value.setStyleHint(QFont.StyleHint.Monospace)
        self.viewport().setFont(self.font_value)
        self.viewport().setMouseTracking(True)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOn)
        self.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.verticalScrollBar().valueChanged.connect(self.scroll_changed)

    def sizeHint(self) -> QSize:
        return QSize(1100, 800)

    def set_diff_file(self, diff_file: DiffFile | None) -> None:
        self.diff_file = diff_file
        self.selected_row = None
        self.hovered_row = None
        self.verticalScrollBar().setValue(0)
        self.rebuild_layout()

    def set_presentation(
        self,
        presentation: DiffPresentation,
    ) -> None:
        self.presentation = presentation
        self.rebuild_layout()

    def set_placement(self, placement: CommentPlacement) -> None:
        self.placement = placement
        self.rebuild_layout()

    def threads_for_file(self) -> list[AnnotationThread]:
        if self.diff_file is None:
            return []

        paths = {
            self.diff_file.old_path,
            self.diff_file.new_path,
        }
        return [
            thread for thread in self.store.document.threads
            if thread.anchor.path in paths
        ]

    def matching_threads(
        self,
        row: DiffRow,
    ) -> list[AnnotationThread]:
        result: list[AnnotationThread] = []
        for thread in self.threads_for_file():
            anchor = thread.anchor
            match anchor.side:
                case DiffSide.OLD:
                    line_number = row.old_line_number
                case DiffSide.NEW:
                    line_number = row.new_line_number

            if line_number is None:
                continue
            if anchor.line_start <= line_number <= anchor.line_end:
                result.append(thread)

        return result

    def thread_height(self, thread: AnnotationThread) -> int:
        editor_height = sum(
            max(
                72,
                min(
                    220,
                    34 + max(1, len(comment.text.splitlines())) * 20,
                ),
            ) for comment in thread.comments)
        return 54 + editor_height + 6 * len(thread.comments)

    def rebuild_layout(self) -> None:
        self.blocks.clear()
        self.block_tops.clear()
        self.row_tops.clear()
        self.margin_placements.clear()

        if self.diff_file is None:
            self.verticalScrollBar().setRange(0, 0)
            self.update_visible_widgets()
            self.viewport().update()
            return

        top = 0
        for row_index, row in enumerate(self.diff_file.rows):
            height = (self.hunk_height
                      if row.kind == DiffLineKind.HUNK else self.line_height)
            self.row_tops[row_index] = top
            self.blocks.append(
                LayoutBlock(
                    row_index=row_index,
                    thread_id=None,
                    top=top,
                    height=height,
                ))
            self.block_tops.append(top)
            top += height

            if self.placement == CommentPlacement.INLINE:
                for thread in self.matching_threads(row):
                    thread_height = self.thread_height(thread)
                    self.blocks.append(
                        LayoutBlock(
                            row_index=None,
                            thread_id=thread.id,
                            top=top,
                            height=thread_height + self.margin_gap,
                        ))
                    self.block_tops.append(top)
                    top += thread_height + self.margin_gap

        if self.placement == CommentPlacement.MARGIN:
            margin_top = 0
            placed: set[str] = set()
            for row_index, row in enumerate(self.diff_file.rows):
                for thread in self.matching_threads(row):
                    if thread.id in placed:
                        continue
                    placed.add(thread.id)
                    anchor_y = self.row_tops[row_index]
                    thread_height = self.thread_height(thread)
                    thread_top = max(anchor_y, margin_top)
                    self.margin_placements[thread.id] = MarginPlacement(
                        thread_id=thread.id,
                        anchor_y=anchor_y,
                        top=thread_top,
                        height=thread_height,
                    )
                    margin_top = thread_top + thread_height + self.margin_gap
            top = max(top, margin_top)

        maximum = max(0, top - self.viewport().height())
        self.verticalScrollBar().setRange(0, maximum)
        self.verticalScrollBar().setPageStep(self.viewport().height())
        self.update_visible_widgets()
        self.viewport().update()

    def thread_by_id(self, thread_id: str) -> AnnotationThread:
        return self.store.find_thread(thread_id)

    def update_visible_widgets(self) -> None:
        scroll = self.verticalScrollBar().value()
        viewport_height = self.viewport().height()
        visible_ids: set[str] = set()
        rectangles: dict[str, QRect] = {}

        if self.placement == CommentPlacement.INLINE:
            for block in self.blocks:
                if block.thread_id is None:
                    continue
                viewport_top = block.top - scroll
                if viewport_top + block.height < 0:
                    continue
                if viewport_height < viewport_top:
                    continue

                width = max(
                    200,
                    self.viewport().width() - self.line_number_width - 24,
                )
                rectangles[block.thread_id] = QRect(
                    self.line_number_width + 12,
                    viewport_top + 4,
                    width,
                    block.height - self.margin_gap,
                )
                visible_ids.add(block.thread_id)
        else:
            left = max(
                0,
                self.viewport().width() - self.margin_width,
            )
            width = max(
                200,
                self.viewport().width() - left - 10,
            )
            for placement in self.margin_placements.values():
                viewport_top = placement.top - scroll
                if viewport_top + placement.height < 0:
                    continue
                if viewport_height < viewport_top:
                    continue

                rectangles[placement.thread_id] = QRect(
                    left + 5,
                    viewport_top,
                    width,
                    placement.height,
                )
                visible_ids.add(placement.thread_id)

        stale_ids = set(self.visible_widgets) - visible_ids
        for thread_id in stale_ids:
            widget = self.visible_widgets.pop(thread_id)
            widget.hide()
            widget.deleteLater()

        for thread_id in visible_ids:
            widget = self.visible_widgets.get(thread_id)
            if widget is None:
                thread = self.thread_by_id(thread_id)
                widget = ThreadWidget(
                    thread=thread,
                    update_callback=self.update_comment,
                    reply_callback=self.append_reply,
                    remove_callback=self.remove_thread,
                    layout_callback=self.rebuild_layout,
                    parent=self.viewport(),
                )
                self.visible_widgets[thread_id] = widget
                widget.show()

            rectangle = rectangles[thread_id]
            widget.setGeometry(rectangle)
            widget.raise_()

    def update_comment(
        self,
        thread_id: str,
        comment_id: str,
        text: str,
    ) -> None:
        self.store.update_comment(
            thread_id,
            comment_id,
            text,
        )

    def append_reply(self, thread_id: str) -> None:
        self.store.append_reply(thread_id)
        widget = self.visible_widgets.pop(thread_id, None)
        if widget is not None:
            widget.hide()
            widget.deleteLater()
        self.rebuild_layout()

    def remove_thread(self, thread_id: str) -> None:
        self.store.remove_thread(thread_id)
        widget = self.visible_widgets.pop(thread_id, None)
        if widget is not None:
            widget.hide()
            widget.deleteLater()
        self.rebuild_layout()

    def scroll_changed(self, value: int) -> None:
        self.update_visible_widgets()
        self.viewport().update()

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self.rebuild_layout()

    def block_at_viewport_y(self, viewport_y: int) -> LayoutBlock | None:
        if not self.block_tops:
            return None

        absolute_y = viewport_y + self.verticalScrollBar().value()
        index = bisect.bisect_right(self.block_tops, absolute_y) - 1
        if index < 0:
            return None
        if len(self.blocks) <= index:
            return None

        block = self.blocks[index]
        if block.top + block.height <= absolute_y:
            return None
        return block

    def side_at_x(self, x: int) -> DiffSide:
        if self.presentation == DiffPresentation.UNIFIED:
            if self.selected_row is not None and self.diff_file is not None:
                row = self.diff_file.rows[self.selected_row]
                if row.kind == DiffLineKind.DELETION:
                    return DiffSide.OLD
            return DiffSide.NEW

        code_width = self.code_width()
        if x < code_width // 2:
            return DiffSide.OLD
        return DiffSide.NEW

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        block = self.block_at_viewport_y(int(event.position().y()))
        if block is None or block.row_index is None:
            return

        row = self.diff_file.rows[block.row_index] if self.diff_file else None
        if row is None or row.kind == DiffLineKind.HUNK:
            return

        self.selected_row = block.row_index
        self.selected_side = self.side_at_x(int(event.position().x()))

        if int(event.position().x()) < 30:
            self.add_comment_for_selection()

        self.viewport().update()

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        block = self.block_at_viewport_y(int(event.position().y()))
        hovered = (block.row_index if block is not None
                   and block.row_index is not None else None)
        if hovered != self.hovered_row:
            self.hovered_row = hovered
            self.viewport().update()

    def leaveEvent(self, event) -> None:
        self.hovered_row = None
        self.viewport().update()

    def add_comment_for_selection(self) -> None:
        if self.diff_file is None or self.selected_row is None:
            return

        row = self.diff_file.rows[self.selected_row]
        side = self.selected_side
        match side:
            case DiffSide.OLD:
                line_number = row.old_line_number
            case DiffSide.NEW:
                line_number = row.new_line_number

        if line_number is None:
            side = DiffSide.NEW if row.new_line_number is not None else DiffSide.OLD
            line_number = (row.new_line_number
                           if side == DiffSide.NEW else row.old_line_number)

        if line_number is None:
            return

        path, blob_oid, snapshot_oid, lines = self.repository.source_for_anchor(
            self.diff_file,
            side,
        )
        if len(lines) < line_number:
            raise ValueError(
                f"Line {line_number} is outside {path!r}, which has {len(lines)} lines"
            )

        selected_text = lines[line_number - 1]
        before, after = line_context(
            lines,
            line_number,
            line_number,
        )
        thread = AnnotationThread(anchor=AnnotationAnchor(
            path=path,
            side=side,
            line_start=line_number,
            line_end=line_number,
            snapshot_commit_oid=snapshot_oid,
            blob_oid=blob_oid,
            selected_text=selected_text,
            context_before=before,
            context_after=after,
        ))
        self.store.document.threads.append(thread)
        self.store.save()
        self.rebuild_layout()

    def code_width(self) -> int:
        if self.placement == CommentPlacement.MARGIN:
            return max(
                300,
                self.viewport().width() - self.margin_width,
            )
        return self.viewport().width()

    def paintEvent(self, event: QPaintEvent) -> None:
        painter = QPainter(self.viewport())
        painter.setRenderHint(
            QPainter.RenderHint.TextAntialiasing,
            True,
        )
        palette = self.viewport().palette()
        painter.fillRect(
            self.viewport().rect(),
            palette.base(),
        )
        painter.setFont(self.font_value)

        if self.diff_file is None:
            painter.setPen(palette.text().color())
            painter.drawText(
                self.viewport().rect(),
                Qt.AlignmentFlag.AlignCenter,
                "Select a changed file",
            )
            return

        scroll = self.verticalScrollBar().value()
        viewport_height = self.viewport().height()
        if self.block_tops:
            index = max(
                0,
                bisect.bisect_right(self.block_tops, scroll) - 1,
            )
        else:
            index = 0

        while index < len(self.blocks):
            block = self.blocks[index]
            viewport_top = block.top - scroll
            if viewport_height < viewport_top:
                break
            if block.row_index is not None:
                self.paint_row(
                    painter,
                    block.row_index,
                    viewport_top,
                    block.height,
                )
            index += 1

        if self.placement == CommentPlacement.MARGIN:
            self.paint_margin_connectors(painter, scroll)

    def paint_row(
        self,
        painter: QPainter,
        row_index: int,
        top: int,
        height: int,
    ) -> None:
        if self.diff_file is None:
            return

        row = self.diff_file.rows[row_index]
        code_width = self.code_width()
        rectangle = QRect(0, top, code_width, height)
        palette = self.viewport().palette()
        dark = palette.base().color().lightness() < 128

        if row.kind == DiffLineKind.HUNK:
            color = QColor(44, 62, 80) if dark else QColor(221, 235, 247)
            painter.fillRect(rectangle, color)
            painter.setPen(palette.text().color())
            painter.drawText(
                rectangle.adjusted(10, 0, -10, 0),
                Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                row.header or "",
            )
            return

        if row_index == self.selected_row:
            selection = palette.highlight().color()
            selection.setAlpha(70)
            painter.fillRect(rectangle, selection)
        else:
            match row.kind:
                case DiffLineKind.ADDITION:
                    color = QColor(25, 60, 40) if dark else QColor(
                        230, 255, 237)
                    painter.fillRect(rectangle, color)
                case DiffLineKind.DELETION:
                    color = QColor(72, 35, 38) if dark else QColor(
                        255, 235, 233)
                    painter.fillRect(rectangle, color)
                case DiffLineKind.CONTEXT:
                    pass

        if row_index == self.hovered_row:
            painter.setPen(palette.highlight().color())
            painter.drawEllipse(QRect(6, top + 3, 18, 18))
            painter.drawLine(11, top + 12, 19, top + 12)
            painter.drawLine(15, top + 8, 15, top + 16)

        match self.presentation:
            case DiffPresentation.UNIFIED:
                self.paint_unified_row(
                    painter,
                    row,
                    top,
                    height,
                    code_width,
                )
            case DiffPresentation.SPLIT:
                self.paint_split_row(
                    painter,
                    row,
                    top,
                    height,
                    code_width,
                )

    def paint_unified_row(
        self,
        painter: QPainter,
        row: DiffRow,
        top: int,
        height: int,
        width: int,
    ) -> None:
        palette = self.viewport().palette()
        painter.setPen(palette.mid().color())
        number_rectangle = QRect(28, top, 94, height)
        old_number = str(
            row.old_line_number) if row.old_line_number is not None else ""
        new_number = str(
            row.new_line_number) if row.new_line_number is not None else ""
        painter.drawText(
            number_rectangle.adjusted(0, 0, -48, 0),
            Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
            old_number,
        )
        painter.drawText(
            number_rectangle.adjusted(48, 0, 0, 0),
            Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
            new_number,
        )

        match row.kind:
            case DiffLineKind.ADDITION:
                marker = "+"
                text = row.new_text or ""
            case DiffLineKind.DELETION:
                marker = "-"
                text = row.old_text or ""
            case DiffLineKind.CONTEXT:
                marker = " "
                text = row.new_text or ""

        painter.setPen(palette.text().color())
        painter.drawText(
            QRect(128, top, 16, height),
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            marker,
        )
        metrics = QFontMetrics(self.font_value)
        visible_text = metrics.elidedText(
            text.expandtabs(4),
            Qt.TextElideMode.ElideRight,
            max(0, width - 152),
        )
        painter.drawText(
            QRect(146, top, max(0, width - 152), height),
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            visible_text,
        )

    def paint_split_row(
        self,
        painter: QPainter,
        row: DiffRow,
        top: int,
        height: int,
        width: int,
    ) -> None:
        half = width // 2
        palette = self.viewport().palette()
        painter.setPen(palette.mid().color())
        painter.drawLine(half, top, half, top + height)

        self.paint_split_cell(
            painter,
            QRect(0, top, half, height),
            row.old_line_number,
            row.old_text,
            row.kind == DiffLineKind.DELETION,
            "-",
        )
        self.paint_split_cell(
            painter,
            QRect(half, top, width - half, height),
            row.new_line_number,
            row.new_text,
            row.kind == DiffLineKind.ADDITION,
            "+",
        )

    def paint_split_cell(
        self,
        painter: QPainter,
        rectangle: QRect,
        line_number: int | None,
        text: str | None,
        changed: bool,
        marker: str,
    ) -> None:
        palette = self.viewport().palette()
        number_width = self.line_number_width
        painter.setPen(palette.mid().color())
        painter.drawText(
            QRect(
                rectangle.left() + 26,
                rectangle.top(),
                number_width - 30,
                rectangle.height(),
            ),
            Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
            str(line_number) if line_number is not None else "",
        )

        if text is None:
            return

        painter.setPen(palette.text().color())
        painter.drawText(
            QRect(
                rectangle.left() + number_width + 4,
                rectangle.top(),
                14,
                rectangle.height(),
            ),
            Qt.AlignmentFlag.AlignVCenter,
            marker if changed else " ",
        )
        available = rectangle.width() - number_width - 24
        metrics = QFontMetrics(self.font_value)
        visible_text = metrics.elidedText(
            text.expandtabs(4),
            Qt.TextElideMode.ElideRight,
            max(0, available),
        )
        painter.drawText(
            QRect(
                rectangle.left() + number_width + 20,
                rectangle.top(),
                max(0, available),
                rectangle.height(),
            ),
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            visible_text,
        )

    def paint_margin_connectors(
        self,
        painter: QPainter,
        scroll: int,
    ) -> None:
        code_width = self.code_width()
        palette = self.viewport().palette()
        pen = QPen(palette.mid().color())
        pen.setWidth(1)
        painter.setPen(pen)

        for placement in self.margin_placements.values():
            anchor_y = placement.anchor_y - scroll + self.line_height // 2
            thread_y = placement.top - scroll + 16
            if anchor_y < 0 and thread_y < 0:
                continue
            if (self.viewport().height() < anchor_y
                    and self.viewport().height() < thread_y):
                continue

            painter.drawLine(
                code_width - 8,
                anchor_y,
                code_width + 16,
                anchor_y,
            )
            painter.drawLine(
                code_width + 16,
                anchor_y,
                code_width + 16,
                thread_y,
            )
            painter.drawLine(
                code_width + 16,
                thread_y,
                code_width + 28,
                thread_y,
            )


@beartype
class ReviewWindow(QMainWindow):

    def __init__(
        self,
        repository: GitReviewRepository,
        store: AnnotationStore,
    ) -> None:
        super().__init__()
        self.repository = repository
        self.store = store
        self.files: list[DiffFile] = []
        self.files_by_path: dict[str, DiffFile] = {}

        self.setWindowTitle(f"Local Review — {repository.worktree_path}")
        self.resize(1500, 900)

        central = QWidget(self)
        central_layout = QVBoxLayout(central)
        central_layout.setContentsMargins(0, 0, 0, 0)
        central_layout.setSpacing(0)

        controls = QHBoxLayout()
        controls.setContentsMargins(8, 6, 8, 6)

        presentation_group = QButtonGroup(self)
        presentation_group.setExclusive(True)

        unified_button = QPushButton("Unified", central)
        unified_button.setCheckable(True)
        unified_button.setChecked(True)
        unified_button.clicked.connect(self.unified_clicked)
        presentation_group.addButton(unified_button)
        controls.addWidget(unified_button)

        split_button = QPushButton("Split", central)
        split_button.setCheckable(True)
        split_button.clicked.connect(self.split_clicked)
        presentation_group.addButton(split_button)
        controls.addWidget(split_button)

        placement_group = QButtonGroup(self)
        placement_group.setExclusive(True)

        inline_button = QPushButton("Inline comments", central)
        inline_button.setCheckable(True)
        inline_button.setChecked(True)
        inline_button.clicked.connect(self.inline_clicked)
        placement_group.addButton(inline_button)
        controls.addWidget(inline_button)

        margin_button = QPushButton("Margin comments", central)
        margin_button.setCheckable(True)
        margin_button.clicked.connect(self.margin_clicked)
        placement_group.addButton(margin_button)
        controls.addWidget(margin_button)

        add_comment_button = QPushButton("Add comment", central)
        add_comment_button.clicked.connect(self.add_comment_clicked)
        controls.addWidget(add_comment_button)
        controls.addStretch(1)

        central_layout.addLayout(controls)

        splitter = QSplitter(Qt.Orientation.Horizontal, central)
        self.tree_model = FileTreeModel()
        self.tree_view = QTreeView(splitter)
        self.tree_view.setModel(self.tree_model)
        self.tree_view.setAlternatingRowColors(True)
        self.tree_view.setUniformRowHeights(True)
        self.tree_view.setColumnWidth(0, 280)
        self.tree_view.setColumnWidth(1, 55)
        self.tree_view.setColumnWidth(2, 55)
        self.tree_view.currentChanged = self.tree_view.currentChanged

        self.diff_view = DiffView(
            repository,
            store,
            splitter,
        )
        splitter.addWidget(self.tree_view)
        splitter.addWidget(self.diff_view)
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)
        splitter.setSizes([360, 1140])
        central_layout.addWidget(splitter)

        self.setCentralWidget(central)
        self.reload_files()

        selection_model = self.tree_view.selectionModel()
        selection_model.currentChanged.connect(self.file_selection_changed)

        self.head_timer = QTimer(self)
        self.head_timer.setInterval(1500)
        self.head_timer.timeout.connect(self.check_head)
        self.head_timer.start()

        add_action = QAction(self)
        add_action.setShortcut("Ctrl+Enter")
        add_action.triggered.connect(self.add_comment_action)
        self.addAction(add_action)

    def reload_files(self) -> None:
        selected_path = (self.diff_view.diff_file.display_path
                         if self.diff_view.diff_file is not None else None)
        self.files = self.repository.load_diff()
        self.files_by_path = {
            diff_file.display_path: diff_file
            for diff_file in self.files
        }
        self.tree_model.replace_files(self.files)
        self.tree_view.expandAll()

        if selected_path is not None:
            diff_file = self.files_by_path.get(selected_path)
            self.diff_view.set_diff_file(diff_file)

    def file_selection_changed(
        self,
        current: QModelIndex,
        previous: QModelIndex,
    ) -> None:
        path = current.data(FileTreeModel.PATH_ROLE)
        if path is None:
            return

        diff_file = self.files_by_path.get(path)
        if diff_file is None:
            raise ValueError(f"Selected diff path {path!r} is not loaded")
        self.diff_view.set_diff_file(diff_file)

    def unified_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_presentation(DiffPresentation.UNIFIED)

    def split_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_presentation(DiffPresentation.SPLIT)

    def inline_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_placement(CommentPlacement.INLINE)

    def margin_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_placement(CommentPlacement.MARGIN)

    def add_comment_clicked(self, checked: bool) -> None:
        self.diff_view.add_comment_for_selection()

    def add_comment_action(self, checked: bool) -> None:
        self.diff_view.add_comment_for_selection()

    def check_head(self) -> None:
        if not self.repository.refresh_head():
            return

        if self.repository.reconcile_pending(self.store.document):
            self.store.save()

        self.reload_files()

    def closeEvent(self, event: QCloseEvent) -> None:
        self.store.save()
        event.accept()


@dataclass(frozen=True)
class Arguments:
    repository: Path
    annotations: Path
    base: str | None


@beartype
def parse_arguments() -> Arguments:
    parser = argparse.ArgumentParser(
        description="Review local Git changes with editable annotations")
    parser.add_argument(
        "repository",
        type=Path,
        help="Path inside the Git repository",
    )
    parser.add_argument(
        "annotations",
        type=Path,
        help="Path to the annotation JSON document",
    )
    parser.add_argument(
        "--base",
        help="Base revision, defaulting to master for new reviews",
    )
    namespace = parser.parse_args()
    return Arguments(
        repository=namespace.repository.resolve(),
        annotations=namespace.annotations.resolve(),
        base=namespace.base,
    )


@beartype
def load_review(
    arguments: Arguments, ) -> tuple[GitReviewRepository, AnnotationStore]:
    if arguments.annotations.exists():
        document = ReviewDocument.model_validate_json(
            arguments.annotations.read_text(encoding="utf-8"))
        if arguments.base is not None and arguments.base != document.base_revision:
            raise ValueError(
                f"Annotation document uses base revision "
                f"{document.base_revision!r}, but --base specified "
                f"{arguments.base!r}")

        repository = GitReviewRepository(
            arguments.repository,
            document.base_revision,
            document.base_commit_oid,
        )
        expected_path = str(repository.worktree_path)
        if Path(document.repository_path).resolve() != Path(expected_path):
            raise ValueError(
                f"Annotation document belongs to repository "
                f"{document.repository_path!r}, not {expected_path!r}")
    else:
        base_revision = arguments.base or "master"
        repository = GitReviewRepository(
            arguments.repository,
            base_revision,
            None,
        )
        document = ReviewDocument(
            repository_path=str(repository.worktree_path),
            base_revision=base_revision,
            base_commit_oid=str(repository.base_commit_oid),
        )

    store = AnnotationStore(arguments.annotations, document)
    store.save()
    return repository, store


@beartype
def main() -> int:
    arguments = parse_arguments()
    repository, store = load_review(arguments)
    application = QApplication(sys.argv)
    window = ReviewWindow(repository, store)
    window.show()
    return application.exec()


if __name__ == "__main__":
    sys.exit(main())

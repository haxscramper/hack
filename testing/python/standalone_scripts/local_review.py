#!/usr/bin/env python

from __future__ import annotations

import argparse
import difflib
import hashlib
from datetime import UTC, datetime
from enum import Enum
from pathlib import Path
from uuid import uuid4

from beartype import beartype
from beartype.typing import Any, Literal
from git import Repo
from pydantic import BaseModel, ConfigDict, Field, ValidationError
from PyQt6.QtCore import (
    QAbstractListModel,
    QAbstractTableModel,
    QModelIndex,
    QObject,
    QStandardPaths,
    Qt,
)
from PyQt6.QtGui import (
    QBrush,
    QColor,
    QFontDatabase,
    QStandardItem,
    QStandardItemModel,
)
from PyQt6.QtWidgets import (
    QApplication,
    QComboBox,
    QHeaderView,
    QLabel,
    QListView,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QPlainTextEdit,
    QSplitter,
    QTableView,
    QTreeView,
    QVBoxLayout,
    QWidget,
)


class ChangeKind(Enum):
    ADDED = "A"
    MODIFIED = "M"
    DELETED = "D"
    RENAMED = "R"
    COPIED = "C"
    TYPE_CHANGED = "T"

    @property
    def title(self) -> str:
        match self:
            case ChangeKind.ADDED:
                return "Added"
            case ChangeKind.MODIFIED:
                return "Modified"
            case ChangeKind.DELETED:
                return "Removed"
            case ChangeKind.RENAMED:
                return "Renamed"
            case ChangeKind.COPIED:
                return "Copied"
            case ChangeKind.TYPE_CHANGED:
                return "Type changed"


class CommentScope(Enum):
    SELECTED_LINE = "selected-line"
    CURRENT_FILE = "current-file"


class FileChange(BaseModel):
    model_config = ConfigDict(frozen=True)

    kind: ChangeKind
    old_path: str | None
    current_path: str | None

    @property
    def display_path(self) -> str:
        path = self.current_path if self.current_path is not None else self.old_path
        if path is None:
            raise RuntimeError(
                f"Change {self.kind.value} has neither an old nor current path"
            )
        return path


class FileContent(BaseModel):
    model_config = ConfigDict(frozen=True)

    lines: list[str]
    exists: bool
    binary: bool


class Annotation(BaseModel):
    model_config = ConfigDict(frozen=True)

    identifier: str
    path: str
    commit: str
    line: int
    line_text: str
    before: list[str]
    after: list[str]
    text: str
    created_at: datetime


class AnnotationDocument(BaseModel):
    version: Literal[1] = 1
    annotations: list[Annotation] = Field(default_factory=list)


class ResolvedAnnotation(BaseModel):
    model_config = ConfigDict(frozen=True)

    annotation: Annotation
    line: int


class CodeLine(BaseModel):
    model_config = ConfigDict(frozen=True)

    number: int
    text: str
    changed: bool
    annotation_count: int


@beartype
class AnnotationStore:

    def __init__(self, path: Path) -> None:
        self.path = path
        self.annotations = self.load()

    def load(self) -> list[Annotation]:
        if not self.path.exists():
            return []

        try:
            document = AnnotationDocument.model_validate_json(
                self.path.read_text(encoding="utf-8"))
        except ValidationError as error:
            raise ValueError(
                f"Annotation cache {self.path} contains invalid data: {error}"
            ) from error

        return list(document.annotations)

    def add(self, annotation: Annotation) -> None:
        self.annotations.append(annotation)
        self.save()

    def update(self, annotation: Annotation) -> None:
        for index, existing in enumerate(self.annotations):
            if existing.identifier == annotation.identifier:
                self.annotations[index] = annotation
                self.save()
                return

        raise ValueError(
            f"Cannot update annotation {annotation.identifier!r} because it is "
            f"not present in {self.path}")

    def save(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        document = AnnotationDocument(annotations=self.annotations)
        self.path.write_text(
            document.model_dump_json(indent=2) + "\n",
            encoding="utf-8",
        )


@beartype
class ReviewRepository:

    def __init__(
        self,
        repository_path: Path,
        target_reference: str,
        current_reference: str | None,
    ) -> None:
        self.repository_path = repository_path.resolve()
        self.repo = Repo(str(self.repository_path))

        if self.repo.bare:
            raise ValueError(
                f"Repository {self.repository_path} is bare and cannot display files"
            )

        self.target_reference = target_reference
        self.target_commit = self.repo.commit(target_reference)
        self.current_commit = self.repo.commit(current_reference or "HEAD")
        self.current_reference = current_reference or self.current_name()

        merge_bases = self.repo.merge_base(self.target_commit,
                                           self.current_commit)
        if len(merge_bases) == 0:
            raise ValueError(
                f"Target {target_reference!r} and current revision "
                f"{self.current_reference!r} have no common ancestor")

        self.base_commit = merge_bases[0]

    def current_name(self) -> str:
        if self.repo.head.is_detached:
            return self.repo.head.commit.hexsha[:12]
        return self.repo.active_branch.name

    def annotation_path(self) -> Path:
        cache_location = QStandardPaths.writableLocation(
            QStandardPaths.StandardLocation.CacheLocation)
        if cache_location == "":
            raise RuntimeError(
                f"Qt did not provide a cache directory for repository "
                f"{self.repository_path}")

        repository_key = hashlib.sha256(
            str(self.repository_path).encode("utf-8")).hexdigest()
        return Path(cache_location) / f"{repository_key}.json"

    def changes(self) -> list[FileChange]:
        result: list[FileChange] = []

        for difference in self.base_commit.diff(self.current_commit, M=True):
            kind = self.change_kind(difference.change_type)
            old_path = difference.a_path
            current_path = difference.b_path

            match kind:
                case ChangeKind.ADDED:
                    old_path = None
                case ChangeKind.DELETED:
                    current_path = None
                case ChangeKind.MODIFIED | ChangeKind.TYPE_CHANGED:
                    if old_path is None or current_path is None:
                        raise RuntimeError(
                            f"Change {kind.value} has incomplete paths: "
                            f"old={old_path!r}, current={current_path!r}")
                case ChangeKind.RENAMED | ChangeKind.COPIED:
                    if old_path is None or current_path is None:
                        raise RuntimeError(
                            f"Change {kind.value} has incomplete paths: "
                            f"old={old_path!r}, current={current_path!r}")

            result.append(
                FileChange(
                    kind=kind,
                    old_path=old_path,
                    current_path=current_path,
                ))

        return sorted(result, key=lambda change: change.display_path)

    def change_kind(self, value: str | None) -> ChangeKind:
        match value:
            case "A":
                return ChangeKind.ADDED
            case "M":
                return ChangeKind.MODIFIED
            case "D":
                return ChangeKind.DELETED
            case "R":
                return ChangeKind.RENAMED
            case "C":
                return ChangeKind.COPIED
            case "T":
                return ChangeKind.TYPE_CHANGED
            case _:
                raise ValueError(
                    f"Git returned unsupported change type {value!r}")

    def base_content(self, change: FileChange) -> FileContent:
        return self.read_content(self.base_commit, change.old_path)

    def current_content(self, change: FileChange) -> FileContent:
        return self.read_content(self.current_commit, change.current_path)

    def read_content(self, commit: Any, path: str | None) -> FileContent:
        if path is None:
            return FileContent(lines=[], exists=False, binary=False)

        try:
            item = commit.tree / path
        except KeyError:
            return FileContent(lines=[], exists=False, binary=False)

        if item.type != "blob":
            return FileContent(lines=[], exists=True, binary=True)

        content: bytes = item.data_stream.read()
        if b"\x00" in content:
            return FileContent(lines=[], exists=True, binary=True)

        text = content.decode("utf-8")
        return FileContent(
            lines=text.splitlines(),
            exists=True,
            binary=False,
        )

    def changed_lines(
        self,
        base_content: FileContent,
        current_content: FileContent,
    ) -> set[int]:
        if not current_content.exists or current_content.binary:
            return set()

        if not base_content.exists or base_content.binary:
            return set(range(1, len(current_content.lines) + 1))

        matcher = difflib.SequenceMatcher(
            None,
            base_content.lines,
            current_content.lines,
            autojunk=False,
        )
        result: set[int] = set()

        for operation, base_start, base_end, current_start, current_end in matcher.get_opcodes(
        ):
            match operation:
                case "replace" | "insert":
                    result.update(range(current_start + 1, current_end + 1))
                case "equal" | "delete":
                    pass
                case _:
                    raise RuntimeError(
                        f"Sequence matcher returned unsupported operation {operation!r} "
                        f"for ranges {base_start}:{base_end} and "
                        f"{current_start}:{current_end}")

        return result


@beartype
def resolve_annotation(
    annotation: Annotation,
    path: str,
    lines: list[str],
) -> ResolvedAnnotation | None:
    if annotation.path != path:
        return None

    candidates = [
        index for index, line_text in enumerate(lines)
        if line_text == annotation.line_text
    ]
    if len(candidates) == 0:
        return None

    best_index: int | None = None
    best_key: tuple[int, int, int] | None = None

    for candidate in candidates:
        score = 0

        for offset, expected in enumerate(reversed(annotation.before),
                                          start=1):
            source_index = candidate - offset
            if 0 <= source_index < len(
                    lines) and lines[source_index] == expected:
                score += 1

        for offset, expected in enumerate(annotation.after, start=1):
            source_index = candidate + offset
            if 0 <= source_index < len(
                    lines) and lines[source_index] == expected:
                score += 1

        distance = abs(candidate + 1 - annotation.line)
        key = (score, -distance, -candidate)

        if best_key is None or best_key < key:
            best_key = key
            best_index = candidate

    if best_index is None:
        raise RuntimeError(f"Could not select a candidate line for annotation "
                           f"{annotation.identifier}")

    return ResolvedAnnotation(
        annotation=annotation,
        line=best_index + 1,
    )


@beartype
class FileTreeModel(QStandardItemModel):

    def __init__(
        self,
        changes: list[FileChange],
        annotations: list[Annotation],
        parent: QObject | None = None,
    ) -> None:
        super().__init__(parent)
        self.changes = changes
        self.file_items: dict[str, QStandardItem] = {}
        self.comment_items: dict[str, QStandardItem] = {}
        self.setHorizontalHeaderLabels(["Path", "State", "Comments"])
        self.build()
        self.update_comment_counts(annotations)

    def build(self) -> None:
        directory_items: dict[str, QStandardItem] = {}
        root = self.invisibleRootItem()

        for change in self.changes:
            parts = Path(change.display_path).parts
            parent_item = root
            directory_path = ""

            for part in parts[:-1]:
                directory_path = (part if directory_path == "" else
                                  f"{directory_path}/{part}")

                if directory_path not in directory_items:
                    path_item = QStandardItem(part)
                    state_item = QStandardItem()
                    comment_item = QStandardItem()

                    for item in (path_item, state_item, comment_item):
                        item.setEditable(False)

                    parent_item.appendRow(
                        [path_item, state_item, comment_item])
                    directory_items[directory_path] = path_item

                parent_item = directory_items[directory_path]

            path_item = QStandardItem(parts[-1])
            state_item = QStandardItem(change.kind.title)
            comment_item = QStandardItem()

            for item in (path_item, state_item, comment_item):
                item.setEditable(False)
                item.setData(change, int(Qt.ItemDataRole.UserRole))
                item.setToolTip(change.display_path)

            self.apply_change_color(
                change.kind,
                [path_item, state_item, comment_item],
            )
            parent_item.appendRow([path_item, state_item, comment_item])
            self.file_items[change.display_path] = path_item
            self.comment_items[change.display_path] = comment_item

    def apply_change_color(
        self,
        kind: ChangeKind,
        items: list[QStandardItem],
    ) -> None:
        match kind:
            case ChangeKind.ADDED:
                foreground = QColor(25, 125, 55)
                background = QColor(60, 180, 90, 40)
            case ChangeKind.DELETED:
                foreground = QColor(190, 45, 45)
                background = QColor(220, 60, 60, 40)
            case ChangeKind.MODIFIED | ChangeKind.TYPE_CHANGED:
                foreground = QColor(185, 120, 15)
                background = QColor(230, 170, 40, 40)
            case ChangeKind.RENAMED | ChangeKind.COPIED:
                foreground = QColor(55, 105, 190)
                background = QColor(70, 125, 220, 40)

        for item in items:
            item.setForeground(QBrush(foreground))
            item.setBackground(QBrush(background))

    def update_comment_counts(self, annotations: list[Annotation]) -> None:
        counts: dict[str, int] = {}
        for annotation in annotations:
            counts[annotation.path] = counts.get(annotation.path, 0) + 1

        for path, path_item in self.file_items.items():
            count = counts.get(path, 0)
            comment_item = self.comment_items[path]
            comment_item.setText(str(count) if 0 < count else "")

            font = path_item.font()
            font.setBold(0 < count)
            path_item.setFont(font)


class CodeModel(QAbstractTableModel):

    def __init__(self, parent: QObject | None = None) -> None:
        super().__init__(parent)
        self.lines: list[CodeLine] = []

    def set_lines(self, lines: list[CodeLine]) -> None:
        self.beginResetModel()
        self.lines = lines
        self.endResetModel()

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self.lines)

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return 3

    def data(
            self,
            index: QModelIndex,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if not index.isValid() or not 0 <= index.row() < len(self.lines):
            return None

        line = self.lines[index.row()]

        match role:
            case value if value == int(Qt.ItemDataRole.DisplayRole):
                match index.column():
                    case 0:
                        return line.number
                    case 1:
                        return line.text
                    case 2:
                        return line.annotation_count if 0 < line.annotation_count else ""
                    case _:
                        return None
            case value if value == int(Qt.ItemDataRole.TextAlignmentRole):
                if index.column() in (0, 2):
                    return int(Qt.AlignmentFlag.AlignRight
                               | Qt.AlignmentFlag.AlignVCenter)
                return int(Qt.AlignmentFlag.AlignLeft
                           | Qt.AlignmentFlag.AlignVCenter)
            case value if value == int(Qt.ItemDataRole.BackgroundRole):
                if 0 < line.annotation_count:
                    return QColor(80, 140, 255, 55)
                if line.changed:
                    return QColor(255, 190, 40, 55)
                return None
            case value if value == int(Qt.ItemDataRole.UserRole):
                return line.number
            case _:
                return None

    def headerData(
            self,
            section: int,
            orientation: Qt.Orientation,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if role != int(Qt.ItemDataRole.DisplayRole):
            return None

        if orientation != Qt.Orientation.Horizontal:
            return None

        match section:
            case 0:
                return "Line"
            case 1:
                return "Code"
            case 2:
                return "Comments"
            case _:
                return None


class AnnotationModel(QAbstractListModel):

    def __init__(self, parent: QObject | None = None) -> None:
        super().__init__(parent)
        self.annotations: list[ResolvedAnnotation] = []

    def set_annotations(self, annotations: list[ResolvedAnnotation]) -> None:
        self.beginResetModel()
        self.annotations = annotations
        self.endResetModel()

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self.annotations)

    def data(
            self,
            index: QModelIndex,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if not index.isValid() or not 0 <= index.row() < len(self.annotations):
            return None

        resolved = self.annotations[index.row()]
        annotation = resolved.annotation
        created_at = annotation.created_at.isoformat(timespec="seconds")

        match role:
            case value if value == int(Qt.ItemDataRole.DisplayRole):
                return (f"Line {resolved.line} · {annotation.commit[:12]} · "
                        f"{created_at}\n{annotation.text}")
            case value if value == int(Qt.ItemDataRole.ToolTipRole):
                return (f"Commit: {annotation.commit}\n"
                        f"Original line: {annotation.line}\n"
                        f"Current line: {resolved.line}")
            case value if value == int(Qt.ItemDataRole.UserRole):
                return resolved
            case _:
                return None


@beartype
class ReviewWindow(QMainWindow):

    def __init__(
        self,
        repository: ReviewRepository,
        store: AnnotationStore,
    ) -> None:
        super().__init__()
        self.repository = repository
        self.store = store
        self.changes = repository.changes()
        self.current_change: FileChange | None = None
        self.current_content = FileContent(
            lines=[],
            exists=False,
            binary=False,
        )
        self.current_resolved: list[ResolvedAnnotation] = []
        self.selected_line: int | None = None
        self.editing_identifier: str | None = None

        self.file_model = FileTreeModel(
            self.changes,
            self.store.annotations,
            self,
        )
        self.code_model = CodeModel(self)
        self.annotation_model = AnnotationModel(self)

        self.file_view = QTreeView()
        self.file_view.setModel(self.file_model)
        self.file_view.setAlternatingRowColors(True)
        self.file_view.setUniformRowHeights(True)
        self.file_view.setSelectionBehavior(
            QTreeView.SelectionBehavior.SelectRows)
        self.file_view.header().setSectionResizeMode(
            0,
            QHeaderView.ResizeMode.Stretch,
        )
        self.file_view.header().setSectionResizeMode(
            1,
            QHeaderView.ResizeMode.ResizeToContents,
        )
        self.file_view.header().setSectionResizeMode(
            2,
            QHeaderView.ResizeMode.ResizeToContents,
        )

        self.path_label = QLabel()
        self.path_label.setTextInteractionFlags(
            Qt.TextInteractionFlag.TextSelectableByMouse)

        fixed_font = QFontDatabase.systemFont(
            QFontDatabase.SystemFont.FixedFont)

        self.code_view = QTableView()
        self.code_view.setModel(self.code_model)
        self.code_view.setFont(fixed_font)
        self.code_view.setSelectionBehavior(
            QTableView.SelectionBehavior.SelectRows)
        self.code_view.setSelectionMode(
            QTableView.SelectionMode.SingleSelection)
        self.code_view.setWordWrap(False)
        self.code_view.setShowGrid(False)
        self.code_view.setStyleSheet(
            "QTableView::item { padding: 0px 4px; margin: 0px; }")
        self.code_view.verticalHeader().setVisible(False)
        self.code_view.verticalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Fixed)
        self.code_view.verticalHeader().setMinimumSectionSize(
            self.code_view.fontMetrics().height() + 2)
        self.code_view.verticalHeader().setDefaultSectionSize(
            self.code_view.fontMetrics().height() + 2)
        self.code_view.horizontalHeader().setSectionResizeMode(
            0,
            QHeaderView.ResizeMode.ResizeToContents,
        )
        self.code_view.horizontalHeader().setSectionResizeMode(
            1,
            QHeaderView.ResizeMode.Stretch,
        )
        self.code_view.horizontalHeader().setSectionResizeMode(
            2,
            QHeaderView.ResizeMode.ResizeToContents,
        )

        self.scope_box = QComboBox()
        self.scope_box.addItem(
            "Selected line",
            CommentScope.SELECTED_LINE,
        )
        self.scope_box.addItem(
            "All annotations on current lines",
            CommentScope.CURRENT_FILE,
        )

        self.annotation_view = QListView()
        self.annotation_view.setModel(self.annotation_model)
        self.annotation_view.setWordWrap(True)

        self.annotation_editor = QPlainTextEdit()
        self.annotation_editor.setPlaceholderText(
            "Write an annotation for the selected line")

        self.new_button = QPushButton("New annotation")
        self.save_button = QPushButton("Add annotation")
        self.new_button.setEnabled(False)
        self.save_button.setEnabled(False)

        center_widget = QWidget()
        center_layout = QVBoxLayout(center_widget)
        center_layout.setContentsMargins(4, 4, 4, 4)
        center_layout.setSpacing(2)
        center_layout.addWidget(self.path_label)
        center_layout.addWidget(self.code_view)

        right_widget = QWidget()
        right_layout = QVBoxLayout(right_widget)
        right_layout.setContentsMargins(4, 4, 4, 4)
        right_layout.setSpacing(4)
        right_layout.addWidget(self.scope_box)
        right_layout.addWidget(self.annotation_view, 1)
        right_layout.addWidget(self.annotation_editor)
        right_layout.addWidget(self.new_button)
        right_layout.addWidget(self.save_button)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self.file_view)
        splitter.addWidget(center_widget)
        splitter.addWidget(right_widget)
        splitter.setSizes([320, 900, 360])

        self.setCentralWidget(splitter)
        self.setWindowTitle(
            f"Git Review — {repository.current_reference} against "
            f"{repository.target_reference}")
        self.resize(1500, 900)

        self.file_view.clicked.connect(self.open_file)
        self.code_view.clicked.connect(self.select_line)
        self.annotation_view.clicked.connect(self.edit_annotation)
        self.scope_box.currentIndexChanged.connect(self.change_scope)
        self.new_button.clicked.connect(self.start_new_annotation)
        self.save_button.clicked.connect(self.save_annotation)

        self.open_first_file()

    def open_first_file(self) -> None:
        if len(self.changes) == 0:
            self.path_label.setText(
                f"No changes between {self.repository.target_reference} and "
                f"{self.repository.current_reference}")
            return

        first_path = self.changes[0].display_path
        item = self.file_model.file_items[first_path]
        index = item.index()
        self.file_view.setCurrentIndex(index)
        self.file_view.scrollTo(index)
        self.open_file(index)

    def open_file(self, index: QModelIndex) -> None:
        path_index = index.sibling(index.row(), 0)
        change = self.file_model.data(
            path_index,
            int(Qt.ItemDataRole.UserRole),
        )

        if change is None:
            self.file_view.expand(path_index)
            return

        if not isinstance(change, FileChange):
            raise TypeError(f"File tree row {index.row()} returned "
                            f"{type(change).__name__}, expected FileChange")

        self.current_change = change
        self.reload_current_file(None)

    def reload_current_file(self, selected_line: int | None) -> None:
        if self.current_change is None:
            raise RuntimeError(
                "Cannot reload file content because no changed file is selected"
            )

        self.current_content = self.repository.current_content(
            self.current_change)
        base_content = self.repository.base_content(self.current_change)
        self.selected_line = None
        self.cancel_editing()
        self.path_label.setText(self.current_change.display_path)

        if not self.current_content.exists:
            self.path_label.setText(
                f"{self.current_change.display_path} — removed")
            self.code_model.set_lines([])
            self.current_resolved = []
            self.annotation_model.set_annotations([])
            self.save_button.setEnabled(False)
            return

        if self.current_content.binary:
            self.path_label.setText(
                f"{self.current_change.display_path} — binary file")
            self.code_model.set_lines([])
            self.current_resolved = []
            self.annotation_model.set_annotations([])
            self.save_button.setEnabled(False)
            return

        changed_lines = self.repository.changed_lines(
            base_content,
            self.current_content,
        )
        self.resolve_current_annotations()

        annotation_counts: dict[int, int] = {}
        for resolved in self.current_resolved:
            annotation_counts[resolved.line] = (
                annotation_counts.get(resolved.line, 0) + 1)

        code_lines = [
            CodeLine(
                number=index + 1,
                text=line,
                changed=index + 1 in changed_lines,
                annotation_count=annotation_counts.get(index + 1, 0),
            ) for index, line in enumerate(self.current_content.lines)
        ]
        self.code_model.set_lines(code_lines)

        if len(code_lines) == 0:
            self.refresh_annotation_view()
            self.save_button.setEnabled(False)
            return

        line = selected_line if selected_line is not None else 1
        line = min(max(line, 1), len(code_lines))
        selected_index = self.code_model.index(line - 1, 0)
        self.code_view.setCurrentIndex(selected_index)
        self.code_view.scrollTo(selected_index)
        self.select_line(selected_index)

    def resolve_current_annotations(self) -> None:
        if (self.current_change is None
                or self.current_change.current_path is None):
            self.current_resolved = []
            return

        resolved: list[ResolvedAnnotation] = []
        for annotation in self.store.annotations:
            current = resolve_annotation(
                annotation,
                self.current_change.current_path,
                self.current_content.lines,
            )
            if current is not None:
                resolved.append(current)

        self.current_resolved = sorted(
            resolved,
            key=lambda current: (
                current.line,
                current.annotation.created_at,
                current.annotation.identifier,
            ),
        )

    def select_line(self, index: QModelIndex) -> None:
        line = self.code_model.data(
            index,
            int(Qt.ItemDataRole.UserRole),
        )
        if not isinstance(line, int):
            raise TypeError(f"Code model row {index.row()} returned "
                            f"{type(line).__name__}, expected int line number")

        self.selected_line = line
        self.cancel_editing()
        self.save_button.setEnabled(True)
        self.refresh_annotation_view()

    def change_scope(self, index: int) -> None:
        if not 0 <= index < self.scope_box.count():
            raise ValueError(
                f"Comment scope index {index} is outside the available "
                f"range 0:{self.scope_box.count()}")

        self.cancel_editing()
        self.refresh_annotation_view()

    def refresh_annotation_view(self) -> None:
        scope = self.scope_box.currentData()

        match scope:
            case CommentScope.SELECTED_LINE:
                if self.selected_line is None:
                    annotations: list[ResolvedAnnotation] = []
                else:
                    annotations = [
                        resolved for resolved in self.current_resolved
                        if resolved.line == self.selected_line
                    ]
            case CommentScope.CURRENT_FILE:
                annotations = self.current_resolved
            case _:
                raise ValueError(f"Unsupported annotation scope {scope!r}")

        self.annotation_model.set_annotations(annotations)

    def edit_annotation(self, index: QModelIndex) -> None:
        resolved = self.annotation_model.data(
            index,
            int(Qt.ItemDataRole.UserRole),
        )
        if not isinstance(resolved, ResolvedAnnotation):
            raise TypeError(
                f"Annotation model row {index.row()} returned "
                f"{type(resolved).__name__}, expected ResolvedAnnotation")

        self.editing_identifier = resolved.annotation.identifier
        self.annotation_editor.setPlainText(resolved.annotation.text)
        self.save_button.setText("Save changes")
        self.save_button.setEnabled(True)
        self.new_button.setEnabled(True)

    def start_new_annotation(self, checked: bool) -> None:
        if checked:
            self.new_button.setChecked(False)

        self.cancel_editing()
        self.annotation_editor.setFocus()

    def cancel_editing(self) -> None:
        self.editing_identifier = None
        self.annotation_editor.clear()
        self.save_button.setText("Add annotation")
        self.new_button.setEnabled(False)
        self.save_button.setEnabled(self.selected_line is not None)

    def save_annotation(self, checked: bool) -> None:
        if checked:
            self.save_button.setChecked(False)

        text = self.annotation_editor.toPlainText().strip()
        if text == "":
            QMessageBox.warning(
                self,
                "Empty annotation",
                "Enter annotation text before saving it.",
            )
            return

        if self.editing_identifier is not None:
            self.update_annotation(text)
            return

        self.add_annotation(text)

    def add_annotation(self, text: str) -> None:
        if (self.current_change is None
                or self.current_change.current_path is None):
            raise RuntimeError(
                "Cannot add an annotation without a current file")

        if self.selected_line is None:
            raise RuntimeError(
                "Cannot add an annotation without a selected line")

        line_index = self.selected_line - 1
        if not 0 <= line_index < len(self.current_content.lines):
            raise IndexError(
                f"Selected line {self.selected_line} is outside file "
                f"{self.current_change.current_path}, which has "
                f"{len(self.current_content.lines)} lines")

        context_size = 3
        before_start = max(0, line_index - context_size)
        after_end = min(
            len(self.current_content.lines),
            line_index + context_size + 1,
        )

        annotation = Annotation(
            identifier=str(uuid4()),
            path=self.current_change.current_path,
            commit=self.repository.current_commit.hexsha,
            line=self.selected_line,
            line_text=self.current_content.lines[line_index],
            before=self.current_content.lines[before_start:line_index],
            after=self.current_content.lines[line_index + 1:after_end],
            text=text,
            created_at=datetime.now(UTC),
        )
        selected_line = self.selected_line
        self.store.add(annotation)
        self.file_model.update_comment_counts(self.store.annotations)
        self.reload_current_file(selected_line)

    def update_annotation(self, text: str) -> None:
        identifier = self.editing_identifier
        if identifier is None:
            raise RuntimeError(
                "Cannot update an annotation without an editing identifier")

        existing = next(
            (annotation for annotation in self.store.annotations
             if annotation.identifier == identifier),
            None,
        )
        if existing is None:
            raise ValueError(
                f"Cannot update annotation {identifier!r} because it is "
                f"not present in the annotation cache")

        updated = existing.model_copy(update={"text": text})
        selected_line = self.selected_line
        self.store.update(updated)
        self.file_model.update_comment_counts(self.store.annotations)
        self.reload_current_file(selected_line)


@beartype
def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=(
        "Review branch changes and attach persistent line annotations"))
    parser.add_argument(
        "repository",
        type=Path,
        help="Path to the Git repository",
    )
    parser.add_argument(
        "--target",
        default="master",
        help="Target branch or revision",
    )
    parser.add_argument(
        "--current",
        default=None,
        help="Current branch or revision; defaults to HEAD",
    )
    return parser.parse_args()


@beartype
def main() -> int:
    arguments = parse_arguments()

    application = QApplication([])
    application.setOrganizationName("haxscramper")
    application.setApplicationName("git-review")

    repository = ReviewRepository(
        repository_path=arguments.repository,
        target_reference=arguments.target,
        current_reference=arguments.current,
    )
    store = AnnotationStore(repository.annotation_path())
    window = ReviewWindow(repository, store)
    window.show()

    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main())

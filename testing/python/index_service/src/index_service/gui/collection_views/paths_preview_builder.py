from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

import magic
from beartype import beartype
from PyQt6.QtCore import QAbstractTableModel, QModelIndex, QSize, Qt, QUrl
from PyQt6.QtGui import QPixmap
from PyQt6.QtMultimedia import QAudioOutput, QMediaPlayer
from PyQt6.QtMultimediaWidgets import QVideoWidget
from PyQt6.QtPdf import QPdfDocument
from PyQt6.QtPdfWidgets import QPdfView
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QHeaderView,
    QLabel,
    QPlainTextEdit,
    QTableView,
    QVBoxLayout,
    QWidget,
    QSizePolicy,
)

from index_service.gui.collection_views.file_content_view.file_content_builder import FileContentViewBuilder
from index_service.gui.collection_views.file_content_view.image_content_view_builder import ImageFileContentViewBuilder
from index_service.gui.collection_views.file_content_view.pdf_content_view_builder import PdfFileContentViewBuilder
from index_service.gui.collection_views.file_content_view.text_content_view_builder import TextFileContentViewBuilder
from index_service.gui.collection_views.file_content_view.video_content_view_builder import VideoFileContentViewBuilder
from index_service.gui.common.qt_utils import get_settings
from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import FileHash
from index_service.services.utils import _format_timestamp_relative

ABSOLUTE_PATHS_QUERY = """
LET file = DOCUMENT("files", @file_key)
FOR entry IN file.paths
  LET root = DOCUMENT("roots", entry.root.name)
  RETURN CONCAT_SEPARATOR("/", root.path, entry.relative)
""".strip()


@dataclass(frozen=True)
class PathRow:
    path: str
    created: str
    modified: str


@beartype
class PathsTableModel(QAbstractTableModel):
    _headers = ("File path", "Created", "Modified")

    def __init__(self, paths: list[str], parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._rows: list[PathRow] = [self._row_from_path(p) for p in paths]

    def _row_from_path(self, path: str) -> PathRow:
        stat = Path(path).stat()
        created_ts = getattr(stat, "st_birthtime", stat.st_ctime)
        modified_ts = stat.st_mtime
        return PathRow(
            path=path,
            created=_format_timestamp_relative(created_ts),
            modified=_format_timestamp_relative(modified_ts),
        )

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self._rows)

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return 3

    def data(self,
             index: QModelIndex,
             role: int = Qt.ItemDataRole.DisplayRole) -> str | None:
        if not index.isValid() or role != Qt.ItemDataRole.DisplayRole:
            return None

        row = self._rows[index.row()]
        column = index.column()

        if column == 0:
            return row.path
        if column == 1:
            return row.created
        if column == 2:
            return row.modified
        return None

    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> str | None:
        if role != Qt.ItemDataRole.DisplayRole:
            return None
        if orientation == Qt.Orientation.Horizontal:
            return self._headers[section]
        return str(section + 1)


class ShrinkableLabel(QLabel):

    def minimumSizeHint(self) -> QSize:
        return QSize(0, 0)

    def sizeHint(self) -> QSize:
        return QSize(0, 0)


@beartype
class DispatchingFileContentPreviewBuilder:

    def __init__(self) -> None:
        self._magic = magic.Magic(mime=True)
        self._builders: list[FileContentViewBuilder] = [
            PdfFileContentViewBuilder(),
            ImageFileContentViewBuilder(),
            VideoFileContentViewBuilder(),
            TextFileContentViewBuilder(),
        ]

    def build(self, absolute_path: str) -> QWidget:
        mime = self._magic.from_file(absolute_path)
        for builder in self._builders:
            if builder.can_build(mime):
                widget = builder.build(absolute_path)
                widget.setSizePolicy(QSizePolicy.Policy.Expanding,
                                     QSizePolicy.Policy.Expanding)
                widget.setMinimumSize(0, 0)
                return widget

        fallback = ShrinkableLabel(f"Unsupported file type: {mime}\n{absolute_path}")
        fallback.setAlignment(Qt.AlignmentFlag.AlignCenter)
        fallback.setWordWrap(True)
        fallback.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        fallback.setMinimumSize(0, 0)
        return fallback


@beartype
class PathsWidgetBuilder:

    def __init__(self) -> None:
        self.absolute_paths: list[str] = []
        self._settings = get_settings()
        self._header_state_key = "paths_widget/table_header_state"

        self._root: QWidget | None = None
        self._preview_host: QWidget | None = None
        self._preview_layout: QVBoxLayout | None = None
        self._table: QTableView | None = None
        self._model: PathsTableModel | None = None

    def _load_absolute_paths(self, db: IndexDatabase, file_hash: FileHash) -> list[str]:
        cursor = db._db.aql.execute(
            ABSOLUTE_PATHS_QUERY,
            bind_vars={"file_key": file_hash.hash},
        )
        return [str(path) for path in cursor]

    def _save_header_state(self) -> None:
        if self._table is None:
            return
        self._settings.setValue(self._header_state_key,
                                self._table.horizontalHeader().saveState())

    def _restore_header_state(self) -> None:
        if self._table is None:
            return
        state = self._settings.value(self._header_state_key, None)
        if state is not None:
            self._table.horizontalHeader().restoreState(state)

    def _ensure_ui(self) -> None:
        if self._root is not None:
            return

        root = QWidget()
        root.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        layout = QVBoxLayout(root)
        layout.setContentsMargins(0, 0, 0, 0)

        preview_host = QWidget(root)
        preview_host.setSizePolicy(QSizePolicy.Policy.Expanding,
                                   QSizePolicy.Policy.Expanding)
        preview_layout = QVBoxLayout(preview_host)
        preview_layout.setContentsMargins(0, 0, 0, 0)

        table = QTableView(root)
        table.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        table.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        table.verticalHeader().setVisible(False)
        table.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        table.horizontalHeader().setSectionResizeMode(
            1, QHeaderView.ResizeMode.ResizeToContents)
        table.horizontalHeader().setSectionResizeMode(
            2, QHeaderView.ResizeMode.ResizeToContents)

        table.horizontalHeader().sectionResized.connect(self._save_header_state)

        layout.addWidget(preview_host, 3)
        layout.addWidget(table, 2)

        self._root = root
        self._preview_host = preview_host
        self._preview_layout = preview_layout
        self._table = table
        self._restore_header_state()

    def _set_preview(self) -> None:
        assert self._preview_layout is not None
        while self._preview_layout.count():
            item = self._preview_layout.takeAt(0)
            w = item.widget()
            if w is not None:
                w.deleteLater()

        if self.absolute_paths:
            preview = DispatchingFileContentPreviewBuilder().build(self.absolute_paths[0])
            self._preview_layout.addWidget(preview)
        else:
            empty = QLabel("No paths found")
            empty.setAlignment(Qt.AlignmentFlag.AlignCenter)
            empty.setSizePolicy(QSizePolicy.Policy.Expanding,
                                QSizePolicy.Policy.Expanding)
            self._preview_layout.addWidget(empty)

    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        self._ensure_ui()
        assert self._root is not None
        assert self._table is not None

        self.absolute_paths = self._load_absolute_paths(db, hash)
        self._set_preview()

        self._model = PathsTableModel(self.absolute_paths, self._table)
        self._table.setModel(self._model)
        self._restore_header_state()

        self._root._paths_model = self._model
        return self._root

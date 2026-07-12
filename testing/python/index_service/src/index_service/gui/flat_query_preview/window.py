from __future__ import annotations

import logging
from typing import List, Optional, Sequence

from beartype import beartype
from PyQt6.QtCore import QModelIndex, QSize, Qt
from PyQt6.QtWidgets import (
    QHBoxLayout,
    QLabel,
    QListView,
    QMainWindow,
    QPlainTextEdit,
    QPushButton,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.preview_pane import FilePreviewPane
from index_service.gui.file_preview_delegate import (
    FilePreviewDelegate,
    ThumbnailCache,
)
from index_service.gui.flat_query_preview.query_model import QueryResultModel
from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)

TILE_SIZE = 128
BATCH_SIZE = 200

DEFAULT_QUERY = """FOR f IN files
  RETURN { hash: f._key }"""


@beartype
class FlatQueryViewWindow(QMainWindow):

    def __init__(
        self,
        db: IndexDatabase,
        collection_names: List[str],
        builders: Sequence[WidgetBuilder],
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)

        self._db = db
        self._model = QueryResultModel(db, BATCH_SIZE)
        self._current_hash: Optional[FileHash] = None

        central = QWidget()
        layout = QHBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        layout.addWidget(splitter)

        left = QWidget()
        left_layout = QVBoxLayout(left)

        self._query_edit = QPlainTextEdit()
        self._query_edit.setPlainText(DEFAULT_QUERY)

        self._run_button = QPushButton("Run")
        self._run_button.clicked.connect(self._on_run)

        self._status = QLabel("Idle")

        left_layout.addWidget(self._query_edit)
        left_layout.addWidget(self._run_button)
        left_layout.addWidget(self._status)

        self._list = QListView()
        self._list.setViewMode(QListView.ViewMode.IconMode)
        self._list.setGridSize(QSize(TILE_SIZE, TILE_SIZE))
        self._list.setResizeMode(QListView.ResizeMode.Adjust)
        self._list.setUniformItemSizes(True)
        self._list.setSpacing(0)
        self._list.setModel(self._model)
        self._list.clicked.connect(self._on_clicked)

        self._cache = ThumbnailCache(TILE_SIZE)
        self._cache.updated.connect(self._list.viewport().update)
        self._list.setItemDelegate(FilePreviewDelegate(self._cache, TILE_SIZE),)

        self._preview = FilePreviewPane(
            db=db,
            collection_names=collection_names,
            builders=builders,
        )

        splitter.addWidget(left)
        splitter.addWidget(self._list)
        splitter.addWidget(self._preview)
        splitter.setSizes([240, 720, 340])
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 3)
        splitter.setStretchFactor(2, 1)

        self.setCentralWidget(central)
        self.setWindowTitle("Index Service Viewer")
        self.resize(1400, 700)

    def _on_run(self) -> None:
        query = self._query_edit.toPlainText()
        log.info("Running AQL query:\n%s", query)

        count = self._model.run_query(query)
        self._status.setText(f"Loaded {count} rows")
        log.info("Initial results loaded: %d", count)

        self._cache = ThumbnailCache(TILE_SIZE)
        self._cache.updated.connect(self._list.viewport().update)
        self._list.setItemDelegate(FilePreviewDelegate(self._cache, TILE_SIZE),)

    def _on_clicked(self, index: QModelIndex) -> None:
        hash_str = index.data(QueryResultModel.HashRole)
        if not hash_str:
            return

        hash = FileHash(hash=hash_str)
        self._current_hash = hash

        log.info("Selected hash=%s", hash.hash)
        self._preview.show_hash(hash)

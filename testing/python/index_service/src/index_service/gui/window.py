from __future__ import annotations

import json
import logging
from typing import List, Optional

from beartype import beartype
from PySide6.QtCore import (
    QModelIndex,
    QSize,
    Qt,
)

from PySide6.QtWidgets import (
    QAbstractItemView,
    QHBoxLayout,
    QLabel,
    QListView,
    QListWidget,
    QListWidgetItem,
    QMainWindow,
    QPlainTextEdit,
    QPushButton,
    QSplitter,
    QTabWidget,
    QVBoxLayout,
    QWidget,
)

from index_service.gui.file_preview_delegate import FilePreviewDelegate, ThumbnailCache
from index_service.gui.query_model import QueryResultModel
from index_service.services.db import IndexDatabase
from index_service.services.types import MD5

log = logging.getLogger(__name__)

TILE_SIZE = 128
BATCH_SIZE = 200
DEFAULT_QUERY = """FOR f IN files
  RETURN { md5: f._key }"""


@beartype
def build_json_widget(doc: Optional[dict]) -> QWidget:
    view = QPlainTextEdit()
    view.setReadOnly(True)
    if doc is None:
        view.setPlainText("<no document>")
    else:
        view.setPlainText(json.dumps(doc, indent=2, default=str))
    return view


@beartype
def build_paths_widget(db: IndexDatabase, md5: MD5) -> QWidget:
    fdoc = db._db.collection("files").get(md5.md5)
    paths = fdoc.get("paths", []) if fdoc else []
    view = QListWidget()
    view.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
    for p in paths:
        QListWidgetItem(str(p), view)
    return view


@beartype
class MainWindow(QMainWindow):

    def __init__(
        self,
        db: IndexDatabase,
        collection_names: List[str],
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self._db = db
        self._collection_names = list(collection_names)
        self._model = QueryResultModel(db, BATCH_SIZE)
        self._current_md5: Optional[MD5] = None

        central = QWidget()
        h = QHBoxLayout(central)
        h.setContentsMargins(0, 0, 0, 0)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        h.addWidget(splitter)

        # Left pane: query editor.
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

        # Central pane: square thumbnail grid.
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
        self._list.setItemDelegate(FilePreviewDelegate(self._cache, TILE_SIZE))

        # Right pane: collection tabs (empty at start).
        self._tabs = QTabWidget()
        self._tabs.currentChanged.connect(self._on_tab_changed)
        self._tabs.setTabPosition(QTabWidget.TabPosition.East)
        self._tabs.setMovable(False)

        splitter.addWidget(left)
        splitter.addWidget(self._list)
        splitter.addWidget(self._tabs)
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 3)
        splitter.setStretchFactor(2, 2)

        self.setCentralWidget(central)
        self.setWindowTitle("Index Service Viewer")
        self.resize(1200, 700)

    def _on_tab_changed(self, index: int) -> None:
        if index >= 0:
            log.info(f"Tab changed to {index}")

    def _on_run(self) -> None:
        query = self._query_edit.toPlainText()
        log.info(f"Running AQL query:\n{query}")

        count = self._model.run_query(query)

        log.info(f"Initial results loaded: {count}")
        self._status.setText(f"Loaded {count} rows")

        self._cache = ThumbnailCache(TILE_SIZE)
        self._cache.updated.connect(self._list.viewport().update)
        self._list.setItemDelegate(FilePreviewDelegate(self._cache, TILE_SIZE))

    def _on_clicked(self, index: QModelIndex) -> None:
        md5_str = index.data(QueryResultModel.Md5Role)
        if not md5_str:
            return
        md5 = MD5(md5=md5_str)
        self._current_md5 = md5
        log.info(f"Selected md5={md5.md5}")
        self._populate_right_pane(md5)

    def _populate_right_pane(self, md5: MD5) -> None:
        prev = self._tabs.currentIndex()
        self._tabs.clear()
        paths_widget = build_paths_widget(self._db, md5)
        self._tabs.addTab(paths_widget, "paths")
        for name in self._collection_names:
            doc = self._db._db.collection(name).get(md5.md5)
            widget = build_json_widget(doc)
            self._tabs.addTab(widget, name)
        target = prev if 0 <= prev < self._tabs.count() else 0
        self._tabs.setCurrentIndex(target)

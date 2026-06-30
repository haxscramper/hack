from __future__ import annotations

import logging
from typing import List, Optional, Sequence

from beartype import beartype
from PySide6.QtCore import (
    QModelIndex,
    QSize,
    Qt,
)
from PySide6.QtGui import QPainter, QPen
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
    QTabBar,
    QTabWidget,
    QVBoxLayout,
    QWidget,
)

from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.json_preview_builder import JsonWidgetBuilder
from index_service.gui.file_preview_delegate import FilePreviewDelegate, ThumbnailCache
from index_service.gui.query_model import QueryResultModel
from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)

TILE_SIZE = 128
BATCH_SIZE = 200

DEFAULT_QUERY = """FOR f IN files
  RETURN { hash: f._key }"""


@beartype
def build_paths_widget(db: IndexDatabase, hash: FileHash) -> QWidget:
    fdoc = db._db.collection("files").get(hash.hash)
    paths = fdoc.get("paths", []) if fdoc else []
    view = QListWidget()
    view.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
    for p in paths:
        QListWidgetItem(str(p), view)
    return view


class PathsWidgetBuilder:

    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        return build_paths_widget(db, hash)


class HorizontalTextTabBar(QTabBar):

    def tabSizeHint(self, index: int) -> QSize:
        fm = self.fontMetrics()
        text = self.tabText(index)
        return QSize(fm.horizontalAdvance(text) + 24, fm.height() + 12)

    def paintEvent(self, event) -> None:  # type: ignore[override]
        painter = QPainter(self)
        pal = self.palette()
        group = (pal.ColorGroup.Current
                 if self.isEnabled() else pal.ColorGroup.Disabled)

        for i in range(self.count()):
            rect = self.tabRect(i)
            selected = self.currentIndex() == i

            if selected:
                painter.fillRect(rect, pal.color(group,
                                                 pal.ColorRole.Highlight))
                text_color = pal.color(group, pal.ColorRole.HighlightedText)
            else:
                painter.fillRect(rect, pal.color(group, pal.ColorRole.Window))
                text_color = pal.color(group, pal.ColorRole.WindowText)

            painter.setPen(QPen(pal.color(group, pal.ColorRole.Mid)))
            painter.drawRect(rect.adjusted(0, 0, -1, -1))

            painter.setPen(text_color)
            painter.drawText(
                rect,
                Qt.AlignmentFlag.AlignCenter,
                self.tabText(i),
            )


@beartype
class MainWindow(QMainWindow):

    def __init__(
        self,
        db: IndexDatabase,
        collection_names: List[str],
        builders: Sequence[WidgetBuilder],
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self._db = db
        self._collection_names = list(collection_names)
        self._model = QueryResultModel(db, BATCH_SIZE)
        self._current_hash: Optional[FileHash] = None

        self._widget_builders: dict[str, WidgetBuilder] = {
            "paths": PathsWidgetBuilder()  # type: ignore
        }

        for name in self._collection_names:
            for builder in builders:
                if builder.asset_name == name:
                    self._widget_builders[name] = builder

            if name not in self._widget_builders:
                self._widget_builders[name] = JsonWidgetBuilder(name)

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
        self._tabs.setTabBar(HorizontalTextTabBar())
        self._tabs.currentChanged.connect(self._on_tab_changed)
        self._tabs.setTabPosition(QTabWidget.TabPosition.East)
        self._tabs.setMovable(False)

        splitter.addWidget(left)
        splitter.addWidget(self._list)
        splitter.addWidget(self._tabs)
        splitter.setSizes([240, 720, 340])
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 3)
        splitter.setStretchFactor(2, 1)

        self.setCentralWidget(central)
        self.setWindowTitle("Index Service Viewer")
        self.resize(1400, 700)

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
        hash_str = index.data(QueryResultModel.HashRole)
        if not hash_str:
            return
        hash = FileHash(hash=hash_str)
        self._current_hash = hash
        log.info(f"Selected hash={hash.hash}")
        self._populate_right_pane(hash)

    def _populate_right_pane(self, hash: FileHash) -> None:
        prev = self._tabs.currentIndex()
        self._tabs.clear()
        for name, builder in self._widget_builders.items():
            widget = builder.build(self._db, hash)
            self._tabs.addTab(widget, name)
        target = prev if 0 <= prev < self._tabs.count() else 0
        self._tabs.setCurrentIndex(target)

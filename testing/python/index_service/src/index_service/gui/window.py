from __future__ import annotations

import json
import logging
import subprocess
from typing import List, Optional, Protocol

from beartype import beartype
from PySide6.QtCore import (
    QModelIndex,
    QSize,
    Qt,
    QTimer,
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
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from index_service.gui.file_preview_delegate import FilePreviewDelegate, ThumbnailCache
from index_service.gui.query_model import QueryResultModel
from index_service.services.db import IndexDatabase
from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexerResult
from index_service.services.resources.wd_tagger import WdTaggerResult
from index_service.services.types import MD5

log = logging.getLogger(__name__)

TILE_SIZE = 128
BATCH_SIZE = 200
REFLOW_DEBOUNCE_MS = 200
DEFAULT_QUERY = """FOR f IN files
  RETURN { md5: f._key }"""


class WidgetBuilder(Protocol):

    def build(self, db: IndexDatabase, md5: MD5) -> QWidget:
        ...


@beartype
def build_paths_widget(db: IndexDatabase, md5: MD5) -> QWidget:
    fdoc = db._db.collection("files").get(md5.md5)
    paths = fdoc.get("paths", []) if fdoc else []
    view = QListWidget()
    view.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
    for p in paths:
        QListWidgetItem(str(p), view)
    return view


class PathsWidgetBuilder:

    def build(self, db: IndexDatabase, md5: MD5) -> QWidget:
        return build_paths_widget(db, md5)


class JsonPreviewWidget(QWidget):

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._view = QPlainTextEdit()
        self._view.setReadOnly(True)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self._view)
        self._doc: Optional[dict] = None
        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.timeout.connect(self.reflow)

    def set_doc(self, doc: Optional[dict]) -> None:
        self._doc = doc
        self.reflow()

    def resizeEvent(self, event) -> None:  # type: ignore[override]
        super().resizeEvent(event)
        self._timer.start(REFLOW_DEBOUNCE_MS)

    def reflow(self) -> None:
        if self._doc is None:
            self._view.setPlainText("<no document>")
            return
        width = max(40, self._view.viewport().width() - 2)
        proc = subprocess.run(
            ["fjson", "-w", str(width)],
            input=json.dumps(self._doc, default=str),
            capture_output=True,
            text=True,
            check=True,
        )
        self._view.setPlainText(proc.stdout)


class JsonWidgetBuilder:

    def __init__(self, collection_name: str) -> None:
        self._collection_name = collection_name

    def build(self, db: IndexDatabase, md5: MD5) -> QWidget:
        doc = db._db.collection(self._collection_name).get(md5.md5)
        widget = JsonPreviewWidget()
        widget.set_doc(doc)
        return widget


class WdTaggerWidgetBuilder:

    def build(self, db: IndexDatabase, md5: MD5) -> QWidget:
        doc = db._db.collection("wd_tags").get(md5.md5)
        if doc is None:
            widget = JsonPreviewWidget()
            widget.set_doc(None)
            return widget
        result = WdTaggerResult.model_validate(doc["result"])
        table = QTableWidget(len(result.tags), 3)
        table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        table.setHorizontalHeaderLabels(["category", "name", "probability"])
        for row, tag in enumerate(result.tags):
            table.setItem(row, 0, QTableWidgetItem(tag.category))
            table.setItem(row, 1, QTableWidgetItem(tag.name))
            table.setItem(row, 2, QTableWidgetItem(f"{tag.probability:.4f}"))
        table.horizontalHeader().setStretchLastSection(True)
        return table


class ComfyInputWidgetBuilder:

    def build(self, db: IndexDatabase, md5: MD5) -> QWidget:
        doc = db._db.collection("comfy_input").get(md5.md5)
        if doc is None:
            widget = JsonPreviewWidget()
            widget.set_doc(None)
            return widget
        result = ComfyInputIndexerResult.model_validate(doc["result"])
        table = QTableWidget(len(result.inputs), 2)
        table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        table.setHorizontalHeaderLabels(["node", "inputs"])
        for row, inp in enumerate(result.inputs):
            table.setItem(row, 0, QTableWidgetItem(inp.node))
            table.setItem(
                row, 1, QTableWidgetItem(json.dumps(inp.inputs, default=str)))
        table.horizontalHeader().setStretchLastSection(True)
        return table


class HorizontalTextTabBar(QTabBar):

    def tabSizeHint(self, index: int) -> QSize:
        fm = self.fontMetrics()
        text = self.tabText(index)
        return QSize(fm.horizontalAdvance(text) + 24, fm.height() + 12)

    def paintEvent(self, event) -> None:  # type: ignore[override]
        painter = QPainter(self)
        pal = self.palette()
        group = pal.ColorGroup.Current if self.isEnabled(
        ) else pal.ColorGroup.Disabled
        for i in range(self.count()):
            rect = self.tabRect(i)
            if i == self.currentIndex():
                painter.fillRect(rect, pal.color(group, pal.ColorRole.Button))
            else:
                painter.fillRect(rect, pal.color(group, pal.ColorRole.Window))
            painter.setPen(QPen(pal.color(group, pal.ColorRole.Mid)))
            painter.drawRect(rect)
            painter.setPen(pal.color(group, pal.ColorRole.WindowText))
            painter.drawText(rect, Qt.AlignmentFlag.AlignCenter,
                             self.tabText(i))


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

        self._widget_builders: dict[str, WidgetBuilder] = {
            "paths": PathsWidgetBuilder()
        }

        for name in self._collection_names:
            if name == "wd_tags":
                self._widget_builders[name] = WdTaggerWidgetBuilder()
            elif name == "comfy_input":
                self._widget_builders[name] = ComfyInputWidgetBuilder()
            else:
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
        splitter.setSizes([240, 720, 240])
        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 3)
        splitter.setStretchFactor(2, 1)

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
        for name, builder in self._widget_builders.items():
            widget = builder.build(self._db, md5)
            self._tabs.addTab(widget, name)
        target = prev if 0 <= prev < self._tabs.count() else 0
        self._tabs.setCurrentIndex(target)

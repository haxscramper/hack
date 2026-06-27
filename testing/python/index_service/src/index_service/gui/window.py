from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Any, Dict, List, Optional

from beartype import beartype
from PySide6.QtCore import (
    QAbstractListModel,
    QModelIndex,
    QObject,
    QSize,
    Qt,
    QRunnable,
    QThreadPool,
    Signal,
)
from PySide6.QtGui import (
    QImage,
    QImageReader,
    QPixmap,
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
    QStyle,
    QStyledItemDelegate,
    QTabWidget,
    QVBoxLayout,
    QWidget,
)

from index_service.services.db import IndexDatabase
from index_service.services.types import MD5

log = logging.getLogger(__name__)

TILE_SIZE = 128
BATCH_SIZE = 200
DEFAULT_QUERY = """FOR f IN files
  RETURN { md5: f._key }"""


@beartype
class _ThumbSignals(QObject):
    ready = Signal(str)


@beartype
class _ThumbLoader(QRunnable):

    def __init__(
        self,
        path: str,
        target: QSize,
        signals: _ThumbSignals,
        sink: Dict[str, QImage],
    ) -> None:
        super().__init__()
        self._path = path
        self._target = target
        self._signals = signals
        self._sink = sink

    def run(self) -> None:
        reader = QImageReader(self._path)
        reader.setAutoTransform(True)
        size = reader.size()
        if size.isValid() and not size.isEmpty():
            scaled = size.scaled(self._target,
                                 Qt.AspectRatioMode.KeepAspectRatio)
            reader.setScaledSize(scaled)
        img = reader.read()
        if not img.isNull():
            self._sink[self._path] = img
            self._signals.ready.emit(self._path)


@beartype
class ThumbnailCache(QObject):
    updated = Signal()

    def __init__(self, tile: int, parent: Optional[QObject] = None) -> None:
        super().__init__(parent)
        self._target = QSize(tile, tile)
        self._pool = QThreadPool.globalInstance()
        self._signals = _ThumbSignals()
        self._signals.ready.connect(self._on_ready)
        self._images: Dict[str, QImage] = {}
        self._pixmaps: Dict[str, QPixmap] = {}
        self._inflight: set[str] = set()
        self._failed: set[str] = set()

    def _on_ready(self, path: str) -> None:
        img = self._images.pop(path, None)
        self._inflight.discard(path)
        if img is not None and not img.isNull():
            self._pixmaps[path] = QPixmap.fromImage(img)
        else:
            self._failed.add(path)
        self.updated.emit()

    def get(self, path: str) -> Optional[QPixmap]:
        if path in self._pixmaps:
            return self._pixmaps[path]
        if path in self._failed or path in self._inflight:
            return None
        if not Path(path).exists():
            self._failed.add(path)
            return None
        self._inflight.add(path)
        self._pool.start(
            _ThumbLoader(path, self._target, self._signals, self._images))
        return None


@beartype
class FilePreviewDelegate(QStyledItemDelegate):

    def __init__(self,
                 cache: ThumbnailCache,
                 parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._cache = cache
        self._tile = TILE_SIZE

    def sizeHint(self, option, index: QModelIndex) -> QSize:
        return QSize(self._tile, self._tile)

    def paint(self, painter, option, index: QModelIndex) -> None:
        painter.save()
        if option.state & QStyle.StateFlag.State_Selected:
            painter.fillRect(option.rect, option.palette.highlight())

        pad = 6
        content = option.rect.adjusted(pad, pad, -pad, -pad)
        path = index.data(QueryResultModel.PathRole)
        pix = self._cache.get(path) if path else None

        if pix is not None and not pix.isNull():
            x = content.x() + (content.width() - pix.width()) // 2
            y = content.y() + (content.height() - pix.height()) // 2
            painter.drawPixmap(x, y, pix)
        else:
            label = (Path(path).name
                     if path else index.data(QueryResultModel.Md5Role))
            painter.drawText(
                content,
                Qt.AlignmentFlag.AlignCenter | Qt.TextFlag.TextWordWrap,
                str(label),
            )
        painter.restore()


@beartype
class QueryResultModel(QAbstractListModel):
    Md5Role = Qt.ItemDataRole.UserRole + 1
    PathRole = Qt.ItemDataRole.UserRole + 2
    ExtraRole = Qt.ItemDataRole.UserRole + 3

    def __init__(self,
                 db: IndexDatabase,
                 parent: Optional[QObject] = None) -> None:
        super().__init__(parent)
        self._db = db
        self._cursor: Any = None
        self._rows: List[dict] = []
        self._paths: Dict[str, str] = {}

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self._rows)

    def canFetchMore(self, parent: QModelIndex) -> bool:
        if parent.isValid():
            return False
        return self._cursor is not None

    def fetchMore(self, parent: QModelIndex) -> None:
        if parent.isValid() or self._cursor is None:
            return
        batch: List[dict] = []
        try:
            for _ in range(BATCH_SIZE):
                batch.append(next(self._cursor))
        except StopIteration:
            self._cursor = None
        if not batch:
            return
        begin = len(self._rows)
        self.beginInsertRows(QModelIndex(), begin, begin + len(batch) - 1)
        self._rows.extend(batch)
        self.endInsertRows()

    def run_query(self, aql: str) -> int:
        self.beginResetModel()
        self._rows.clear()
        self._paths.clear()
        if self._cursor is not None:
            try:
                self._cursor.close()
            except Exception:
                pass
            self._cursor = None
        self._cursor = self._db._db.aql.execute(aql,
                                                batch_size=BATCH_SIZE,
                                                count=True)
        self.endResetModel()
        self.fetchMore(QModelIndex())
        return len(self._rows)

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        if not index.isValid():
            return None
        row = index.row()
        if row < 0 or row >= len(self._rows):
            return None
        doc = self._rows[row]
        md5 = doc.get("md5")
        if role == Qt.ItemDataRole.DisplayRole:
            return str(md5)
        if role == self.Md5Role:
            return md5
        if role == self.PathRole:
            if md5 not in self._paths:
                fdoc = self._db._db.collection("files").get(md5)
                paths = fdoc.get("paths", []) if fdoc else []
                self._paths[md5] = str(paths[0]) if paths else ""
            return self._paths[md5]
        if role == self.ExtraRole:
            return {k: v for k, v in doc.items() if k != "md5"}
        return None


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
        self._model = QueryResultModel(db)
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
        self._list.setItemDelegate(FilePreviewDelegate(self._cache))

        # Right pane: collection tabs (empty at start).
        self._tabs = QTabWidget()
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

    def _on_run(self) -> None:
        query = self._query_edit.toPlainText()
        log.info(f"Running AQL query:\n{query}")
        try:
            count = self._model.run_query(query)
        except Exception as e:
            log.error(f"Query failed: {e}")
            self._status.setText(f"Error: {e}")
            return
        log.info(f"Initial results loaded: {count}")
        self._status.setText(f"Loaded {count} rows")

        self._cache = ThumbnailCache(TILE_SIZE)
        self._cache.updated.connect(self._list.viewport().update)
        self._list.setItemDelegate(FilePreviewDelegate(self._cache))

    def _on_clicked(self, index: QModelIndex) -> None:
        md5_str = index.data(QueryResultModel.Md5Role)
        if not md5_str:
            return
        md5 = MD5(md5=md5_str)
        self._current_md5 = md5
        log.info(f"Selected md5={md5.md5}")
        self._populate_right_pane(md5)

    def _populate_right_pane(self, md5: MD5) -> None:
        self._tabs.clear()
        paths_widget = build_paths_widget(self._db, md5)
        self._tabs.addTab(paths_widget, "paths")
        for name in self._collection_names:
            doc = self._db._db.collection(name).get(md5.md5)
            widget = build_json_widget(doc)
            self._tabs.addTab(widget, name)

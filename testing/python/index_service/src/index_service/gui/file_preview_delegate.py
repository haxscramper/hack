from pathlib import Path

from beartype import beartype
from beartype.typing import Optional, Dict

from beartype import beartype
from PySide6.QtCore import (
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
    QStyle,
    QStyledItemDelegate,
    QWidget,
)

from index_service.gui.flat_query_preview.query_model import QueryResultModel


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
            scaled = size.scaled(self._target, Qt.AspectRatioMode.KeepAspectRatio)
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
        self._pool.start(_ThumbLoader(path, self._target, self._signals, self._images))
        return None


@beartype
class FilePreviewDelegate(QStyledItemDelegate):

    def __init__(self,
                 cache: ThumbnailCache,
                 tile: int,
                 parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._cache = cache
        self._tile = tile

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
            label = (Path(path).name if path else index.data(QueryResultModel.HashRole))
            painter.drawText(
                content,
                Qt.AlignmentFlag.AlignCenter | Qt.TextFlag.TextWordWrap,
                str(label),
            )
        painter.restore()

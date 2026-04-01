from __future__ import annotations

from pathlib import Path

from PySide6.QtCore import (
    Signal,
    Qt,
    QAbstractListModel,
    QModelIndex,
    QPersistentModelIndex,
    QThreadPool,
    QRunnable,
    QObject,
    QSize,
)
from PySide6.QtGui import QImage, QPixmap, QImageReader
from PySide6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QListView,
    QSizePolicy,
)

from config import config

IMAGE_EXTENSIONS = config.IMAGE_EXTENSIONS


class ImageLoadSignals(QObject):
    loaded = Signal(int, Path, QImage)


class ImageLoadTask(QRunnable):
    def __init__(
        self, row: int, path: Path, thumb_size: int, signals: ImageLoadSignals
    ):
        super().__init__()
        self.row = row
        self.path = path
        self.thumb_size = thumb_size
        self.signals = signals

    def run(self):
        reader = QImageReader(str(self.path))
        reader.setAutoTransform(True)
        size = reader.size()
        if size.isValid():
            w = size.width()
            h = size.height()
            if w > 0 and h > 0:
                scale = min(self.thumb_size / w, self.thumb_size / h)
                target = QSize(max(1, int(w * scale)), max(1, int(h * scale)))
                reader.setScaledSize(target)
        image = reader.read()
        self.signals.loaded.emit(self.row, self.path, image)


class ImageListModel(QAbstractListModel):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.images = []
        self.cache = {}
        self.loading = set()
        self.thumb_size = 100
        self.signals = ImageLoadSignals()
        self.signals.loaded.connect(self.on_image_loaded)
        self.thread_pool = QThreadPool.globalInstance()

    def set_images(self, images):
        self.beginResetModel()
        self.images = images
        self.cache.clear()
        self.loading.clear()
        self.endResetModel()

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = QModelIndex()
    ) -> int:
        return len(self.images)

    def data(
        self,
        index: QModelIndex | QPersistentModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole.value,
    ):
        if not index.isValid():
            return None
        row = index.row()
        if role == Qt.ItemDataRole.DisplayRole:
            return self.images[row].name
        elif role == Qt.ItemDataRole.DecorationRole:
            if row in self.cache:
                return self.cache[row]
            else:
                if row not in self.loading:
                    self.loading.add(row)
                    task = ImageLoadTask(
                        row, self.images[row], self.thumb_size, self.signals
                    )
                    self.thread_pool.start(task)

                pix = QPixmap(self.thumb_size, self.thumb_size)
                pix.fill(Qt.GlobalColor.lightGray)
                return pix
        elif role == Qt.ItemDataRole.UserRole:
            return str(self.images[row])
        return None

    def on_image_loaded(self, row: int, path: Path, image: QImage):
        if row >= len(self.images) or self.images[row] != path:
            return

        if row in self.loading:
            self.loading.remove(row)
        if not image.isNull():
            self.cache[row] = QPixmap.fromImage(image)
        else:
            pix = QPixmap(self.thumb_size, self.thumb_size)
            pix.fill(Qt.GlobalColor.darkGray)
            self.cache[row] = pix

        index = self.index(row, 0)
        if index.isValid():
            self.dataChanged.emit(index, index, [Qt.ItemDataRole.DecorationRole])


class ImageListWidget(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self.list_view = QListView()
        self.list_view.setViewMode(QListView.ViewMode.IconMode)
        self.list_view.setResizeMode(QListView.ResizeMode.Adjust)
        self.list_view.setWordWrap(True)
        self.list_view.setUniformItemSizes(True)
        self.list_view.setGridSize(QSize(120, 140))
        self.list_view.setIconSize(QSize(100, 100))

        self.model = ImageListModel()
        self.list_view.setModel(self.model)

        layout.addWidget(self.list_view)

    def set_images(self, images: list[Path]):
        self.model.set_images(images)

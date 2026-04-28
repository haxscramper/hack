from __future__ import annotations

from beartype import beartype
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
    QMenu,
    QWidget,
    QVBoxLayout,
    QListView,
    QDialog,
    QVBoxLayout as QDialogVBoxLayout,
    QDialogButtonBox,
    QLabel,
)

from image_tagger.config import config
from image_tagger.utils.utils import confirm_clear_selection

IMAGE_EXTENSIONS = config.IMAGE_EXTENSIONS

SELECTION_CLEAR_CONFIRM_THRESHOLD = 3

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

    def set_images(self, images: list[Path | str]):
        self.beginResetModel()
        self.images = [Path(p) if isinstance(p, str) else p for p in images]
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


class ConfirmClearListView(QListView):
    """QListView that shows a confirmation dialog before clearing a large selection."""

    def mousePressEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            modifiers = event.modifiers()
            # Only check for plain left-clicks (no Ctrl/Shift)
            if not (
                modifiers & Qt.KeyboardModifier.ControlModifier
                or modifiers & Qt.KeyboardModifier.ShiftModifier
            ):
                index = self.indexAt(event.position().toPoint())
                selection_model = self.selectionModel()
                if selection_model is not None:
                    selected = selection_model.selectedIndexes()
                    # If clicking on an already selected item, Qt will keep the selection
                    # If clicking on a non-selected item and we have > threshold selected
                    if (
                        not selection_model.isSelected(index)
                        and len(selected) > SELECTION_CLEAR_CONFIRM_THRESHOLD
                    ):
                        if not confirm_clear_selection(self, len(selected)):
                            event.accept()
                            return
        super().mousePressEvent(event)


class ImageListWidget(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self.list_view = ConfirmClearListView()
        self.list_view.setViewMode(QListView.ViewMode.IconMode)
        self.list_view.setResizeMode(QListView.ResizeMode.Adjust)
        self.list_view.setSelectionMode(QListView.SelectionMode.ExtendedSelection)
        self.list_view.setSelectionBehavior(QListView.SelectionBehavior.SelectItems)
        self.list_view.setWordWrap(True)
        self.list_view.setUniformItemSizes(True)
        self.list_view.setGridSize(QSize(120, 140))
        self.list_view.setIconSize(QSize(100, 100))

        self.model = ImageListModel()
        self.list_view.setModel(self.model)

        self.list_view.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.list_view.customContextMenuRequested.connect(self._show_context_menu)

        layout.addWidget(self.list_view)

    def _show_context_menu(self, pos):
        index = self.list_view.indexAt(pos)
        if not index.isValid():
            return

        selection_model = self.list_view.selectionModel()
        if not selection_model.isSelected(index):
            from PySide6.QtCore import QItemSelectionModel

            selection_model.clearSelection()
            selection_model.select(
                index,
                QItemSelectionModel.SelectionFlag.Select
                | QItemSelectionModel.SelectionFlag.Current,
            )

        indexes = selection_model.selectedIndexes()

        menu = QMenu(self)
        sxiv_action = menu.addAction("open in sxiv")

        def open_sxiv():
            import subprocess

            paths = [str(self.model.images[idx.row()]) for idx in indexes]
            subprocess.Popen(["sxiv"] + paths)

        sxiv_action.triggered.connect(open_sxiv)

        copy_path_action = menu.addAction("copy path")

        def copy_path():
            from PySide6.QtWidgets import QApplication

            paths = "\n".join(
                str(self.model.images[idx.row()].resolve()) for idx in indexes
            )
            QApplication.clipboard().setText(paths)

        copy_path_action.triggered.connect(copy_path)

        menu.exec(self.list_view.viewport().mapToGlobal(pos))

    def set_images(self, images: list[Path | str]):
        self.model.set_images(list(images))

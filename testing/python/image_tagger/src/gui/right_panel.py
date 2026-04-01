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
    QHBoxLayout,
    QLabel,
    QPushButton,
    QListWidget,
    QListWidgetItem,
    QListView,
    QFrame,
    QSplitter,
    QSizePolicy,
)

from config import IMAGE_EXTENSIONS


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


class DirectorySelectorWidget(QWidget):
    directoryChanged = Signal(Path)
    removeRequested = Signal()

    def __init__(self, root_dir: Path, initial_dir: Path | None = None, parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.current_dir = initial_dir or root_dir

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self.top_widget = QWidget()
        self.top_widget.setSizePolicy(
            QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed
        )
        self.top_layout = QHBoxLayout(self.top_widget)
        self.top_layout.setContentsMargins(0, 0, 0, 0)
        self.top_layout.setSpacing(0)

        self.path_bar = QHBoxLayout()
        self.path_bar.setContentsMargins(0, 0, 0, 0)
        self.path_bar.setSpacing(0)

        self.remove_btn = QPushButton("x")
        self.remove_btn.setFixedWidth(24)
        self.remove_btn.setFlat(True)
        self.remove_btn.setStyleSheet(
            "QPushButton { border: none; padding: 2px; margin: 0px; } QPushButton:hover { background: rgba(255, 0, 0, 0.2); border-radius: 2px; }"
        )
        self.remove_btn.clicked.connect(self.removeRequested.emit)

        self.top_layout.addLayout(self.path_bar, 1)
        self.top_layout.addWidget(self.remove_btn, 0)

        self.subdir_list = QListWidget()
        self.subdir_list.setFrameShape(QFrame.Shape.NoFrame)
        self.subdir_list.setVerticalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff
        )
        self.subdir_list.setStyleSheet(
            "QListWidget { background: transparent; } QListWidget::item { padding: 2px; }"
        )
        self.subdir_list.setSizePolicy(
            QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Maximum
        )

        layout.addWidget(self.top_widget)
        layout.addWidget(self.subdir_list)

        self.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Maximum)

        self.subdir_list.itemClicked.connect(self._on_subdir_clicked)
        self.refresh()

    def refresh(self):
        while self.path_bar.count():
            item = self.path_bar.takeAt(0)
            assert item
            w = item.widget()
            if w is not None:
                w.deleteLater()  # type: ignore

        rel_parts = (
            self.current_dir.relative_to(self.root_dir).parts
            if self.current_dir != self.root_dir
            else ()
        )
        accumulated = self.root_dir

        btn_style = "QPushButton { border: none; padding: 2px 4px; margin: 0px; background: transparent; } QPushButton:hover { background: rgba(128, 128, 128, 0.2); border-radius: 2px; }"

        root_btn = QPushButton(self.root_dir.name)
        root_btn.setFlat(True)
        root_btn.setStyleSheet(btn_style)
        root_btn.clicked.connect(lambda: self._set_dir(self.root_dir))
        self.path_bar.addWidget(root_btn)

        for part in rel_parts:
            arrow = QLabel(">")
            arrow.setStyleSheet("margin: 0px 2px; padding: 0px; color: gray;")
            self.path_bar.addWidget(arrow)
            accumulated = accumulated / part
            btn = QPushButton(part)
            btn.setFlat(True)
            btn.setStyleSheet(btn_style)
            btn.clicked.connect(lambda checked=False, p=accumulated: self._set_dir(p))
            self.path_bar.addWidget(btn)

        self.path_bar.addStretch(1)

        self.subdir_list.clear()
        count = 0
        for child in sorted(self.current_dir.iterdir()):
            if child.is_dir():
                item = QListWidgetItem(child.name)
                item.setData(Qt.ItemDataRole.UserRole, str(child))
                self.subdir_list.addItem(item)
                count += 1

        if count > 0:
            self.subdir_list.show()
            self.subdir_list.doItemsLayout()
            row_height = self.subdir_list.sizeHintForRow(0)
            if row_height <= 0:
                row_height = 20
            self.subdir_list.setFixedHeight(row_height * count)
        else:
            self.subdir_list.hide()

    def _set_dir(self, path: Path):
        self.current_dir = path
        self.refresh()
        self.directoryChanged.emit(self.current_dir)

    def _on_subdir_clicked(self, item: QListWidgetItem):
        self._set_dir(Path(item.data(Qt.ItemDataRole.UserRole)))


class DirectoryPreviewWidget(QFrame):
    removeRequested = Signal(QWidget)

    def __init__(self, root_dir: Path, parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.current_dir = root_dir
        self.setFrameShape(QFrame.Shape.StyledPanel)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self.selector = DirectorySelectorWidget(root_dir)
        layout.addWidget(self.selector)

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

        self.selector.directoryChanged.connect(self.set_directory)
        self.selector.removeRequested.connect(lambda: self.removeRequested.emit(self))
        self.set_directory(root_dir)

    def set_directory(self, path: Path):
        self.current_dir = path

        images = [
            p
            for p in sorted(path.iterdir())
            if p.is_file() and p.suffix.lower() in IMAGE_EXTENSIONS
        ]

        self.model.set_images(images)


class RightPanel(QWidget):
    def __init__(self, root_dir: Path, parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.main_layout = QVBoxLayout(self)
        self.splitter = QSplitter(Qt.Orientation.Vertical)
        self.main_layout.addWidget(self.splitter)

        self.add_btn = QPushButton("+")
        self.add_btn.clicked.connect(self.add_preview_widget)
        self.main_layout.addWidget(self.add_btn)

        self.add_preview_widget()

    def add_preview_widget(self):
        widget = DirectoryPreviewWidget(self.root_dir)
        widget.removeRequested.connect(self.remove_preview_widget)
        self.splitter.addWidget(widget)

    def remove_preview_widget(self, widget):
        widget.setParent(None)
        widget.deleteLater()

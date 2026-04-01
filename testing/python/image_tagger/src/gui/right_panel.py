from __future__ import annotations

from pathlib import Path

from PySide6.QtCore import Signal, Qt
from PySide6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QListWidget,
    QListWidgetItem,
    QGridLayout,
    QScrollArea,
    QFrame,
)

from gui.image_preview import ImageThumbWidget
from config import IMAGE_EXTENSIONS, PREVIEW_GRID_COLUMNS


class DirectorySelectorWidget(QWidget):
    directoryChanged = Signal(Path)

    def __init__(self, root_dir: Path, initial_dir: Path | None = None, parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.current_dir = initial_dir or root_dir

        layout = QVBoxLayout(self)
        self.path_bar = QHBoxLayout()
        self.subdir_list = QListWidget()

        layout.addLayout(self.path_bar)
        layout.addWidget(self.subdir_list)

        self.subdir_list.itemClicked.connect(self._on_subdir_clicked)
        self.refresh()

    def refresh(self):
        while self.path_bar.count():
            item = self.path_bar.takeAt(0)
            if item.widget():
                item.widget().deleteLater()

        rel_parts = self.current_dir.relative_to(self.root_dir).parts if self.current_dir != self.root_dir else ()
        accumulated = self.root_dir

        root_btn = QPushButton(self.root_dir.name)
        root_btn.clicked.connect(lambda: self._set_dir(self.root_dir))
        self.path_bar.addWidget(root_btn)

        for part in rel_parts:
            arrow = QLabel(">")
            self.path_bar.addWidget(arrow)
            accumulated = accumulated / part
            btn = QPushButton(part)
            btn.clicked.connect(lambda checked=False, p=accumulated: self._set_dir(p))
            self.path_bar.addWidget(btn)

        self.path_bar.addStretch(1)

        self.subdir_list.clear()
        for child in sorted(self.current_dir.iterdir()):
            if child.is_dir():
                item = QListWidgetItem(child.name)
                item.setData(Qt.UserRole, str(child))
                self.subdir_list.addItem(item)

    def _set_dir(self, path: Path):
        self.current_dir = path
        self.refresh()
        self.directoryChanged.emit(self.current_dir)

    def _on_subdir_clicked(self, item: QListWidgetItem):
        self._set_dir(Path(item.data(Qt.UserRole)))


class DirectoryPreviewWidget(QFrame):
    removeRequested = Signal(QWidget)

    def __init__(self, root_dir: Path, parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.current_dir = root_dir
        self.setFrameShape(QFrame.StyledPanel)

        layout = QVBoxLayout(self)

        top_row = QHBoxLayout()
        self.selector = DirectorySelectorWidget(root_dir)
        self.remove_btn = QPushButton("x")
        self.remove_btn.setFixedWidth(30)
        top_row.addWidget(self.selector, 1)
        top_row.addWidget(self.remove_btn, 0)
        layout.addLayout(top_row)

        self.preview_container = QWidget()
        self.preview_grid = QGridLayout(self.preview_container)
        self.preview_scroll = QScrollArea()
        self.preview_scroll.setWidgetResizable(True)
        self.preview_scroll.setWidget(self.preview_container)
        layout.addWidget(self.preview_scroll)

        self.selector.directoryChanged.connect(self.set_directory)
        self.remove_btn.clicked.connect(lambda: self.removeRequested.emit(self))
        self.set_directory(root_dir)

    def set_directory(self, path: Path):
        self.current_dir = path
        while self.preview_grid.count():
            item = self.preview_grid.takeAt(0)
            if item.widget():
                item.widget().deleteLater()

        images = [
            p for p in sorted(path.iterdir())
            if p.is_file() and p.suffix.lower() in IMAGE_EXTENSIONS
        ]

        for idx, image_path in enumerate(images):
            row = idx // PREVIEW_GRID_COLUMNS
            col = idx % PREVIEW_GRID_COLUMNS
            self.preview_grid.addWidget(ImageThumbWidget(image_path, size=100), row, col)


class RightPanel(QWidget):
    def __init__(self, root_dir: Path, parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.layout = QVBoxLayout(self)
        self.widgets_layout = QVBoxLayout()
        self.layout.addLayout(self.widgets_layout)

        self.add_btn = QPushButton("+")
        self.add_btn.clicked.connect(self.add_preview_widget)
        self.layout.addWidget(self.add_btn)

        self.add_preview_widget()

    def add_preview_widget(self):
        widget = DirectoryPreviewWidget(self.root_dir)
        widget.removeRequested.connect(self.remove_preview_widget)
        self.widgets_layout.addWidget(widget)

    def remove_preview_widget(self, widget):
        self.widgets_layout.removeWidget(widget)
        widget.deleteLater()

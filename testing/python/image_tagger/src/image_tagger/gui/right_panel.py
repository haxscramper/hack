from __future__ import annotations

from beartype import beartype
from pathlib import Path

from PySide6.QtCore import Signal, Qt, QSize
from PySide6.QtGui import QColor
from PySide6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QListWidget,
    QListWidgetItem,
    QFrame,
    QSplitter,
    QSizePolicy,
)

from image_tagger.gui.image_list_widget import ImageListWidget
from image_tagger.config import config
from image_tagger.gui.state_models import DirectorySelectorState

IMAGE_EXTENSIONS = config.IMAGE_EXTENSIONS


def compute_relevant_paths(root_dir: Path, paths: list[Path]) -> list[str]:
    """Compute the relevant path suffix for each path based on commonality.

    - If all paths differ only in the last component, return last component.
    - If paths share a common directory under root, return suffix after that common dir.
    - If no common parts, return path relative to root.
    """
    if not paths:
        return []

    rel_parts_list = []
    for p in paths:
        try:
            rel = p.relative_to(root_dir)
            parts = rel.parts
        except ValueError:
            parts = p.parts
        rel_parts_list.append(parts)

    if not rel_parts_list:
        return []

    # Find longest common prefix
    min_len = min(len(parts) for parts in rel_parts_list)
    common_len = 0
    for i in range(min_len):
        if all(parts[i] == rel_parts_list[0][i] for parts in rel_parts_list):
            common_len = i + 1
        else:
            break

    # If all paths differ only in the last component (same depth, same prefix except last)
    if all(len(parts) == len(rel_parts_list[0]) for parts in rel_parts_list):
        first_parts = rel_parts_list[0]
        if len(first_parts) >= 1 and common_len == len(first_parts) - 1:
            return [parts[-1] for parts in rel_parts_list]

    if common_len > 0:
        result = ["/".join(parts[common_len:]) for parts in rel_parts_list]
        return [r if r else str(paths[i].name) for i, r in enumerate(result)]

    # No common parts - return relative to root
    result = ["/".join(parts) for parts in rel_parts_list]
    # Ensure we never return empty strings; fall back to path name
    return [r if r else str(paths[i].name) for i, r in enumerate(result)]


class DirectorySelectorWidget(QWidget):
    directoryChanged = Signal(Path)
    removeRequested = Signal()

    def __init__(self,
                 root_dir: Path,
                 initial_dir: Path | None = None,
                 parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.current_dir = initial_dir or root_dir

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self.top_widget = QWidget()
        self.top_widget.setSizePolicy(QSizePolicy.Policy.Preferred,
                                      QSizePolicy.Policy.Fixed)
        self.top_layout = QHBoxLayout(self.top_widget)
        self.top_layout.setContentsMargins(0, 0, 0, 0)
        self.top_layout.setSpacing(0)

        self.index_label = QLabel()
        self.index_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.index_label.setFixedSize(24, 24)
        self.index_label.hide()

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

        self.top_layout.addWidget(self.index_label, 0)
        self.top_layout.addLayout(self.path_bar, 1)
        self.top_layout.addWidget(self.remove_btn, 0)

        self.subdir_list = QListWidget()
        self.subdir_list.setFrameShape(QFrame.Shape.NoFrame)
        self.subdir_list.setVerticalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.subdir_list.setStyleSheet(
            "QListWidget { background: transparent; } QListWidget::item { padding: 2px; }"
        )
        self.subdir_list.setSizePolicy(QSizePolicy.Policy.Preferred,
                                       QSizePolicy.Policy.Maximum)

        layout.addWidget(self.top_widget)
        layout.addWidget(self.subdir_list)

        self.setSizePolicy(QSizePolicy.Policy.Preferred,
                           QSizePolicy.Policy.Maximum)

        self.subdir_list.itemClicked.connect(self._on_subdir_clicked)
        self.refresh()

    def set_index(self, index: int, color: str):
        self.index_label.setText(str(index))
        self.index_label.setStyleSheet(
            f"background-color: {color}; color: black; border-radius: 4px; font-weight: bold; margin-right: 4px;"
        )
        self.index_label.show()

    def refresh(self):
        while self.path_bar.count():
            item = self.path_bar.takeAt(0)
            assert item
            w = item.widget()
            if w is not None:
                w.deleteLater()  # type: ignore

        rel_parts = (self.current_dir.relative_to(self.root_dir).parts
                     if self.current_dir != self.root_dir else ())
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
            btn.clicked.connect(
                lambda checked=False, p=accumulated: self._set_dir(p))
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
            max_visible = 4
            self.subdir_list.setFixedHeight(row_height *
                                            min(count, max_visible))
        else:
            self.subdir_list.hide()

    def _set_dir(self, path: Path):
        self.current_dir = path
        self.refresh()
        self.directoryChanged.emit(self.current_dir)

    def _on_subdir_clicked(self, item: QListWidgetItem):
        self._set_dir(Path(item.data(Qt.ItemDataRole.UserRole)))

    def get_state(self) -> DirectorySelectorState:
        from image_tagger.gui.state_models import DirectorySelectorState

        return DirectorySelectorState(current_dir=str(self.current_dir))

    def set_state(self, state: DirectorySelectorState) -> None:
        self._set_dir(Path(state.current_dir))


class DirectoryPreviewWidget(QFrame):
    removeRequested = Signal(QWidget)

    def __init__(self, root_dir: Path, parent=None):
        super().__init__(parent)
        self.root_dir = root_dir
        self.current_dir = root_dir
        self._index_color = ""
        self._relevant_path_text = ""
        self.setFrameShape(QFrame.Shape.StyledPanel)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self.selector = DirectorySelectorWidget(root_dir)
        layout.addWidget(self.selector)

        self.image_list = ImageListWidget()
        layout.addWidget(self.image_list)

        self.selector.directoryChanged.connect(self.set_directory)
        self.selector.removeRequested.connect(
            lambda: self.removeRequested.emit(self))
        self.set_directory(root_dir)

        # Overlay for move dialog highlighting
        self.overlay_widget = QFrame(self)
        self.overlay_widget.setAttribute(
            Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        self.overlay_widget.hide()

        overlay_layout = QVBoxLayout(self.overlay_widget)
        overlay_layout.setContentsMargins(0, 0, 0, 0)
        overlay_layout.setAlignment(Qt.AlignmentFlag.AlignCenter)

        self.overlay_label = QLabel()
        self.overlay_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        overlay_layout.addWidget(self.overlay_label)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        self.overlay_widget.setGeometry(self.rect())

    def set_index(self, index: int, color: str):
        self.selector.set_index(index, color)
        self._index_color = color

    def set_relevant_path_text(self, text: str):
        self._relevant_path_text = text

    def show_move_overlay(self):
        if self._index_color:
            self.overlay_widget.setStyleSheet(
                f"QFrame {{ background-color: {self._index_color}; }}")
        font = self.overlay_label.font()
        font.setPointSize(24)
        font.setBold(True)
        self.overlay_label.setFont(font)
        self.overlay_label.setStyleSheet(
            "color: black; background: transparent;")
        self.overlay_label.setText(self._relevant_path_text)
        self.overlay_widget.setGeometry(self.rect())
        self.overlay_widget.show()
        self.overlay_widget.raise_()
        self.overlay_widget.update()
        self.overlay_label.update()

    def hide_move_overlay(self):
        self.overlay_widget.hide()

    def set_directory(self, path: Path):
        self.current_dir = path

        images = [
            p for p in sorted(path.iterdir())
            if p.is_file() and p.suffix.lower() in IMAGE_EXTENSIONS
        ]

        self.image_list.set_images(images)

    def get_state(self) -> "DirectoryPreviewState":
        from image_tagger.gui.state_models import DirectoryPreviewState

        return DirectoryPreviewState(selector=self.selector.get_state())

    def set_state(self, state: "DirectoryPreviewState") -> None:
        self.selector.set_state(state.selector)


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

    def _update_indices(self):
        for i in range(self.splitter.count()):
            widget = self.splitter.widget(i)
            if isinstance(widget, DirectoryPreviewWidget):
                hue = (i * 137) % 360
                color = QColor.fromHsv(hue, 120, 255).name()
                widget.set_index(i + 1, color)

    def get_preview_widget_paths(self) -> list[Path]:
        paths = []
        for i in range(self.splitter.count()):
            widget = self.splitter.widget(i)
            if isinstance(widget, DirectoryPreviewWidget):
                paths.append(widget.current_dir)
        return paths

    def update_relevant_paths(self):
        paths = self.get_preview_widget_paths()
        relevant = compute_relevant_paths(self.root_dir, paths)
        idx = 0
        for i in range(self.splitter.count()):
            widget = self.splitter.widget(i)
            if isinstance(widget, DirectoryPreviewWidget):
                if idx < len(relevant):
                    widget.set_relevant_path_text(relevant[idx])
                idx += 1

    def show_move_overlay(self, target_index: int):
        self.update_relevant_paths()
        for i in range(self.splitter.count()):
            widget = self.splitter.widget(i)
            if isinstance(widget, DirectoryPreviewWidget):
                if i == target_index:
                    widget.show_move_overlay()
                else:
                    widget.hide_move_overlay()

    def hide_move_overlays(self):
        for i in range(self.splitter.count()):
            widget = self.splitter.widget(i)
            if isinstance(widget, DirectoryPreviewWidget):
                widget.hide_move_overlay()

    def add_preview_widget(self):
        widget = DirectoryPreviewWidget(self.root_dir)
        widget.removeRequested.connect(self.remove_preview_widget)
        self.splitter.addWidget(widget)
        self._update_indices()

    def remove_preview_widget(self, widget):
        widget.setParent(None)
        widget.deleteLater()
        self._update_indices()

    def get_state(self) -> "RightPanelState":
        from image_tagger.gui.state_models import RightPanelState

        preview_widgets = []
        for i in range(self.splitter.count()):
            w = self.splitter.widget(i)
            if isinstance(w, DirectoryPreviewWidget):
                preview_widgets.append(w.get_state())

        return RightPanelState(
            preview_widgets=preview_widgets,
            splitter_sizes=self.splitter.sizes(),
        )

    def set_state(self, state: "RightPanelState") -> None:
        # Remove existing preview widgets
        while self.splitter.count():
            w = self.splitter.widget(0)
            if w is not None:
                w.setParent(None)
                w.deleteLater()

        for preview_state in state.preview_widgets:
            widget = DirectoryPreviewWidget(self.root_dir)
            widget.removeRequested.connect(self.remove_preview_widget)
            widget.set_state(preview_state)
            self.splitter.addWidget(widget)

        self._update_indices()

        if state.splitter_sizes:
            self.splitter.setSizes(state.splitter_sizes)

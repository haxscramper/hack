from __future__ import annotations

from pathlib import Path

from PySide6.QtWidgets import (
    QDialog,
    QVBoxLayout,
    QHBoxLayout,
    QDialogButtonBox,
    QLabel,
    QListView,
)
from PySide6.QtCore import Qt, QSize, Signal
from PySide6.QtGui import QKeySequence, QShortcut

from image_tagger.gui.image_list_widget import ImageListModel


class MoveDialog(QDialog):
    """Dialog for confirming file move with target-switching shortcuts."""

    moveConfirmed = Signal(int, list, Path)
    outputIndexChanged = Signal(int)

    def __init__(
        self,
        selected_files: list[Path],
        targets: list[tuple[Path, str, str]],
        current_index: int,
        parent=None,
    ):
        super().__init__(parent)
        self.selected_files = selected_files
        self.targets = targets
        self.current_index = current_index

        self._build_ui()
        self._setup_shortcuts()
        self._update_target(self.current_index)

    def _build_ui(self):
        self.layout = QVBoxLayout(self)

        self.resize(600, 400)
        self.top_layout = QHBoxLayout()

        self.count_label = QLabel()
        self.top_layout.addWidget(self.count_label)

        self.path_label = QLabel()
        self.path_label.setStyleSheet(
            "font-weight: bold; padding: 4px 8px; border-radius: 4px;")
        self.top_layout.addWidget(self.path_label)
        self.top_layout.addStretch(1)
        self.layout.addLayout(self.top_layout)

        self.list_view = QListView()
        self.list_view.setViewMode(QListView.ViewMode.IconMode)
        self.list_view.setResizeMode(QListView.ResizeMode.Adjust)
        self.list_view.setWordWrap(True)
        self.list_view.setUniformItemSizes(True)
        self.list_view.setGridSize(QSize(120, 140))
        self.list_view.setIconSize(QSize(100, 100))

        self.model = ImageListModel()
        self.model.set_images(self.selected_files)
        self.list_view.setModel(self.model)

        self.layout.addWidget(self.list_view)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok
                                   | QDialogButtonBox.StandardButton.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        self.layout.addWidget(buttons)

    def _setup_shortcuts(self):
        for i in range(1, 10):
            shortcut = QShortcut(QKeySequence(f"Ctrl+{i}"), self)
            shortcut.activated.connect(lambda idx=i: self._on_shortcut(idx))

    def _on_shortcut(self, idx: int):
        target_index = idx - 1
        if 0 <= target_index < len(self.targets):
            self._update_target(target_index)

        self.outputIndexChanged.emit(target_index)

    def _update_target(self, index: int):
        self.current_index = index
        target_dir, target_color, target_relevant_path = self.targets[index]

        self.setWindowTitle(f"Move to {target_dir.name}?")
        self.count_label.setText(f"Move {len(self.selected_files)} items to")

        display_text = target_relevant_path or target_dir.name
        self.path_label.setText(display_text)
        if target_color:
            self.path_label.setStyleSheet(
                f"background-color: {target_color}; color: black; "
                f"border-radius: 4px; font-weight: bold; padding: 4px 8px;")
        else:
            self.path_label.setStyleSheet(
                "font-weight: bold; padding: 4px 8px; border-radius: 4px;")

    def done(self, result: int):
        target_dir = self.targets[self.current_index][0]
        self.moveConfirmed.emit(result, self.selected_files, target_dir)
        super().done(result)

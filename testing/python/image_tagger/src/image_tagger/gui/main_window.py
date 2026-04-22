from __future__ import annotations
import logging

from beartype import beartype
from pathlib import Path

from PySide6.QtWidgets import (
    QMainWindow,
    QTableWidget,
    QWidget,
    QHBoxLayout,
    QSplitter,
    QDialog,
    QVBoxLayout,
    QDialogButtonBox,
    QLabel,
    QListView,
)
from PySide6.QtCore import Qt, QSize
from PySide6.QtGui import QUndoStack, QUndoCommand, QKeySequence, QShortcut

from image_tagger.db.models import ImageEntry
from sqlalchemy import select
import shutil

from image_tagger.db.repository import Repository, get_md5
from image_tagger.gui.image_directory_view import MixedTreeTileView
from image_tagger.gui.image_list_widget import ImageListModel
from image_tagger.gui.left_panel import LeftPanel

from image_tagger.gui.center_panel import CenterPanel
from image_tagger.gui.right_panel import RightPanel
from image_tagger.gui.state_models import AppState


class MoveFilesCommand(QUndoCommand):
    def __init__(
        self,
        repository: Repository,
        source_files: list[Path],
        target_dir: Path,
        root_dir: Path,
        main_window: "MainWindow",
        parent=None,
    ):
        super().__init__(parent)
        self.repository = repository
        self.source_files = source_files
        self.target_dir = target_dir
        self.root_dir = root_dir
        self.main_window = main_window

        self.moves = []
        used_names = set()
        for src in sorted(self.source_files):
            dest = self.target_dir / src.name
            if src == dest:
                continue
            if dest.exists() or src.name in used_names:
                md5 = get_md5(src)
                suffix = md5[:8]
                stem = src.stem
                ext = src.suffix
                new_name = f"{stem}_{suffix}{ext}"
                dest = self.target_dir / new_name
                while dest.exists() or new_name in used_names:
                    md5 = get_md5(src)
                    suffix = md5[:8]
                    new_name = f"{stem}_{suffix}{ext}"
                    dest = self.target_dir / new_name
            used_names.add(dest.name)
            self.moves.append((src, dest))

        self.setText(f"Move {len(self.moves)} files to {self.target_dir.name}")

    def _update_db(self, old_path: Path, new_path: Path):
        old_rel = str(old_path.resolve().relative_to(self.root_dir.resolve()))
        entry = self.repository.session.scalar(
            select(ImageEntry).where(ImageEntry.relative_path == old_rel)
        )
        if entry:
            entry.relative_path = str(
                new_path.resolve().relative_to(self.root_dir.resolve())
            )
            entry.original_name = new_path.name
            self.repository.session.commit()

    def _refresh_ui(self):
        self.main_window.left_panel.selected_files = {dest for src, dest in self.moves}
        self.main_window.left_panel.tree_view.refresh()
        self.main_window.left_panel.viewport().update()

        for i in range(self.main_window.right_panel.splitter.count()):
            w = self.main_window.right_panel.splitter.widget(i)
            if hasattr(w, "current_dir"):
                w.set_directory(w.current_dir)

    def redo(self):
        for src, dest in self.moves:
            try:
                shutil.move(str(src), str(dest))
                self._update_db(src, dest)
            except Exception as e:
                logging.error(f"Failed to move {src} to {dest}: {e}")
        self._refresh_ui()

    def undo(self):
        for src, dest in self.moves:
            try:
                shutil.move(str(dest), str(src))
                self._update_db(dest, src)
            except Exception as e:
                logging.error(f"Failed to move back {dest} to {src}: {e}")
        self._refresh_ui()


class MainWindow(QMainWindow):
    def get_mixed_view(self) -> MixedTreeTileView:
        return self.left_panel.tree_view

    def get_probability_tags_table(self) -> QTableWidget:
        return self.center_panel.prob_table

    def __init__(self, root_dir: Path, repository: Repository):
        super().__init__()
        self.root_dir = root_dir
        self.repository = repository
        self.current_image = None
        logging.info(f"MainWindow initialized with root_dir={root_dir}")

        self.setWindowTitle("Image Tagger")
        self.resize(1600, 900)

        central = QWidget()
        self.setCentralWidget(central)
        layout = QHBoxLayout(central)

        splitter = QSplitter(Qt.Orientation.Horizontal)

        self.left_panel = LeftPanel(root_dir, self.repository.session)
        self.center_panel = CenterPanel()
        self.right_panel = RightPanel(root_dir)

        self._update_fully_annotated()

        splitter.addWidget(self.left_panel)
        splitter.addWidget(self.center_panel)
        splitter.addWidget(self.right_panel)
        splitter.setSizes([350, 700, 550])

        layout.addWidget(splitter)

        self.left_panel.fileSelected.connect(self.on_file_selected)
        self.center_panel.probabilisticTagAdded.connect(self.on_prob_tag_added)
        self.center_panel.regularTagAdded.connect(self.on_regular_tag_added)
        self.center_panel.regularTagDeleted.connect(self.on_regular_tag_deleted)
        self.center_panel.descriptionSaved.connect(self.on_description_saved)
        self.center_panel.probTagSearchRequested.connect(
            self.on_prob_tag_search_requested
        )
        self.center_panel.regTagSearchRequested.connect(
            self.on_reg_tag_search_requested
        )

        self.undo_stack = QUndoStack(self)
        undo_action = self.undo_stack.createUndoAction(self, "Undo")
        undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self.addAction(undo_action)

        for i in range(1, 10):
            shortcut = QShortcut(QKeySequence(f"Ctrl+{i}"), self)
            shortcut.activated.connect(lambda idx=i: self.on_move_shortcut(idx))

    def _update_fully_annotated(self):
        rel_paths = self.repository.get_fully_annotated_paths()
        full_paths = {self.root_dir / p for p in rel_paths}
        self.left_panel.fully_annotated_files = full_paths
        self.left_panel.viewport().update()

    def create_move_dialog(self, selected_files, target_dir):
        dialog = QDialog(self)
        dialog.setWindowTitle(f"Move to {target_dir.name}?")
        dialog.resize(600, 400)

        layout = QVBoxLayout(dialog)

        label = QLabel(f"Move {len(selected_files)} items to {target_dir}?")
        layout.addWidget(label)

        list_view = QListView()
        list_view.setViewMode(QListView.ViewMode.IconMode)
        list_view.setResizeMode(QListView.ResizeMode.Adjust)
        list_view.setWordWrap(True)
        list_view.setUniformItemSizes(True)
        list_view.setGridSize(QSize(120, 140))
        list_view.setIconSize(QSize(100, 100))

        model = ImageListModel()
        model.set_images(selected_files)
        list_view.setModel(model)

        layout.addWidget(list_view)

        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        return dialog

    def on_move_dialog_finished(self, result, selected_files, target_dir):
        if result != QDialog.DialogCode.Accepted:
            return

        command = MoveFilesCommand(
            self.repository,
            selected_files,
            target_dir,
            self.root_dir,
            self,
        )

        self.undo_stack.push(command)

        # Clear selection in the tree view
        self.left_panel.tree_view.selected_files = set()
        self.left_panel.tree_view.viewport().update()

        # Re-run query in search view to clear selection there
        self.left_panel.search_view.execute_search()

    def on_move_shortcut(self, idx: int):
        if idx - 1 >= self.right_panel.splitter.count():
            return

        widget = self.right_panel.splitter.widget(idx - 1)
        if not widget or not hasattr(widget, "current_dir"):
            return

        target_dir = getattr(widget, "current_dir")
        selected_files = list(self.left_panel.selected_files)

        if not selected_files:
            return

        dialog = self.create_move_dialog(selected_files, target_dir)
        dialog.finished.connect(
            lambda result: self.on_move_dialog_finished(
                result, selected_files, target_dir
            )
        )
        dialog.open()

    def on_file_selected(self, file_path: str):
        logging.debug(f"File selected in GUI: {file_path}")
        try:
            rel_path = str(
                Path(file_path).resolve().relative_to(self.root_dir.resolve())
            )
        except ValueError:
            # Fallback if somehow it's already a relative path string
            rel_path = file_path

        entry = self.repository.get_image_by_path(rel_path)
        if entry is None:
            entry = self.repository.upsert_image(self.root_dir, Path(file_path))

        self.current_image = entry

        prob_rows = self.repository.list_probabilistic_tags(entry.id)
        prob_tags = [
            (tag.category, tag.name, rel.probability) for rel, tag in prob_rows
        ]
        self.center_panel.set_probabilistic_tags(prob_tags)

        reg_rows = self.repository.list_regular_tags(entry.id)
        reg_tags = [(tag.category, tag.name) for rel, tag in reg_rows]
        self.center_panel.set_regular_tags(reg_tags)

        desc = self.repository.get_description(entry.id)
        self.center_panel.set_description(desc.description if desc else "")

    def on_prob_tag_added(self, name: str, probability: float):
        if not self.current_image:
            return
        logging.info(
            f"Adding prob tag '{name}' with prob {probability} to image {self.current_image.id}"
        )
        self.repository.set_probabilistic_tag(self.current_image.id, name, probability)
        self._update_fully_annotated()
        self.on_file_selected(str(self.root_dir / self.current_image.relative_path))

    def on_regular_tag_added(self, category: str, name: str):
        if not self.current_image:
            return
        logging.info(
            f"Adding regular tag '{category}:{name}' to image {self.current_image.id}"
        )
        self.repository.add_regular_tag(self.current_image.id, category, name)
        self._update_fully_annotated()
        self.on_file_selected(str(self.root_dir / self.current_image.relative_path))

    def on_regular_tag_deleted(self, category: str, name: str):
        if not self.current_image:
            return
        logging.info(
            f"Deleting regular tag '{category}:{name}' from image {self.current_image.id}"
        )
        self.repository.delete_regular_tag(self.current_image.id, category, name)
        self._update_fully_annotated()
        self.on_file_selected(str(self.root_dir / self.current_image.relative_path))

    def on_description_saved(self, text: str):
        if not self.current_image:
            return
        logging.info(f"Saving description for image {self.current_image.id}")
        self.repository.set_description(self.current_image.id, text)
        self._update_fully_annotated()

    def on_prob_tag_search_requested(self, category: str, name: str):
        self.left_panel.tabs.setCurrentIndex(1)
        self.left_panel.search_view.add_tag_to_query(
            "probabilistic_tag", category, name
        )

    def on_reg_tag_search_requested(self, category: str, name: str):
        self.left_panel.tabs.setCurrentIndex(1)
        self.left_panel.search_view.add_tag_to_query("regular_tag", category, name)

    def get_state(self) -> AppState:
        from image_tagger.gui.state_models import AppState

        # Find the main horizontal splitter
        central = self.centralWidget()
        splitter = None
        if central is not None:
            layout = central.layout()
            for i in range(layout.count()):
                item = layout.itemAt(i)
                if item and item.widget() and isinstance(item.widget(), QSplitter):
                    splitter = item.widget()
                    break

        splitter_sizes = splitter.sizes() if splitter is not None else []

        return AppState(
            window_size=(self.width(), self.height()),
            splitter_sizes=splitter_sizes,
            left_panel=self.left_panel.get_state(),
            center_panel=self.center_panel.get_state(),
            right_panel=self.right_panel.get_state(),
        )

    def set_state(self, state: AppState) -> None:
        self.resize(*state.window_size)

        central = self.centralWidget()
        splitter = None
        if central is not None:
            layout = central.layout()
            for i in range(layout.count()):
                item = layout.itemAt(i)
                if item and item.widget() and isinstance(item.widget(), QSplitter):
                    splitter = item.widget()
                    break

        if splitter is not None and state.splitter_sizes:
            splitter.setSizes(state.splitter_sizes)

        self.left_panel.set_state(state.left_panel)
        self.center_panel.set_state(state.center_panel)
        self.right_panel.set_state(state.right_panel)

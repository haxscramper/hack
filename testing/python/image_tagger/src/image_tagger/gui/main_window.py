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
from image_tagger.gui.right_panel import RightPanel, DirectoryPreviewWidget
from image_tagger.gui.state_models import AppState
from image_tagger.gui.fuzzy_file_selector import (
    PaletteDialog,
    build_directory_entries,
)
from image_tagger.gui.move_dialog import MoveDialog


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
        self.original_selection = set(source_files)

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
            select(ImageEntry).where(ImageEntry.relative_path == old_rel))
        if entry:
            entry.relative_path = str(new_path.resolve().relative_to(
                self.root_dir.resolve()))
            entry.original_name = new_path.name
            self.repository.session.commit()

    def _refresh_ui(self):
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
        moved_files = [dest for src, dest in self.moves]
        self.main_window._focus_after_move(moved_files)

    def undo(self):
        for src, dest in self.moves:
            try:
                shutil.move(str(dest), str(src))
                self._update_db(dest, src)
            except Exception as e:
                logging.error(f"Failed to move back {dest} to {src}: {e}")
        self._refresh_ui()
        self.main_window._focus_after_move(
            list(self.original_selection),
            selection_override=self.original_selection,
        )


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
        self.center_panel.regularTagDeleted.connect(
            self.on_regular_tag_deleted)
        self.center_panel.descriptionSaved.connect(self.on_description_saved)
        self.center_panel.probTagSearchRequested.connect(
            self.on_prob_tag_search_requested)
        self.center_panel.regTagSearchRequested.connect(
            self.on_reg_tag_search_requested)

        self.undo_stack = QUndoStack(self)
        undo_action = self.undo_stack.createUndoAction(self, "Undo")
        undo_action.setShortcut(QKeySequence("Ctrl+Z"))
        self.addAction(undo_action)

        redo_action = self.undo_stack.createRedoAction(self, "Redo")
        redo_action.setShortcut(QKeySequence("Ctrl+Shift+Z"))
        self.addAction(redo_action)

        for i in range(1, 10):
            shortcut = QShortcut(QKeySequence(f"Ctrl+{i}"), self)
            shortcut.activated.connect(
                lambda idx=i: self.on_move_shortcut(idx))

        self._palette_pinned_paths: set[str] = set()
        self._palette_recent_paths: list[str] = []

        palette_shortcut = QShortcut(QKeySequence("Ctrl+M"), self)
        palette_shortcut.activated.connect(self.on_palette_move_shortcut)

    def _update_fully_annotated(self):
        rel_paths = self.repository.get_fully_annotated_paths()
        full_paths = {self.root_dir / p for p in rel_paths}
        self.left_panel.fully_annotated_files = full_paths
        self.left_panel.viewport().update()

    def _build_move_targets(self) -> list[tuple[Path, str, str]]:
        """Build list of (target_dir, color, relevant_path) for all preview widgets."""
        targets = []
        self.right_panel.update_relevant_paths()
        for i in range(self.right_panel.splitter.count()):
            widget = self.right_panel.splitter.widget(i)
            if widget and hasattr(widget, "current_dir"):
                target_dir = getattr(widget, "current_dir")
                target_color = ""
                target_relevant_path = ""
                if isinstance(widget, DirectoryPreviewWidget):
                    target_color = widget._index_color
                    target_relevant_path = widget._relevant_path_text
                targets.append(
                    (target_dir, target_color, target_relevant_path))
        return targets

    def _focus_after_move(self, moved_files, selection_override=None):
        """Select an appropriate file after a move or undo operation.

        If *selection_override* is given it is used directly as the new
        selection set (used by undo to restore the original selection).
        Otherwise the first remaining file sharing a common parent with the
        moved files is selected.  When the search tab is visible its results
        are refreshed and the first result is also selected.
        """
        tree_view = self.left_panel.tree_view

        if selection_override is not None:
            new_selection = next(iter(selection_override), None)
        else:
            source_parents = [p.parent for p in moved_files]
            common_parent = source_parents[0] if source_parents else None
            for p in source_parents[1:]:
                while not str(p).startswith(str(common_parent)):
                    common_parent = common_parent.parent

            new_selection = None
            if common_parent is not None:
                for hit in tree_view.tile_hits:
                    if hit.file_path.parent == common_parent:
                        new_selection = hit.file_path
                        break

            if new_selection is None and tree_view.tile_hits:
                new_selection = tree_view.tile_hits[0].file_path

        if new_selection is not None:
            tree_view.selected_files = {new_selection}
            tree_view.last_clicked_file = new_selection
        else:
            tree_view.selected_files = set()

        tree_view.viewport().update()

        search_view = self.left_panel.search_view
        search_view.execute_search()
        if self.left_panel.tabs.currentIndex() == 1:
            results = search_view.get_result_images()
            if results:
                from PySide6.QtCore import QItemSelectionModel
                thumb_list = search_view.thumbnail_list
                index = thumb_list.model.index(0, 0)
                thumb_list.list_view.selectionModel().select(
                    index,
                    QItemSelectionModel.SelectionFlag.Select
                    | QItemSelectionModel.SelectionFlag.Current,
                )

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

    def get_selected_files_for_move(self):
        """Return the currently selected files suitable for a move operation."""
        return list(self.left_panel.selected_files)

    def on_move_shortcut(self, idx: int):
        selected_files = list(self.left_panel.selected_files)
        if not selected_files:
            return

        targets = self._build_move_targets()
        target_index = idx - 1
        if target_index < 0 or target_index >= len(targets):
            return

        self.right_panel.show_move_overlay(target_index)

        dialog = MoveDialog(
            selected_files,
            targets,
            target_index,
            parent=self,
        )
        dialog.moveConfirmed.connect(self._on_move_dialog_closed)
        dialog.outputIndexChanged.connect(self.right_panel.show_move_overlay)
        dialog.open()

    def _on_move_dialog_closed(self, result, selected_files, target_dir):
        self.right_panel.hide_move_overlays()
        self.on_move_dialog_finished(result, selected_files, target_dir)

    def on_palette_move_shortcut(self):
        selected_files = list(self.left_panel.selected_files)
        if not selected_files:
            return

        entries = build_directory_entries(
            self.root_dir,
            pinned_paths=self._palette_pinned_paths,
            recent_paths=self._palette_recent_paths,
        )

        self._run_palette_move_loop(entries, selected_files)

    def _run_palette_move_loop(self, entries, selected_files):
        """Open palette dialog and handle move confirmation in a loop.

        If the move is cancelled, the palette stays open for re-selection.
        If the move is confirmed, the palette is hidden and the move is executed.
        """
        palette = PaletteDialog(entries, self)
        palette.setWindowTitle("Move to directory")
        palette.input.setPlaceholderText(
            "Type to search for target directory...")

        def on_palette_finished(result):
            if result != QDialog.DialogCode.Accepted:
                return

            target_dir = palette.selected_path
            if target_dir is None or not target_dir.is_dir():
                return

            # Update recent paths: move to front, keep max 100
            target_str = str(target_dir)
            if target_str in self._palette_recent_paths:
                self._palette_recent_paths.remove(target_str)
            self._palette_recent_paths.insert(0, target_str)
            self._palette_recent_paths = self._palette_recent_paths[:100]

            targets = self._build_move_targets()
            # Find or append the palette-selected target
            palette_target_index = -1
            for i, (t_dir, _, _) in enumerate(targets):
                if t_dir == target_dir:
                    palette_target_index = i
                    break
            if palette_target_index == -1:
                targets.append((target_dir, "",
                                str(target_dir.relative_to(self.root_dir))))
                palette_target_index = len(targets) - 1

            dialog = MoveDialog(
                selected_files,
                targets,
                palette_target_index,
                parent=self,
            )

            def on_move_finished(result, files, t_dir):
                if result == QDialog.DialogCode.Accepted:
                    self.on_move_dialog_finished(result, files, t_dir)
                else:
                    # Move cancelled: reopen palette with updated entries
                    new_entries = build_directory_entries(
                        self.root_dir,
                        pinned_paths=self._palette_pinned_paths,
                        recent_paths=self._palette_recent_paths,
                    )
                    self._run_palette_move_loop(new_entries, selected_files)

            dialog.moveConfirmed.connect(on_move_finished)
            dialog.open()

        palette.finished.connect(on_palette_finished)
        palette.open()

    def on_file_selected(self, file_path: str):
        logging.debug(f"File selected in GUI: {file_path}")
        try:
            rel_path = str(
                Path(file_path).resolve().relative_to(self.root_dir.resolve()))
        except ValueError:
            # Fallback if somehow it's already a relative path string
            rel_path = file_path

        entry = self.repository.get_image_by_path(rel_path)
        if entry is None:
            entry = self.repository.upsert_image(self.root_dir,
                                                 Path(file_path))

        self.current_image = entry

        prob_rows = self.repository.list_probabilistic_tags(entry.id)
        prob_tags = [(tag.category, tag.name, rel.probability)
                     for rel, tag in prob_rows]
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
        self.repository.set_probabilistic_tag(self.current_image.id, name,
                                              probability)
        self._update_fully_annotated()
        self.on_file_selected(
            str(self.root_dir / self.current_image.relative_path))

    def on_regular_tag_added(self, category: str, name: str):
        if not self.current_image:
            return
        logging.info(
            f"Adding regular tag '{category}:{name}' to image {self.current_image.id}"
        )
        self.repository.add_regular_tag(self.current_image.id, category, name)
        self._update_fully_annotated()
        self.on_file_selected(
            str(self.root_dir / self.current_image.relative_path))

    def on_regular_tag_deleted(self, category: str, name: str):
        if not self.current_image:
            return
        logging.info(
            f"Deleting regular tag '{category}:{name}' from image {self.current_image.id}"
        )
        self.repository.delete_regular_tag(self.current_image.id, category,
                                           name)
        self._update_fully_annotated()
        self.on_file_selected(
            str(self.root_dir / self.current_image.relative_path))

    def on_description_saved(self, text: str):
        if not self.current_image:
            return
        logging.info(f"Saving description for image {self.current_image.id}")
        self.repository.set_description(self.current_image.id, text)
        self._update_fully_annotated()

    def on_prob_tag_search_requested(self, category: str, name: str):
        self.left_panel.tabs.setCurrentIndex(1)
        self.left_panel.search_view.add_tag_to_query("probabilistic_tag",
                                                     category, name)

    def on_reg_tag_search_requested(self, category: str, name: str):
        self.left_panel.tabs.setCurrentIndex(1)
        self.left_panel.search_view.add_tag_to_query("regular_tag", category,
                                                     name)

    def get_state(self) -> AppState:
        from image_tagger.gui.state_models import AppState, PaletteState

        # Find the main horizontal splitter
        central = self.centralWidget()
        splitter = None
        if central is not None:
            layout = central.layout()
            for i in range(layout.count()):
                item = layout.itemAt(i)
                if item and item.widget() and isinstance(
                        item.widget(), QSplitter):
                    splitter = item.widget()
                    break

        splitter_sizes = splitter.sizes() if splitter is not None else []

        return AppState(
            window_size=(self.width(), self.height()),
            splitter_sizes=splitter_sizes,
            left_panel=self.left_panel.get_state(),
            center_panel=self.center_panel.get_state(),
            right_panel=self.right_panel.get_state(),
            palette=PaletteState(
                pinned_paths=sorted(self._palette_pinned_paths),
                recent_paths=list(self._palette_recent_paths),
            ),
        )

    def set_state(self, state: AppState) -> None:
        self.resize(*state.window_size)

        central = self.centralWidget()
        splitter = None
        if central is not None:
            layout = central.layout()
            for i in range(layout.count()):
                item = layout.itemAt(i)
                if item and item.widget() and isinstance(
                        item.widget(), QSplitter):
                    splitter = item.widget()
                    break

        if splitter is not None and state.splitter_sizes:
            splitter.setSizes(state.splitter_sizes)

        self.left_panel.set_state(state.left_panel)
        self.center_panel.set_state(state.center_panel)
        self.right_panel.set_state(state.right_panel)

        if state.palette:
            self._palette_pinned_paths = set(state.palette.pinned_paths)
            self._palette_recent_paths = list(state.palette.recent_paths)

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
    """Undoable batch file move with automatic name-collision resolution.

    When ``redo()`` is executed each source file is moved into *target_dir*
    using :func:`shutil.move`.  If a destination path already exists (or the
    basename was already claimed by another selected file) the command
    generates a unique name by appending the first 8 hex characters of the
    file's MD5 hash to the stem, looping until a free name is found.

    The command keeps the database in sync by updating the
    ``relative_path`` and ``original_name`` columns of the corresponding
    :class:`~image_tagger.db.models.ImageEntry` rows, then refreshes the
    relevant UI panels so that the tree view and preview widgets reflect the
    new filesystem state.  On ``undo()`` the inverse moves are performed and
    the original file selection is restored.
    """

    def __init__(
        self,
        repository: Repository,
        source_files: list[Path],
        target_dir: Path,
        root_dir: Path,
        main_window: "MainWindow",
        parent=None,
    ):
        """Prepare the move operation without touching the filesystem.

        The constructor resolves every destination path ahead of time so that
        ``redo`` and ``undo`` operate on a stable pre-computed list.  Name
        collisions are resolved by hashing the source file with
        :func:`~image_tagger.db.repository.get_md5` and appending ``_{md5[:8]}``
        to the stem.  The original set of selected files is preserved so that
        ``undo`` can restore the user's prior selection.
        """
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
        """Synchronize the database with a single filesystem move.

        The row matching *old_path* (resolved relative to *root_dir*) is
        looked up via ``relative_path``.  If found, the column is rewritten to
        the path relative to *new_path* and the change is committed
        immediately.
        """
        old_rel = str(old_path.resolve().relative_to(self.root_dir.resolve()))
        entry = self.repository.session.scalar(
            select(ImageEntry).where(ImageEntry.relative_path == old_rel))
        if entry:
            entry.relative_path = str(new_path.resolve().relative_to(
                self.root_dir.resolve()))
            entry.original_name = new_path.name
            self.repository.session.commit()

    def _refresh_ui(self):
        """Refresh all views that display directory contents.

        This re-populates the left-hand tree view and every preview widget in
        the right-hand panel so that moved files disappear from their old
        locations and appear in the new one.
        """
        self.main_window.left_panel.tree_view.refresh()
        self.main_window.left_panel.viewport().update()

        for i in range(self.main_window.right_panel.splitter.count()):
            w = self.main_window.right_panel.splitter.widget(i)
            if hasattr(w, "current_dir"):
                w.set_directory(w.current_dir)

    def redo(self):
        """Execute the move forward.

        Files are moved via :func:`shutil.move`, database rows are updated,
        the UI is refreshed, and the main window shifts focus to the moved
        files so the user sees the result immediately.
        """
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
        """Reverse the move operation.

        Each file is moved back from its destination to the original source
        path, the database is reverted, the UI is refreshed, and the user's
        original selection is restored.
        """
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
    """Primary application window orchestrating the image-tagging workflow.

    The window is organised as a horizontal splitter with three panels:

    * **Left** – :class:`~image_tagger.gui.left_panel.LeftPanel` showing a
        directory tree (with a tile view) and a search tab.
    * **Center** – :class:`~image_tagger.gui.center_panel.CenterPanel`
        displaying probabilistic tags, regular tags, and a description editor
        for the currently selected image.
    * **Right** – :class:`~image_tagger.gui.right_panel.RightPanel`
        containing one or more :class:`DirectoryPreviewWidget` columns that act
        as visual drop targets for file moves.

    Control flow
    ============

    1. **File selection** – When the user clicks a file in the left panel,
        :meth:`on_file_selected` is emitted.  It resolves the file to a
        relative path, looks up (or lazily creates) the
        :class:`~image_tagger.db.models.ImageEntry`, and pushes the
        associated probabilistic tags, regular tags, and description into the
        center panel.

    2. **Tagging / description edits** – Center-panel signals
        (``probabilisticTagAdded``, ``regularTagAdded``, etc.) are wired to
        repository methods.  After each mutation
        :meth:`_update_fully_annotated` is called so that the left panel can
        highlight files whose annotation is complete.

    3. **Search** – Clicking a tag in the center panel emits a search-request
        signal that switches the left tab to search and seeds the query with
        the clicked tag.

    4. **Moving files** – Two user-facing paths exist:

        * *Direct shortcuts* ``Ctrl+1`` … ``Ctrl+9`` invoke
            :meth:`on_move_shortcut`, which temporarily highlights the target
            preview widget, opens a :class:`MoveDialog`, and—on
            confirmation—creates a :class:`MoveFilesCommand` and pushes it onto
            the :class:`QUndoStack`.

        * *Palette move* ``Ctrl+M`` invokes
            :meth:`on_palette_move_shortcut`, which opens a fuzzy
            :class:`PaletteDialog` for directory selection.  If the user
            confirms, a :class:`MoveDialog` is opened; if cancelled, the palette
            reopens so the user can pick another directory immediately.

        In both cases the actual filesystem work is performed by
        :class:`MoveFilesCommand`, which also updates the DB and refreshes
        the UI.
    """

    def get_mixed_view(self) -> MixedTreeTileView:
        """Return the tree-and-tile view from the left panel."""
        return self.left_panel.tree_view

    def get_probability_tags_table(self) -> QTableWidget:
        """Return the probabilistic tags table from the center panel."""
        return self.center_panel.prob_table

    def __init__(self, root_dir: Path, repository: Repository):
        """Construct the main window, lay out the three panels, and wire signals.

        A :class:`QUndoStack` is created with ``Ctrl+Z`` / ``Ctrl+Shift+Z``
        actions.  Numeric shortcuts ``Ctrl+1`` … ``Ctrl+9`` are bound to the
        nine right-panel preview targets, and ``Ctrl+M`` opens the fuzzy-move
        palette.
        """
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
        """Query the repository for fully-annotated images and highlight them.

        The set of absolute paths is forwarded to the left panel's tree view
        so that completed files can be drawn with a distinct visual marker.
        """
        rel_paths = self.repository.get_fully_annotated_paths()
        full_paths = {self.root_dir / p for p in rel_paths}
        self.left_panel.fully_annotated_files = full_paths
        self.left_panel.viewport().update()

    def _build_move_targets(self) -> list[tuple[Path, str, str]]:
        """Collect the current target directories exposed by the right panel.

        Iterates over every widget inside the right-hand splitter.  For
        :class:`DirectoryPreviewWidget` instances the associated colour index
        and relevant-path text are extracted as well.  The returned list is
        used to populate :class:`MoveDialog` and to map numeric shortcuts to
        concrete directories.
        """
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
        """Select a sensible file after a move or undo.

        If *selection_override* is provided (used by undo) it is converted
        directly into the new selection.  Otherwise the method walks the
        visible tiles in the left panel looking for a file that shares a
        common parent with the moved items.  If the search tab is active its
        results are refreshed and the first result is selected.
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
        """Create and execute a :class:`MoveFilesCommand` when a move is accepted.

        This is the common exit point for both direct-shortcut moves and
        palette-driven moves.
        """
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
        """Return the files currently selected in the left panel as a list."""
        return list(self.left_panel.selected_files)

    def on_move_shortcut(self, idx: int):
        """Handle ``Ctrl+<idx>`` by opening a move dialog for the Nth target.

        The right-panel preview widget corresponding to *idx* is temporarily
        highlighted via :meth:`RightPanel.show_move_overlay`.  If the dialog
        is cancelled the overlay is hidden in
        :meth:`_on_move_dialog_closed`.
        """
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
        """Hide move overlays and forward the result to the command builder."""
        self.right_panel.hide_move_overlays()
        self.on_move_dialog_finished(result, selected_files, target_dir)

    def on_palette_move_shortcut(self):
        """Handle ``Ctrl+M`` by opening the fuzzy directory palette.

        If files are selected, the palette is seeded with pinned and recent
        directories.  Selection of a directory advances to the move-dialog
        stage via :meth:`_run_palette_move_loop`.
        """
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
        """Open the palette dialog and, if accepted, chain into a move dialog.

        Implementation detail: this method is **self-recursive**.  If the
        user cancels the subsequent :class:`MoveDialog`, a fresh palette is
        reopened with updated recent-path history so the user can pick a
        different target without re-pressing the shortcut.

        Recent paths are maintained as an MRU list capped at 10 items.
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

            # Update recent paths: move to front, keep max 10
            target_str = str(target_dir)
            if target_str in self._palette_recent_paths:
                self._palette_recent_paths.remove(target_str)
            self._palette_recent_paths.insert(0, target_str)
            self._palette_recent_paths = self._palette_recent_paths[:10]

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
        """Load metadata for *file_path* and populate the center panel.

        The path is resolved to a location relative to :attr:`root_dir`.  The
        repository is queried for an existing :class:`ImageEntry`; if absent a
        new record is created via :meth:`Repository.upsert_image`.  All
        probabilistic tags, regular tags, and the description are fetched and
        forwarded to the center panel widgets.
        """
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
        """Persist a probabilistic tag for the current image and refresh UI."""
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
        """Persist a regular tag for the current image and refresh UI."""
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
        """Remove a regular tag from the current image and refresh UI."""
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
        """Persist the description text for the current image."""
        if not self.current_image:
            return
        logging.info(f"Saving description for image {self.current_image.id}")
        self.repository.set_description(self.current_image.id, text)
        self._update_fully_annotated()

    def on_prob_tag_search_requested(self, category: str, name: str):
        """Switch to the search tab and filter by the clicked probabilistic tag."""
        self.left_panel.tabs.setCurrentIndex(1)
        self.left_panel.search_view.add_tag_to_query("probabilistic_tag",
                                                     category, name)

    def on_reg_tag_search_requested(self, category: str, name: str):
        """Switch to the search tab and filter by the clicked regular tag."""
        self.left_panel.tabs.setCurrentIndex(1)
        self.left_panel.search_view.add_tag_to_query("regular_tag", category,
                                                     name)

    def get_state(self) -> AppState:
        """Serialize the complete window layout and panel states.

        Captures window size, horizontal splitter sizes, the internal state of
        each panel, and the palette's pinned / recent path history.
        """
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
        """Restore window geometry, splitter proportions, and panel states."""
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

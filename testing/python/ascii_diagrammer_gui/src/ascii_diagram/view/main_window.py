from __future__ import annotations

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QMainWindow,
    QSplitter,
    QVBoxLayout,
    QWidget,
    QMenuBar,
    QFileDialog,
    QMessageBox,
)

from ascii_diagram.model.scene_model import SceneModel
from ascii_diagram.rendering.ascii_grid import AsciiGrid
from ascii_diagram.rendering.raster_renderer import render_scene_to_grid
from ascii_diagram.io.scene_io import save_to_file, load_from_file
from ascii_diagram.view.scene_view import SceneView
from ascii_diagram.view.shape_palette import ShapePaletteWidget
from ascii_diagram.view.properties_panel import PropertiesPanel
from ascii_diagram.view.scene_tree import SceneTreeWidget
from ascii_diagram.view.export_dialog import ExportDialog


class MainWindow(QMainWindow):

    def __init__(self):
        super().__init__()
        self.setWindowTitle("ASCII Diagrammer")
        self.resize(1200, 800)

        self._model = SceneModel(self)

        self._scene_view = SceneView(self._model)
        self._palette = ShapePaletteWidget()
        self._properties_panel = PropertiesPanel(self._model)
        self._scene_tree = SceneTreeWidget(self._model)

        self._setup_menu()
        self._setup_layout()
        self._connect_signals()

    def _setup_menu(self) -> None:
        menubar = self.menuBar()

        file_menu = menubar.addMenu("&File")

        new_action = file_menu.addAction("&New")
        new_action.setShortcut("Ctrl+N")
        new_action.triggered.connect(self._new_scene)

        open_action = file_menu.addAction("&Open...")
        open_action.setShortcut("Ctrl+O")
        open_action.triggered.connect(self._open_file)

        save_action = file_menu.addAction("&Save...")
        save_action.setShortcut("Ctrl+S")
        save_action.triggered.connect(self._save_file)

        file_menu.addSeparator()

        export_action = file_menu.addAction("&Export ASCII...")
        export_action.setShortcut("Ctrl+E")
        export_action.triggered.connect(self._export_ascii)

    def _setup_layout(self) -> None:
        central = QWidget()
        self.setCentralWidget(central)

        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(0, 0, 0, 0)
        left_layout.addWidget(self._palette)

        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)
        right_layout.setContentsMargins(0, 0, 0, 0)
        right_splitter = QSplitter(Qt.Vertical)
        right_splitter.addWidget(self._properties_panel)
        right_splitter.addWidget(self._scene_tree)
        right_splitter.setStretchFactor(0, 1)
        right_splitter.setStretchFactor(1, 1)
        right_layout.addWidget(right_splitter)

        main_splitter = QSplitter(Qt.Horizontal)
        main_splitter.addWidget(left_panel)
        main_splitter.addWidget(self._scene_view)
        main_splitter.addWidget(right_panel)
        main_splitter.setStretchFactor(0, 0)
        main_splitter.setStretchFactor(1, 1)
        main_splitter.setStretchFactor(2, 0)
        main_splitter.setSizes([150, 700, 300])

        layout = QVBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(main_splitter)

    def _connect_signals(self) -> None:
        self._scene_view.selection_updated.connect(
            self._sync_scene_selection_to_tree)
        self._scene_tree.selection_changed_for_panel.connect(
            self._sync_tree_selection_to_scene)
        self._properties_panel.property_changed.connect(
            self._scene_view.raster_overlay().rebuild)

    def _sync_scene_selection_to_tree(self) -> None:
        indices = self._scene_view.selected_model_indices()
        if indices:
            self._scene_tree.selectionModel().blockSignals(True)
            self._scene_tree.clearSelection()
            for idx in indices:
                self._scene_tree.setCurrentIndex(idx)
                self._scene_tree.selectionModel().select(
                    idx,
                    self._scene_tree.selectionModel().Select
                    | self._scene_tree.selectionModel().Rows,
                )
            self._scene_tree.selectionModel().blockSignals(False)

            if len(indices) == 1:
                self._properties_panel.set_current_index(indices[0])
            else:
                self._properties_panel.set_current_index(
                    indices[0] if indices else indices[0])
        else:
            self._properties_panel.set_current_index(self._model.index(0, 0))

    def _sync_tree_selection_to_scene(self, index) -> None:
        self._scene_view.select_model_indices([index])
        self._properties_panel.set_current_index(index)

    def _new_scene(self) -> None:
        self._model.reset_model()
        self._scene_view._rebuild_all()

    def _open_file(self) -> None:
        path, _ = QFileDialog.getOpenFileName(
            self,
            "Open Scene",
            "",
            "JSON Files (*.json)",
        )
        if path:
            try:
                load_from_file(self._model, path)
                self._scene_view._rebuild_all()
            except Exception as e:
                QMessageBox.critical(self, "Error",
                                     f"Failed to open file: {e}")

    def _save_file(self) -> None:
        path, _ = QFileDialog.getSaveFileName(
            self,
            "Save Scene",
            "",
            "JSON Files (*.json)",
        )
        if path:
            try:
                save_to_file(self._model, path)
            except Exception as e:
                QMessageBox.critical(self, "Error",
                                     f"Failed to save file: {e}")

    def _export_ascii(self) -> None:
        grid = render_scene_to_grid(self._model)
        text = grid.to_string(crop_to_bounds=True)
        dialog = ExportDialog(text, self)
        dialog.exec()

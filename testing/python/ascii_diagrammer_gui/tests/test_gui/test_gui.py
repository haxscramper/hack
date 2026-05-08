"""GUI integration tests using pytest-qt."""

import pytest
from PySide6.QtCore import Qt, QPoint
from PySide6.QtGui import QFontMetrics
from PySide6.QtWidgets import QApplication, QGraphicsItem

from ascii_diagram.model.scene_model import SceneModel
from ascii_diagram.model.enums import ShapeType
from ascii_diagram.view.main_window import MainWindow
from ascii_diagram.interaction.drag_drop import (
    encode_palette_mime,
    PALETTE_MIME,
)


@pytest.fixture
def model():
    return SceneModel()


@pytest.fixture
def window(qtbot):
    win = MainWindow()
    qtbot.addWidget(win)
    win.show()
    qtbot.waitExposed(win)
    return win


class TestGUI:

    def test_main_window_initializes(self, window):
        assert window.windowTitle() == "ASCII Diagrammer"

    def test_new_scene_clears(self, window):
        window._model.insert_item(ShapeType.RECTANGLE)
        assert window._model.rowCount() == 1
        window._new_scene()
        assert window._model.rowCount() == 0

    def test_palette_populated(self, window):
        assert window._palette.count() == 4

    def test_insert_shape_via_drop(self, window, qtbot):
        from PySide6.QtCore import QPointF

        mime = encode_palette_mime(ShapeType.RECTANGLE)

        class FakeDropEvent:

            def __init__(self):
                self._pos = QPoint(50, 50)

            def mimeData(self):
                return mime

            def position(self):
                return QPointF(self._pos)

            def acceptProposedAction(self):
                pass

        event = FakeDropEvent()
        window._scene_view.dropEvent(event)

        assert window._model.rowCount() == 1

    def test_scene_tree_delete(self, window):
        window._model.insert_item(ShapeType.RECTANGLE)
        window._model.insert_item(ShapeType.ELLIPSE)
        assert window._model.rowCount() == 2

        idx = window._model.index(0, 0)
        from PySide6.QtCore import QItemSelectionModel
        sel_model = window._scene_tree.selectionModel()
        sel_model.select(
            idx,
            QItemSelectionModel.SelectionFlag.Select
            | QItemSelectionModel.SelectionFlag.Rows,
        )
        window._scene_tree.delete_selected()
        assert window._model.rowCount() == 1

    def test_scene_view_delete(self, window):
        window._model.insert_item(ShapeType.RECTANGLE)
        window._model.insert_item(ShapeType.ELLIPSE)
        assert window._model.rowCount() == 2

        window._scene_view._delete_selected()

    def test_export_dialog(self, window, qtbot):
        from ascii_diagram.view.export_dialog import ExportDialog

        window._model.insert_item(ShapeType.RECTANGLE)
        dialog = ExportDialog("test", window)
        qtbot.addWidget(dialog)
        assert dialog.windowTitle() == "Export ASCII Diagram"

    def test_properties_panel_no_selection(self, window):
        assert window._properties_panel._no_selection_label.isVisible()

    def test_properties_panel_with_selection(self, window):
        idx = window._model.insert_item(ShapeType.RECTANGLE)
        window._properties_panel.set_current_index(idx)
        assert not window._properties_panel._no_selection_label.isVisible()
        assert window._properties_panel._stack.isVisible()

    def test_model_roundtrip_after_gui_ops(self, window):
        from ascii_diagram.io.scene_io import (
            model_to_json,
            json_to_model,
        )

        window._model.insert_item(ShapeType.RECTANGLE)
        window._model.insert_item(ShapeType.ELLIPSE)

        json_str = model_to_json(window._model)
        new_model = SceneModel()
        json_to_model(new_model, json_str)
        assert new_model.rowCount() == 2

    def test_zoom(self, window):
        initial_transform = window._scene_view.transform()
        window._scene_view.scale(2.0, 2.0)
        new_transform = window._scene_view.transform()
        assert new_transform.m11() == initial_transform.m11() * 2.0

    def test_raster_overlay_exists(self, window):
        overlay = window._scene_view.raster_overlay()
        assert overlay is not None
        flags = overlay.flags()
        assert not (flags & QGraphicsItem.GraphicsItemFlag.ItemIsSelectable)

    def test_tree_view_selection_sync(self, window):
        idx = window._model.insert_item(ShapeType.RECTANGLE)

        window._scene_tree.setCurrentIndex(idx)

        current = window._scene_tree.currentIndex()
        assert current.isValid()
        assert current == idx

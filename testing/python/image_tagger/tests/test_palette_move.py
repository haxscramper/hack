from beartype.typing import cast
import pytest
from beartype import beartype
from image_tagger.gui.right_panel import DirectoryPreviewWidget
from utils import take_screenshot
from conftest import AppInstanceRes, _click_tile, _get_visible_dialog
from pathlib import Path
from pytestqt.qtbot import QtBot
from PySide6.QtCore import QTimer, Qt, QPoint
from PySide6.QtTest import QTest
from PySide6.QtWidgets import (
    QApplication,
    QDialogButtonBox,
    QLabel,
    QListView,
    QDialog,
)


def test_palette_move_single_file(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving a single file via the palette (Ctrl+M)."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    # Open palette with Ctrl+M
    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_M,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(200)

    palette = _get_visible_dialog()
    assert palette.windowTitle() == "Move to directory"

    # Type "sub1" to filter to sub1 directory
    palette.input.setText("sub1")
    qtbot.wait(100)

    # Press Enter to accept the palette selection
    QTest.keyClick(palette, Qt.Key.Key_Return, delay=50)
    qtbot.wait(200)

    # Move confirmation dialog should appear
    move_dialog = _get_visible_dialog()
    assert move_dialog.windowTitle().startswith("Move to ")

    labels = move_dialog.findChildren(QLabel)
    assert any("1 items" in lbl.text() for lbl in labels)

    qtbot.wait(150)
    take_screenshot(move_dialog,
                    screenshot_dir / "palette_move_single_dialog.png")

    assert file_to_move.exists()
    move_dialog.accept()
    qtbot.wait(150)
    take_screenshot(central_widget,
                    screenshot_dir / "palette_move_single_after.png")

    assert not file_to_move.exists()
    assert (image_directory / "sub1" / "image_255_0_239.png").exists()

    # Verify a file from the common parent directory is selected after move
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory


def test_palette_move_multiple_files(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving multiple selected files via the palette."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file1 = image_directory / "image_255_0_239.png"
    file2 = image_directory / "image_255_0_191.png"

    _click_tile(widget, file1, qtbot)
    assert file1 in widget.selected_files

    rect2 = widget.get_tile_rect(file2)
    assert rect2 is not None
    if not widget.is_element_visible(rect2):
        widget.scroll_to_content_y(rect2.center().y())
        qtbot.wait(50)
        rect2 = widget.get_tile_rect(file2)
        assert rect2 is not None
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect2.center().x(), rect2.center().y() - scroll_y)
    QTest.mouseClick(
        widget.viewport(),
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.ControlModifier,
        click_pos,
    )
    qtbot.wait(50)

    assert file1 in widget.selected_files
    assert file2 in widget.selected_files
    assert len(widget.selected_files) == 2

    # Open palette with Ctrl+M
    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_M,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(200)

    palette = _get_visible_dialog()
    assert palette.windowTitle() == "Move to directory"

    # Type "sub2" to filter
    palette.input.setText("sub2")
    qtbot.wait(100)

    QTest.keyClick(palette, Qt.Key.Key_Return, delay=50)
    qtbot.wait(200)

    move_dialog = _get_visible_dialog()
    assert move_dialog.windowTitle().startswith("Move to ")

    labels = move_dialog.findChildren(QLabel)
    assert any("2 items" in lbl.text() for lbl in labels)

    qtbot.wait(150)
    take_screenshot(move_dialog,
                    screenshot_dir / "palette_move_multi_dialog.png")

    assert file1.exists()
    assert file2.exists()
    move_dialog.accept()
    qtbot.wait(150)

    assert not file1.exists()
    assert not file2.exists()
    assert (image_directory / "sub2" / "image_255_0_239.png").exists()
    assert (image_directory / "sub2" / "image_255_0_191.png").exists()

    # Verify a file from the common parent directory is selected after move
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory


def test_palette_move_cancel_keeps_palette_open(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that cancelling the move dialog reopens the palette for re-selection."""
    qtbot.waitExposed(gui_app_instance.window)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    # Open palette with Ctrl+M
    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_M,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(200)

    palette = _get_visible_dialog()
    assert palette.windowTitle() == "Move to directory"

    # Select sub1 directory
    palette.input.setText("sub1")
    qtbot.wait(100)
    QTest.keyClick(palette, Qt.Key.Key_Return, delay=50)
    qtbot.wait(200)

    # Move dialog appears
    move_dialog = _get_visible_dialog()
    assert move_dialog.windowTitle().startswith("Move to ")

    # Cancel the move dialog
    move_dialog.reject()
    qtbot.wait(200)

    # Palette should reopen
    palette = _get_visible_dialog()
    assert palette.windowTitle() == "Move to directory"

    # Now select sub2 and confirm
    palette.input.setText("sub2")
    qtbot.wait(100)
    QTest.keyClick(palette, Qt.Key.Key_Return, delay=50)
    qtbot.wait(200)

    move_dialog = _get_visible_dialog()
    move_dialog.accept()
    qtbot.wait(150)

    assert not file_to_move.exists()
    assert (image_directory / "sub2" / "image_255_0_239.png").exists()


def test_palette_move_from_search_results(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving a file selected from search results via the palette."""
    qtbot.waitExposed(gui_app_instance.window)

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "palette_move_tag", 0.9)],
    )

    from conftest import setup_search_tab

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "general",
        "name": "palette_move_tag",
        "min_probability": 0.5,
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)
    search_tab.execute_search()
    qtbot.wait(100)

    results = search_tab.get_result_images()
    assert len(results) == 1
    assert str(target) == str(results[0])

    thumbnail_list = search_tab.thumbnail_list.list_view
    index = thumbnail_list.model().index(0, 0)

    from PySide6.QtCore import QItemSelectionModel

    selection_model = thumbnail_list.selectionModel()
    selection_model.select(
        index,
        QItemSelectionModel.SelectionFlag.Select
        | QItemSelectionModel.SelectionFlag.Current,
    )
    thumbnail_list.clicked.emit(index)
    qtbot.wait(50)

    assert target in gui_app_instance.window.left_panel.selected_files

    # Open palette with Ctrl+M
    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_M,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(200)

    palette = _get_visible_dialog()
    assert palette.windowTitle() == "Move to directory"

    palette.input.setText("sub1")
    qtbot.wait(100)
    QTest.keyClick(palette, Qt.Key.Key_Return, delay=50)
    qtbot.wait(200)

    move_dialog = _get_visible_dialog()
    assert move_dialog.windowTitle().startswith("Move to ")

    qtbot.wait(150)
    take_screenshot(move_dialog,
                    screenshot_dir / "palette_move_search_dialog.png")

    assert target.exists()
    move_dialog.accept()
    qtbot.wait(150)

    assert not target.exists()
    assert (image_directory / "sub1" / "image_255_0_239.png").exists()

    # Verify a file from the common parent directory is selected after move
    widget = gui_app_instance.window.get_mixed_view()
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory


def test_palette_move_no_selection_does_nothing(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that Ctrl+M does nothing when no files are selected."""
    qtbot.waitExposed(gui_app_instance.window)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Clear any selection
    widget.selected_files = set()
    widget.viewport().update()
    qtbot.wait(50)

    # Open palette with Ctrl+M - should not open anything
    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_M,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(200)

    # No dialog should be visible
    dialog = next(
        (w for w in QApplication.topLevelWidgets()
         if isinstance(w, QDialog) and w.isVisible()),
        None,
    )
    assert dialog is None


def test_palette_state_persistence(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that palette recent/pinned state is included in app state."""
    qtbot.waitExposed(gui_app_instance.window)

    window = gui_app_instance.window

    # Set some palette state
    window._palette_pinned_paths = {str(image_directory / "sub1")}
    window._palette_recent_paths = [
        str(image_directory / "sub2"),
        str(image_directory / "sub1"),
    ]

    state = window.get_state()
    assert state.palette is not None
    assert str(image_directory / "sub1") in state.palette.pinned_paths
    assert state.palette.recent_paths[0] == str(image_directory / "sub2")
    assert state.palette.recent_paths[1] == str(image_directory / "sub1")

    # Reset and restore state
    window._palette_pinned_paths = set()
    window._palette_recent_paths = []

    window.set_state(state)
    assert str(image_directory / "sub1") in window._palette_pinned_paths
    assert window._palette_recent_paths == [
        str(image_directory / "sub2"),
        str(image_directory / "sub1"),
    ]


def test_palette_move_updates_recent_paths(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that using the palette move updates the recent paths."""
    qtbot.waitExposed(gui_app_instance.window)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    window = gui_app_instance.window
    assert window._palette_recent_paths == []

    # Open palette with Ctrl+M
    QTest.keyClick(window, Qt.Key.Key_M, Qt.KeyboardModifier.ControlModifier,
                   100)
    qtbot.wait(200)

    palette = _get_visible_dialog()
    palette.input.setText("sub1")
    qtbot.wait(100)
    QTest.keyClick(palette, Qt.Key.Key_Return, delay=50)
    qtbot.wait(200)

    move_dialog = _get_visible_dialog()
    move_dialog.accept()
    qtbot.wait(150)

    # Recent paths should now include sub1
    assert len(window._palette_recent_paths) >= 1
    assert window._palette_recent_paths[0] == str(image_directory / "sub1")

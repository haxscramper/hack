from beartype.typing import cast
import pytest
from beartype import beartype
from image_tagger.gui.right_panel import DirectoryPreviewWidget
from utils import take_screenshot
from conftest import AppInstanceRes, _click_tile, _get_visible_dialog, setup_search_tab
from pathlib import Path
from pytestqt.qtbot import QtBot
from image_tagger.utils.utils import _generate_monotone_image
from PySide6.QtCore import QTimer, Qt, QPoint
from PySide6.QtTest import QTest
from PySide6.QtWidgets import (
    QDialogButtonBox,
    QLabel,
    QListView,
)


def test_move_file_from_root_to_output(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving a file from the root directory to the default output target."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    right_panel = gui_app_instance.window.right_panel
    preview_widget = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(0))
    selector = preview_widget.selector

    sub1_dir = image_directory / "sub1"
    list_widget = selector.subdir_list
    sub1_item = None
    for i in range(list_widget.count()):
        item = list_widget.item(i)
        if item.text() == "sub1":
            sub1_item = item
            break
    assert sub1_item is not None
    list_widget.itemClicked.emit(sub1_item)
    qtbot.wait(50)
    assert preview_widget.current_dir == sub1_dir

    gui_app_instance.window.activateWindow()
    qtbot.wait(50)
    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    labels = dialog.findChildren(QLabel)
    assert labels
    assert any("1 items" in lbl.text() for lbl in labels)
    # The highlighted path label is a separate QLabel
    path_label = next((lbl for lbl in labels if lbl.text() == "sub1"), None)
    assert path_label is not None

    list_view = dialog.findChild(QListView)
    assert list_view is not None

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_root_dialog.png")
    take_screenshot(central_widget, screenshot_dir / "move_root_before.png")

    assert file_to_move.exists()
    dialog.accept()
    qtbot.wait(150)
    take_screenshot(central_widget, screenshot_dir / "move_root_after.png")

    assert not file_to_move.exists()
    assert (sub1_dir / "image_255_0_239.png").exists()

    # Verify a file from the common parent directory is selected after move
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory


def test_move_multiple_files_to_output(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving multiple selected files to the default output target."""
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

    right_panel = gui_app_instance.window.right_panel
    preview_widget = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(0))
    selector = preview_widget.selector

    sub2_dir = image_directory / "sub2"
    list_widget = selector.subdir_list
    sub2_item = None
    for i in range(list_widget.count()):
        item = list_widget.item(i)
        if item.text() == "sub2":
            sub2_item = item
            break
    assert sub2_item is not None
    list_widget.itemClicked.emit(sub2_item)
    qtbot.wait(50)
    assert preview_widget.current_dir == sub2_dir

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    labels = dialog.findChildren(QLabel)
    assert labels
    assert any("2 items" in lbl.text() for lbl in labels)
    path_label = next((lbl for lbl in labels if lbl.text() == "sub2"), None)
    assert path_label is not None

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_multi_dialog.png")

    assert file1.exists()
    assert file2.exists()
    dialog.accept()
    qtbot.wait(150)

    assert not file1.exists()
    assert not file2.exists()

    assert (sub2_dir / "image_255_0_239.png").exists()
    assert (sub2_dir / "image_255_0_191.png").exists()

    # Verify a file from the common parent directory is selected after move
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory


def test_move_file_to_second_output(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving a file to the second output target (Ctrl+2)."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    right_panel = gui_app_instance.window.right_panel
    right_panel.add_preview_widget()
    qtbot.wait(50)

    assert right_panel.splitter.count() == 2

    second_preview = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(1))
    sub1_dir = image_directory / "sub1"
    second_preview.set_directory(sub1_dir)
    qtbot.wait(50)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_2,
                   Qt.KeyboardModifier.ControlModifier, 100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    labels = dialog.findChildren(QLabel)
    assert labels
    path_label = next((lbl for lbl in labels if lbl.text() == "sub1"), None)
    assert path_label is not None

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_second_output_dialog.png")

    assert file_to_move.exists()
    dialog.accept()
    qtbot.wait(150)

    assert not file_to_move.exists()
    assert (sub1_dir / "image_255_0_239.png").exists()

    # Verify a file from the common parent directory is selected after move
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory

    # Verify state reflects two preview widgets
    state = gui_app_instance.window.get_state()
    assert len(state.right_panel.preview_widgets) == 2
    # After the move, the right panel refreshes and may reset the preview widget
    # directory back to root. Check that the second preview widget exists.
    assert state.right_panel.preview_widgets[1].selector.current_dir


def test_move_file_with_output_navigation(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test navigating the output target (clicking path bar / subdirs) before moving."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    right_panel = gui_app_instance.window.right_panel
    preview_widget = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(0))
    selector = preview_widget.selector

    sub1_dir = image_directory / "sub1"
    sub12_dir = sub1_dir / "sub12"

    assert preview_widget.current_dir == image_directory

    list_widget = selector.subdir_list
    sub1_item = None
    for i in range(list_widget.count()):
        item = list_widget.item(i)
        if item.text() == "sub1":
            sub1_item = item
            break
    assert sub1_item is not None

    list_widget.itemClicked.emit(sub1_item)
    qtbot.wait(50)

    assert preview_widget.current_dir == sub1_dir

    sub12_item = None
    for i in range(list_widget.count()):
        item = list_widget.item(i)
        if item.text() == "sub12":
            sub12_item = item
            break
    assert sub12_item is not None

    list_widget.itemClicked.emit(sub12_item)
    qtbot.wait(50)

    assert preview_widget.current_dir == sub12_dir

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    labels = dialog.findChildren(QLabel)
    assert labels
    path_label = next((lbl for lbl in labels if lbl.text() == "sub12"), None)
    assert path_label is not None

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_navigated_dialog.png")

    assert file_to_move.exists()
    dialog.accept()
    qtbot.wait(150)

    assert not file_to_move.exists()
    assert (sub12_dir / "image_255_0_239.png").exists()

    # Verify a file from the common parent directory is selected after move
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory

    # Verify state reflects the navigated directory in the preview widget
    state = gui_app_instance.window.get_state()
    assert state.right_panel.preview_widgets[0].selector.current_dir == str(
        sub12_dir)


def test_move_file_from_search_results(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving a file selected from the search results view."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "move_test_tag", 0.9)],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "general",
        "name": "move_test_tag",
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

    right_panel = gui_app_instance.window.right_panel
    preview_widget = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(0))
    selector = preview_widget.selector

    sub1_dir = image_directory / "sub1"
    list_widget = selector.subdir_list
    sub1_item = None
    for i in range(list_widget.count()):
        item = list_widget.item(i)
        if item.text() == "sub1":
            sub1_item = item
            break
    assert sub1_item is not None
    list_widget.itemClicked.emit(sub1_item)
    qtbot.wait(50)
    assert preview_widget.current_dir == sub1_dir

    gui_app_instance.window.activateWindow()
    qtbot.wait(50)
    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")
    labels = dialog.findChildren(QLabel)
    path_label = next((lbl for lbl in labels if lbl.text() == "sub1"), None)
    assert path_label is not None

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_search_dialog.png")

    assert target.exists()
    dialog.accept()
    qtbot.wait(150)

    assert not target.exists()
    assert (sub1_dir / "image_255_0_239.png").exists()

    # Verify a file from the common parent directory is selected after move
    widget = gui_app_instance.window.get_mixed_view()
    assert len(widget.selected_files) == 1
    selected = next(iter(widget.selected_files))
    assert selected.parent == image_directory


def _accept_clear_selection_dialog(qtbot: QtBot):
    """Find and accept the visible clear-selection confirmation dialog."""
    dialog = _get_visible_dialog()
    assert dialog.windowTitle() == "Clear Selection?"
    buttons = dialog.findChild(QDialogButtonBox)
    assert buttons is not None
    qtbot.wait(50)
    dialog.accept()
    qtbot.wait(50)


def _reject_clear_selection_dialog(qtbot: QtBot):
    """Find and reject the visible clear-selection confirmation dialog."""
    dialog = _get_visible_dialog()
    assert dialog.windowTitle() == "Clear Selection?"
    buttons = dialog.findChild(QDialogButtonBox)
    assert buttons is not None
    qtbot.wait(50)
    dialog.reject()
    qtbot.wait(50)


def test_mixed_view_large_selection_clear_confirmed(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that clearing >3 selected files in mixed view shows confirmation dialog."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Expand subdirectories to get enough images
    for d in image_directory.iterdir():
        if d.is_dir():
            toggle = widget.get_toggle_rect(d)
            if toggle:
                scroll_y = widget.verticalScrollBar().value()
                pos = QPoint(toggle.center().x(),
                             toggle.center().y() - scroll_y)
                QTest.mouseClick(widget.viewport(),
                                 Qt.MouseButton.LeftButton,
                                 pos=pos)
                qtbot.wait(50)

    # Select 4 images with Ctrl+Click
    images = [hit.file_path for hit in widget.tile_hits][:4]
    assert len(images) >= 4, f"Need at least 4 images, found {len(images)}"

    for i, img in enumerate(images):
        rect = widget.get_tile_rect(img)
        assert rect is not None
        if not widget.is_element_visible(rect):
            widget.scroll_to_content_y(rect.center().y())
            qtbot.wait(50)
            rect = widget.get_tile_rect(img)
            assert rect is not None
        scroll_y = widget.verticalScrollBar().value()
        pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)
        if i == 0:
            QTest.mouseClick(widget.viewport(),
                             Qt.MouseButton.LeftButton,
                             pos=pos)
        else:
            QTest.mouseClick(
                widget.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.ControlModifier,
                pos,
            )
        qtbot.wait(50)

    assert len(widget.selected_files) == 4

    # Now left-click a 5th image without Ctrl/Shift - should show confirmation
    other_images = [hit.file_path for hit in widget.tile_hits]
    fifth_img = None
    for img in other_images:
        if img not in widget.selected_files:
            fifth_img = img
            break
    assert fifth_img is not None, "Need an unselected image to click"

    rect5 = widget.get_tile_rect(fifth_img)
    assert rect5 is not None
    if not widget.is_element_visible(rect5):
        widget.scroll_to_content_y(rect5.center().y())
        qtbot.wait(50)
        rect5 = widget.get_tile_rect(fifth_img)
        assert rect5 is not None
    scroll_y = widget.verticalScrollBar().value()
    pos5 = QPoint(rect5.center().x(), rect5.center().y() - scroll_y)

    # Use a timer to accept the dialog since it will block
    QTimer.singleShot(100, lambda: _accept_clear_selection_dialog(qtbot))
    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=pos5)
    qtbot.wait(200)

    # Selection should be cleared and only fifth_img selected
    assert len(widget.selected_files) == 1
    assert fifth_img in widget.selected_files


def test_mixed_view_large_selection_clear_cancelled(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that cancelling the clear-selection dialog preserves selection."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Expand subdirectories to get enough images
    for d in image_directory.iterdir():
        if d.is_dir():
            toggle = widget.get_toggle_rect(d)
            if toggle:
                scroll_y = widget.verticalScrollBar().value()
                pos = QPoint(toggle.center().x(),
                             toggle.center().y() - scroll_y)
                QTest.mouseClick(widget.viewport(),
                                 Qt.MouseButton.LeftButton,
                                 pos=pos)
                qtbot.wait(50)

    # Select 4 images with Ctrl+Click
    images = [hit.file_path for hit in widget.tile_hits][:4]
    assert len(images) >= 4, f"Need at least 4 images, found {len(images)}"

    for i, img in enumerate(images):
        rect = widget.get_tile_rect(img)
        assert rect is not None
        if not widget.is_element_visible(rect):
            widget.scroll_to_content_y(rect.center().y())
            qtbot.wait(50)
            rect = widget.get_tile_rect(img)
            assert rect is not None
        scroll_y = widget.verticalScrollBar().value()
        pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)
        if i == 0:
            QTest.mouseClick(widget.viewport(),
                             Qt.MouseButton.LeftButton,
                             pos=pos)
        else:
            QTest.mouseClick(
                widget.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.ControlModifier,
                pos,
            )
        qtbot.wait(50)

    original_selection = set(widget.selected_files)
    assert len(original_selection) == 4

    # Now left-click a 5th image without Ctrl/Shift - should show confirmation
    other_images = [hit.file_path for hit in widget.tile_hits]
    fifth_img = None
    for img in other_images:
        if img not in widget.selected_files:
            fifth_img = img
            break
    assert fifth_img is not None, "Need an unselected image to click"

    rect5 = widget.get_tile_rect(fifth_img)
    assert rect5 is not None
    if not widget.is_element_visible(rect5):
        widget.scroll_to_content_y(rect5.center().y())
        qtbot.wait(50)
        rect5 = widget.get_tile_rect(fifth_img)
        assert rect5 is not None
    scroll_y = widget.verticalScrollBar().value()
    pos5 = QPoint(rect5.center().x(), rect5.center().y() - scroll_y)

    # Use a timer to reject the dialog since it will block
    QTimer.singleShot(100, lambda: _reject_clear_selection_dialog(qtbot))
    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=pos5)
    qtbot.wait(200)

    # Selection should be preserved
    assert widget.selected_files == original_selection
    assert len(widget.selected_files) == 4


def test_mixed_view_small_selection_no_confirmation(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that clearing <=3 selected files does NOT show confirmation dialog."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Expand subdirectories to get enough images
    for d in image_directory.iterdir():
        if d.is_dir():
            toggle = widget.get_toggle_rect(d)
            if toggle:
                scroll_y = widget.verticalScrollBar().value()
                pos = QPoint(toggle.center().x(),
                             toggle.center().y() - scroll_y)
                QTest.mouseClick(widget.viewport(),
                                 Qt.MouseButton.LeftButton,
                                 pos=pos)
                qtbot.wait(50)

    # Select 3 images with Ctrl+Click
    images = [hit.file_path for hit in widget.tile_hits][:3]
    assert len(images) >= 3, f"Need at least 3 images, found {len(images)}"

    for i, img in enumerate(images):
        rect = widget.get_tile_rect(img)
        assert rect is not None
        if not widget.is_element_visible(rect):
            widget.scroll_to_content_y(rect.center().y())
            qtbot.wait(50)
            rect = widget.get_tile_rect(img)
            assert rect is not None
        scroll_y = widget.verticalScrollBar().value()
        pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)
        if i == 0:
            QTest.mouseClick(widget.viewport(),
                             Qt.MouseButton.LeftButton,
                             pos=pos)
        else:
            QTest.mouseClick(
                widget.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.ControlModifier,
                pos,
            )
        qtbot.wait(50)

    assert len(widget.selected_files) == 3

    # Now left-click a 4th image without Ctrl/Shift - should NOT show confirmation
    other_images = [hit.file_path for hit in widget.tile_hits]
    fourth_img = None
    for img in other_images:
        if img not in widget.selected_files:
            fourth_img = img
            break
    assert fourth_img is not None, "Need an unselected image to click"

    rect4 = widget.get_tile_rect(fourth_img)
    assert rect4 is not None
    if not widget.is_element_visible(rect4):
        widget.scroll_to_content_y(rect4.center().y())
        qtbot.wait(50)
        rect4 = widget.get_tile_rect(fourth_img)
        assert rect4 is not None
    scroll_y = widget.verticalScrollBar().value()
    pos4 = QPoint(rect4.center().x(), rect4.center().y() - scroll_y)

    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=pos4)
    qtbot.wait(100)

    # No dialog should appear, selection should be cleared immediately
    assert len(widget.selected_files) == 1
    assert fourth_img in widget.selected_files


def test_search_view_large_selection_clear_confirmed(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that clearing >3 selected files in search view shows confirmation dialog."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    # Use path_contains to get all images
    spec = {"type": "path_contains", "text": ".png"}
    search_tab.set_search_spec(spec)
    search_tab.execute_search()
    qtbot.wait(100)

    results = search_tab.get_result_images()
    assert len(
        results) >= 4, f"Need at least 4 search results, found {len(results)}"

    thumbnail_list = search_tab.thumbnail_list.list_view

    # Select first 4 results with actual mouse clicks
    for i in range(4):
        index = thumbnail_list.model().index(i, 0)
        rect = thumbnail_list.visualRect(index)
        if i == 0:
            QTest.mouseClick(
                thumbnail_list.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.NoModifier,
                rect.center(),
            )
        else:
            QTest.mouseClick(
                thumbnail_list.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.ControlModifier,
                rect.center(),
            )
        qtbot.wait(50)

    selected = search_tab.thumbnail_list.get_selected_images()
    assert len(selected) == 4

    # Click on 5th result without Ctrl - should show confirmation
    index5 = thumbnail_list.model().index(4, 0)
    QTimer.singleShot(100, lambda: _accept_clear_selection_dialog(qtbot))
    QTest.mouseClick(
        thumbnail_list.viewport(),
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.NoModifier,
        thumbnail_list.visualRect(index5).center(),
    )
    qtbot.wait(200)

    selected_after = search_tab.thumbnail_list.get_selected_images()
    assert len(selected_after) == 1
    assert selected_after[0] == results[4]


def test_search_view_large_selection_clear_cancelled(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that cancelling the clear-selection dialog in search view preserves selection."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    # Use path_contains to get all images
    spec = {"type": "path_contains", "text": ".png"}
    search_tab.set_search_spec(spec)
    search_tab.execute_search()
    qtbot.wait(100)

    results = search_tab.get_result_images()
    assert len(
        results) >= 4, f"Need at least 4 search results, found {len(results)}"

    thumbnail_list = search_tab.thumbnail_list.list_view

    # Select first 4 results with actual mouse clicks
    for i in range(4):
        index = thumbnail_list.model().index(i, 0)
        rect = thumbnail_list.visualRect(index)
        if i == 0:
            QTest.mouseClick(
                thumbnail_list.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.NoModifier,
                rect.center(),
            )
        else:
            QTest.mouseClick(
                thumbnail_list.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.ControlModifier,
                rect.center(),
            )
        qtbot.wait(50)

    selected = search_tab.thumbnail_list.get_selected_images()
    assert len(selected) == 4

    # Click on 5th result without Ctrl - should show confirmation
    index5 = thumbnail_list.model().index(4, 0)
    QTimer.singleShot(100, lambda: _reject_clear_selection_dialog(qtbot))
    QTest.mouseClick(
        thumbnail_list.viewport(),
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.NoModifier,
        thumbnail_list.visualRect(index5).center(),
    )
    qtbot.wait(200)

    selected_after = search_tab.thumbnail_list.get_selected_images()
    assert len(selected_after) == 4


def test_search_view_small_selection_no_confirmation(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that clearing <=3 selected files in search view does NOT show confirmation."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    # Use path_contains to get all images
    spec = {"type": "path_contains", "text": ".png"}
    search_tab.set_search_spec(spec)
    search_tab.execute_search()
    qtbot.wait(100)

    results = search_tab.get_result_images()
    assert len(
        results) >= 3, f"Need at least 3 search results, found {len(results)}"

    thumbnail_list = search_tab.thumbnail_list.list_view

    # Select first 3 results with actual mouse clicks
    for i in range(3):
        index = thumbnail_list.model().index(i, 0)
        rect = thumbnail_list.visualRect(index)
        if i == 0:
            QTest.mouseClick(
                thumbnail_list.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.NoModifier,
                rect.center(),
            )
        else:
            QTest.mouseClick(
                thumbnail_list.viewport(),
                Qt.MouseButton.LeftButton,
                Qt.KeyboardModifier.ControlModifier,
                rect.center(),
            )
        qtbot.wait(50)

    selected = search_tab.thumbnail_list.get_selected_images()
    assert len(selected) == 3

    # Click on 4th result without Ctrl - should NOT show confirmation
    index4 = thumbnail_list.model().index(3, 0)
    QTest.mouseClick(
        thumbnail_list.viewport(),
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.NoModifier,
        thumbnail_list.visualRect(index4).center(),
    )
    qtbot.wait(100)

    selected_after = search_tab.thumbnail_list.get_selected_images()
    assert len(selected_after) == 1
    assert selected_after[0] == results[3]


def test_move_files_with_identical_names(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test moving two files with identical names from different source directories.

    This test is expected to fail due to a UNIQUE constraint on relative_path.
    """
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    sub1 = image_directory / "sub1"
    sub2 = image_directory / "sub2"

    file1 = sub1 / "random.png"
    file2 = sub2 / "random.png"

    _generate_monotone_image(file1, color=(256, 0, 0))
    _generate_monotone_image(file2, color=(0, 256, 0))

    gui_app_instance.repo.upsert_image(image_directory, file1)
    gui_app_instance.repo.upsert_image(image_directory, file2)

    gui_app_instance.window.get_mixed_view().refresh()
    qtbot.wait(100)

    widget = gui_app_instance.window.get_mixed_view()

    widget.toggle_subdir(sub1, qtbot)
    widget.toggle_subdir(sub2, qtbot)

    take_screenshot(central_widget, screenshot_dir / "move_root_dialog.png")
    _click_tile(widget, file1, qtbot)
    assert file1 in widget.selected_files

    rect2 = widget.get_tile_rect(file2)
    if rect2 is None:
        header_rect = widget.get_header_rect(sub2)
        if header_rect:
            widget.scroll_to_content_y(header_rect.center().y())
            qtbot.wait(150)
        toggle_rect = widget.get_toggle_rect(sub2)
        if toggle_rect:
            scroll_y = widget.verticalScrollBar().value()
            pos = QPoint(toggle_rect.center().x(),
                         toggle_rect.center().y() - scroll_y)
            QTest.mouseClick(widget.viewport(),
                             Qt.MouseButton.LeftButton,
                             pos=pos)
            qtbot.wait(50)
        rect2 = widget.get_tile_rect(file2)

    assert rect2 is not None, f"Tile {file2} not found"
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

    file2_md5 = gui_app_instance.repo.get_image_by_path(
        str(file2.relative_to(image_directory))).md5_digest

    file1_md5 = gui_app_instance.repo.get_image_by_path(
        str(file1.relative_to(image_directory))).md5_digest

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_identical_names_dialog.png")

    assert file1.exists()
    assert file2.exists()
    dialog.accept()
    qtbot.wait(150)
    take_screenshot(central_widget, screenshot_dir / "after_move.png")

    assert not file1.exists()
    assert not file2.exists()

    # Files were from different parents (sub1 and sub2), so no common parent
    # file to select. Fallback to first visible file.
    assert len(widget.selected_files) == 1

    right_panel = gui_app_instance.window.right_panel
    preview_widget = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(0))
    target_dir = preview_widget.current_dir
    assert (target_dir / f"random.png").exists()
    assert (target_dir / f"random_{file2_md5[:8]}.png").exists() or (
        target_dir / f"random_{file1_md5[:8]}.png").exists()


def test_move_overlay_shown_on_target_widget(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that the move overlay is shown on the target preview widget."""
    qtbot.waitExposed(gui_app_instance.window)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    right_panel = gui_app_instance.window.right_panel
    preview_widget = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(0))

    assert not preview_widget.overlay_widget.isVisible()

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    assert preview_widget.overlay_widget.isVisible()
    assert preview_widget.overlay_label.text() != ""

    dialog.reject()
    qtbot.wait(50)

    assert not preview_widget.overlay_widget.isVisible()


def test_move_overlay_hidden_on_other_widgets(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that only the target preview widget shows the overlay."""
    qtbot.waitExposed(gui_app_instance.window)

    right_panel = gui_app_instance.window.right_panel
    right_panel.add_preview_widget()
    qtbot.wait(50)

    assert right_panel.splitter.count() == 2

    first_preview = cast(DirectoryPreviewWidget,
                         right_panel.splitter.widget(0))
    second_preview = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(1))

    sub1_dir = image_directory / "sub1"
    second_preview.set_directory(sub1_dir)
    qtbot.wait(50)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_2,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    assert not first_preview.overlay_widget.isVisible()
    assert second_preview.overlay_widget.isVisible()

    dialog.reject()
    qtbot.wait(50)

    assert not first_preview.overlay_widget.isVisible()
    assert not second_preview.overlay_widget.isVisible()


def test_move_dialog_shows_relevant_path_with_common_parent(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test dialog shows relevant path when widgets share a common parent."""
    qtbot.waitExposed(gui_app_instance.window)

    right_panel = gui_app_instance.window.right_panel
    right_panel.add_preview_widget()
    qtbot.wait(50)

    first_preview = cast(DirectoryPreviewWidget,
                         right_panel.splitter.widget(0))
    second_preview = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(1))

    sub1_dir = image_directory / "sub1"
    sub12_dir = sub1_dir / "sub12"
    first_preview.set_directory(sub1_dir)
    second_preview.set_directory(sub12_dir)
    qtbot.wait(50)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    labels = dialog.findChildren(QLabel)
    path_label = next((lbl for lbl in labels if lbl.text() == "sub1"), None)
    assert path_label is not None

    assert first_preview.overlay_label.text() == "sub1"

    # Verify the overlay widget is visible on the second (target) widget
    assert not second_preview.overlay_widget.isVisible()

    dialog.reject()
    qtbot.wait(50)


def test_move_relevant_path_last_component_only(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test relevant path shows only last component when all differ by that."""
    qtbot.waitExposed(gui_app_instance.window)

    right_panel = gui_app_instance.window.right_panel
    right_panel.add_preview_widget()
    qtbot.wait(50)

    first_preview = cast(DirectoryPreviewWidget,
                         right_panel.splitter.widget(0))
    second_preview = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(1))

    sub1_dir = image_directory / "sub1"
    sub2_dir = image_directory / "sub2"
    first_preview.set_directory(sub1_dir)
    second_preview.set_directory(sub2_dir)
    qtbot.wait(50)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    labels = dialog.findChildren(QLabel)
    path_label = next((lbl for lbl in labels if lbl.text() == "sub1"), None)
    assert path_label is not None

    assert first_preview.overlay_label.text() == "sub1"

    dialog.reject()
    qtbot.wait(50)


def test_move_relevant_path_relative_to_root(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test relevant path falls back to relative path when no common parts."""
    qtbot.waitExposed(gui_app_instance.window)

    right_panel = gui_app_instance.window.right_panel
    right_panel.add_preview_widget()
    qtbot.wait(50)

    first_preview = cast(DirectoryPreviewWidget,
                         right_panel.splitter.widget(0))
    second_preview = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(1))

    sub1_dir = image_directory / "sub1"
    sub2_sub23_dir = image_directory / "sub2" / "sub23"
    first_preview.set_directory(sub1_dir)
    second_preview.set_directory(sub2_sub23_dir)
    qtbot.wait(50)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    file_to_move = image_directory / "image_255_0_239.png"
    _click_tile(widget, file_to_move, qtbot)
    assert file_to_move in widget.selected_files

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)
    qtbot.wait(100)

    dialog = _get_visible_dialog()
    assert dialog.windowTitle().startswith("Move to ")

    labels = dialog.findChildren(QLabel)
    path_label = next((lbl for lbl in labels if lbl.text() == "sub1"), None)
    assert path_label is not None

    assert first_preview.overlay_label.text() == "sub1"

    dialog.reject()
    qtbot.wait(50)

from beartype.typing import cast
import pytest
from beartype import beartype
from image_tagger.gui.right_panel import DirectoryPreviewWidget
from utils import take_screenshot
from conftest import AppInstanceRes
from pathlib import Path
from pytestqt.qtbot import QtBot
from image_tagger.utils.utils import _generate_monotone_image
import logging
from PySide6.QtCore import QTimer, Qt, QPoint, QItemSelectionModel
from PySide6.QtTest import QTest, QSignalSpy
from PySide6.QtWidgets import (
    QApplication,
    QDialog,
    QDialogButtonBox,
    QLabel,
    QListView,
    QPushButton,
)


def _get_visible_dialog() -> QDialog:
    """Return the currently visible QDialog, or raise AssertionError."""
    dialog = next(
        (widget for widget in QApplication.topLevelWidgets()
         if isinstance(widget, QDialog) and widget.isVisible()),
        None,
    )
    assert dialog is not None, "No visible dialog found"
    return dialog


def _click_tile(widget, file_path: Path, qtbot: QtBot):
    """Click a tile in the mixed view, scrolling if necessary."""
    rect = widget.get_tile_rect(file_path)
    assert rect is not None, f"Tile {file_path} not found"
    if not widget.is_element_visible(rect):
        widget.scroll_to_content_y(rect.center().y())
        qtbot.wait(50)
        rect = widget.get_tile_rect(file_path)
        assert rect is not None
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)
    QTest.mouseClick(widget.viewport(),
                     Qt.MouseButton.LeftButton,
                     pos=click_pos)
    qtbot.wait(50)


def _accept_move_dialog(qtbot: QtBot):
    """Find the visible move dialog and accept it."""
    dialog = _get_visible_dialog()
    buttons = dialog.findChild(QDialogButtonBox)
    assert buttons is not None
    qtbot.wait(50)
    dialog.accept()
    qtbot.wait(50)


def test_capture_specific_widget(gui_app_instance: AppInstanceRes,
                                 screenshot_dir: Path, qtbot: QtBot):
    """Capture screenshot of a specific nested widget."""
    qtbot.waitExposed(gui_app_instance.window)

    # Navigate to nested widget (adjust path as needed)
    central_widget = gui_app_instance.window.centralWidget()

    # Ensure widget is visible
    qtbot.waitExposed(central_widget)

    screenshot_path = screenshot_dir / "status_panel.png"
    take_screenshot(central_widget, screenshot_path)

    # Wait for images to load
    qtbot.wait(1000)

    screenshot_path2 = screenshot_dir / "status_panel_after_load.png"
    take_screenshot(central_widget, screenshot_path2)

    assert screenshot_path.exists()
    assert screenshot_path2.exists()


def test_mixed_view_content_acces(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Capture screenshot of a specific nested widget."""
    qtbot.waitExposed(gui_app_instance.window)

    # Navigate to nested widget (adjust path as needed)
    central_widget = gui_app_instance.window.centralWidget()

    # Ensure widget is visible
    qtbot.waitExposed(central_widget)
    qtbot.wait(500)

    widget = gui_app_instance.window.get_mixed_view()

    target = image_directory / "image_255_0_239.png"
    rect = widget.get_tile_rect(target)
    assert rect is not None, f"Tile {target} not found - ensure parent is expanded"

    # Scroll if needed
    if not widget.is_element_visible(rect):
        widget.scroll_to_content_y(rect.center().y())
        qtbot.wait(50)

    # Convert content coordinates to viewport coordinates
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)

    # Click and verify signal
    spy = QSignalSpy(widget.imageClicked)
    QTest.mouseClick(widget.viewport(),
                     Qt.MouseButton.LeftButton,
                     pos=click_pos)

    screenshot_path = screenshot_dir / "status_panel.png"
    take_screenshot(central_widget, screenshot_path)

    assert spy.count() == 1
    assert spy.at(0)[0] == target
    assert target in widget.selected_files


def test_mixed_view_expand_directory_reveals_tiles(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test expanding a collapsed directory reveals its image tiles."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Find first subdirectory containing images
    subdir = image_directory.joinpath("sub1")
    toggle_rect = widget.get_toggle_rect(subdir)

    # If not found, scroll to make header visible
    if toggle_rect is None:
        header_rect = widget.get_header_rect(subdir)
        if header_rect:
            widget.scroll_to_content_y(header_rect.center().y())
            qtbot.wait(150)
            toggle_rect = widget.get_toggle_rect(subdir)

    qtbot.wait(150)
    assert toggle_rect is not None, f"Could not find toggle for directory {subdir}"

    # Verify files are not visible (directory collapsed)
    image_files = list(subdir.glob("*.png"))
    assert widget.get_tile_rect(
        image_files[0]) is None, ("Directory should start collapsed")

    # Click toggle to expand
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(toggle_rect.center().x(),
                       toggle_rect.center().y() - scroll_y)
    QTest.mouseClick(widget.viewport(),
                     Qt.MouseButton.LeftButton,
                     pos=click_pos)
    qtbot.wait(50)

    take_screenshot(central_widget, screenshot_dir / "directory_expanded.png")

    # Verify tiles are now accessible
    for img in image_files[:3]:
        rect = widget.get_tile_rect(img)
        assert rect is not None, f"Tile {img.name} should be visible after expansion"

    file_to_move = subdir / image_files[0]
    first_image_rect = widget.get_element_click_pos(file_to_move)

    QTest.mouseClick(widget.viewport(),
                     Qt.MouseButton.LeftButton,
                     pos=first_image_rect)

    QTest.keyClick(gui_app_instance.window, Qt.Key.Key_1,
                   Qt.KeyboardModifier.ControlModifier, 100)

    dialog = next(widget for widget in QApplication.topLevelWidgets()
                  if isinstance(widget, QDialog) and widget.isVisible())

    assert dialog.windowTitle().startswith("Move to ")

    label = dialog.findChild(QLabel)
    assert label is not None

    list_view = dialog.findChild(QListView)
    assert list_view is not None

    buttons = dialog.findChild(QDialogButtonBox)
    assert buttons is not None

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "dialog_prompt.png")
    take_screenshot(central_widget, screenshot_dir / "before_move.png")
    assert file_to_move.exists()
    dialog.accept()
    assert not file_to_move.exists()
    qtbot.wait(150)
    take_screenshot(central_widget, screenshot_dir / "after_move.png")

    assert not widget.get_tile_rect(subdir / image_files[0])
    assert widget.get_element_click_pos(subdir / image_files[1])
    assert widget.get_element_click_pos(subdir / image_files[2])


def test_mixed_view_ctrl_click_multi_select(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test Ctrl+Click multi-selection of image tiles."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Get first two available images
    images = [hit.file_path for hit in widget.tile_hits][:2]
    if len(images) < 2:
        # Try to expand directories to find more images
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
        images = [hit.file_path for hit in widget.tile_hits][:2]

    img1, img2 = images[0], images[1]

    # Click first image (no modifier)
    rect1 = widget.get_tile_rect(img1)
    scroll_y = widget.verticalScrollBar().value()
    assert rect1
    pos1 = QPoint(rect1.center().x(), rect1.center().y() - scroll_y)

    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=pos1)
    qtbot.wait(50)
    assert img1 in widget.selected_files

    # Ctrl+Click second image
    rect2 = widget.get_tile_rect(img2)
    assert rect2
    if not widget.is_element_visible(rect2):
        widget.scroll_to_content_y(rect2.center().y())
        qtbot.wait(50)
        scroll_y = widget.verticalScrollBar().value()

    pos2 = QPoint(rect2.center().x(), rect2.center().y() - scroll_y)
    QTest.mouseClick(
        widget.viewport(),
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.ControlModifier,
        pos2,
    )
    qtbot.wait(50)

    screenshot_path = screenshot_dir / "multi_select.png"
    take_screenshot(central_widget, screenshot_path)

    assert img1 in widget.selected_files, "First image should remain selected"
    assert img2 in widget.selected_files, "Second image should be added to selection"
    assert len(widget.selected_files) == 2


def test_mixed_view_scroll_to_offscreen_element(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test scrolling to reveal and interact with off-screen tiles."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Resize to small height to force scrolling
    original_size = gui_app_instance.window.size()
    gui_app_instance.window.resize(800, 200)
    qtbot.wait(100)

    # Get all available tiles
    all_tiles = [hit.file_path for hit in widget.tile_hits]
    # Select last tile (likely off-screen)
    target = all_tiles[-1]
    rect = widget.get_tile_rect(target)
    assert rect

    # Verify it's not fully visible before scroll
    # Scroll to make visible
    widget.scroll_to_content_y(rect.center().y())
    qtbot.wait(50)

    rect = widget.get_tile_rect(target)
    assert rect
    assert widget.is_element_visible(
        rect), "Tile should be visible after scroll"

    # Take screenshot of scrolled state
    screenshot_path = screenshot_dir / "scrolled_to_tile.png"
    take_screenshot(central_widget, screenshot_path)

    # Verify interaction works after scrolling
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)

    spy = QSignalSpy(widget.imageClicked)
    QTest.mouseClick(widget.viewport(),
                     Qt.MouseButton.LeftButton,
                     pos=click_pos)

    assert spy.count() == 1
    assert spy.at(0)[0] == target
    assert target in widget.selected_files

    # Restore original size
    gui_app_instance.window.resize(original_size)


def test_mixed_view_double_click_emits_file_selected(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test double-clicking a tile emits fileSelected signal."""

    # gui_app_instance.repo.replace_probabilistic_annotations()

    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    target = widget.tile_hits[0].file_path
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[
            ("general", "mixed", 0.45),
            ("general", "random", 0.5),
            ("general", "castle", 0.4),
        ],
    )

    rect = widget.get_tile_rect(target)
    assert rect

    if not widget.is_element_visible(rect):
        widget.scroll_to_content_y(rect.center().y())
        qtbot.wait(50)
        rect = widget.get_tile_rect(target)
        assert rect

    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)

    spy = QSignalSpy(widget.fileSelected)
    QTest.mouseDClick(widget.viewport(),
                      Qt.MouseButton.LeftButton,
                      pos=click_pos)
    qtbot.wait(50)

    screenshot_path = screenshot_dir / "double_click_selection.png"
    take_screenshot(central_widget, screenshot_path)

    assert spy.count() == 1
    assert spy.at(0)[0] == str(target)
    assert target in widget.selected_files

    prob_table = gui_app_instance.window.get_probability_tags_table()

    assert prob_table.rowCount() == 3
    assert prob_table.item(0, 1).text() == "random"  # type: ignore
    assert prob_table.item(1, 1).text() == "mixed"  # type: ignore
    assert prob_table.item(2, 1).text() == "castle"  # type: ignore

    assert "0.5" in prob_table.item(0, 2).text()  # type: ignore
    assert "0.45" in prob_table.item(1, 2).text()  # type: ignore
    assert "0.4" in prob_table.item(2, 2).text()  # type: ignore


def _setup_search_tab(gui_app_instance: AppInstanceRes, qtbot: QtBot):
    """Switch to the search tab and return the SearchTab widget."""
    left_panel = gui_app_instance.window.left_panel
    left_panel.tabs.setCurrentIndex(1)
    qtbot.wait(50)
    return left_panel.search_view


def test_search_by_probabilistic_tag(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by probabilistic tag."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[
            ("general", "castle", 0.8),
            ("general", "random", 0.3),
        ],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[
            ("general", "castle", 0.6),
        ],
    )

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_255.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id3,
        items=[
            ("general", "castle", 0.4),
        ],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "general",
        "name": "castle",
        "min_probability": 0.5,
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_prob_tag.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_by_regular_tag(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by regular tag."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id1,
        items=[("category1", "tag_a"), ("category2", "tag_b")],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id2,
        items=[("category1", "tag_a")],
    )

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_159.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id3,
        items=[("category1", "tag_c")],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "regular_tag",
        "category": "category1",
        "name": "tag_a",
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_regular_tag.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_by_description(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by description text."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.set_description(
        image_id=image_id1, description="a beautiful sunset over the ocean")

    target2 = image_directory / "sub1" / "sub12" / "image_31_255_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.set_description(
        image_id=image_id2, description="a beautiful mountain landscape")

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_207.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.set_description(image_id=image_id3,
                                          description="just a random picture")

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "description",
        "text": "beautiful",
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_description.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_by_path_contains(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by path containing a substring."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "path_contains",
        "text": "sub12",
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_path.png")

    results = search_tab.get_result_images()
    assert len(results) == 8

    subdir = image_directory / "sub1" / "sub12"
    for img in subdir.glob("*.png"):
        assert str(img) in [str(r) for r in results]

    assert "Found 8 images" in search_tab.status_label.text()


def test_search_and_combination(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching with AND combination of conditions."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id1,
        items=[("category1", "tag_a")],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.7)],
    )

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_159.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id3,
        items=[("category1", "tag_a")],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type":
        "and",
        "children": [
            {
                "type": "probabilistic_tag",
                "category": "general",
                "name": "castle",
                "min_probability": 0.5,
            },
            {
                "type": "regular_tag",
                "category": "category1",
                "name": "tag_a",
            },
        ],
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_and.png")

    results = search_tab.get_result_images()
    assert len(results) == 1
    assert str(target1) in [str(r) for r in results]
    assert str(target2) not in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 1 images" in search_tab.status_label.text()


def test_search_or_combination(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching with OR combination of conditions."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id2,
        items=[("category1", "tag_a")],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type":
        "or",
        "children": [
            {
                "type": "probabilistic_tag",
                "category": "general",
                "name": "castle",
                "min_probability": 0.5,
            },
            {
                "type": "regular_tag",
                "category": "category1",
                "name": "tag_a",
            },
        ],
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_or.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_not_condition(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching with NOT condition."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "not",
        "child": {
            "type": "probabilistic_tag",
            "category": "general",
            "name": "castle",
            "min_probability": 0.5,
        },
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_not.png")

    results = search_tab.get_result_images()
    result_strs = [str(r) for r in results]
    assert str(target1) not in result_strs
    assert str(target2) in result_strs
    assert "Found" in search_tab.status_label.text()


def test_search_no_results(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test search that returns no results."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "nonexistent_category",
        "name": "nonexistent_tag",
        "min_probability": 0.5,
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_no_results.png")

    results = search_tab.get_result_images()
    assert len(results) == 0
    assert "No images found" in search_tab.status_label.text()


def test_search_empty_spec(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test search with no valid conditions."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_empty.png")

    results = search_tab.get_result_images()
    assert len(results) == 0
    assert "No valid filter" in search_tab.status_label.text()


def test_search_double_click_emits_file_selected(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test double-clicking a search result emits fileSelected signal."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "test_tag", 0.9)],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "general",
        "name": "test_tag",
        "min_probability": 0.5,
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_double_click.png")

    results = search_tab.get_result_images()
    assert len(results) == 1
    assert str(target) == str(results[0])

    spy = QSignalSpy(gui_app_instance.window.left_panel.fileSelected)

    thumbnail_list = search_tab.thumbnail_list.list_view
    index = thumbnail_list.model().index(0, 0)
    thumbnail_list.doubleClicked.emit(index)
    qtbot.wait(50)

    assert spy.count() == 1
    assert spy.at(0)[0] == str(target)


def test_search_via_add_tag_to_query(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test adding tags to query via add_tag_to_query method."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "castle", 0.7)],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.6)],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    search_tab.add_tag_to_query("probabilistic_tag", "general", "castle")
    qtbot.wait(50)

    take_screenshot(central_widget, screenshot_dir / "search_add_tag.png")

    search_tab.execute_search()
    qtbot.wait(100)

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_sexp_text_input(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering an S-expression directly into the text field."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.6)],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    # Type S-expression directly
    sexp = "(probabilistic_tag general castle 0.5)"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_text.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_sexp_and_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering a compound AND S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id1,
        items=[("category1", "tag_a")],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.7)],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = "(and (probabilistic_tag general castle 0.5) (regular_tag category1 tag_a))"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_and.png")

    results = search_tab.get_result_images()
    assert len(results) == 1
    assert str(target1) in [str(r) for r in results]


def test_search_sexp_or_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering a compound OR S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id2,
        items=[("category1", "tag_a")],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = "(or (probabilistic_tag general castle 0.5) (regular_tag category1 tag_a))"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_or.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]


def test_search_sexp_not_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering a NOT S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = "(not (probabilistic_tag general castle 0.5))"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_not.png")

    results = search_tab.get_result_images()
    result_strs = [str(r) for r in results]
    assert str(target1) not in result_strs
    assert str(target2) in result_strs


def test_search_sexp_add_tag_rewrites_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that add_tag_to_query rewrites an existing S-expression correctly."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "castle", 0.7)],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.6)],
    )

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    # Start with one tag
    search_tab.add_tag_to_query("probabilistic_tag", "general", "castle")
    qtbot.wait(50)

    first_text = search_tab.sexp_input.toPlainText().strip()
    assert first_text == "(probabilistic_tag general castle 0.5)"

    # Add another tag - should wrap in 'and'
    search_tab.add_tag_to_query("probabilistic_tag", "general", "castle")
    qtbot.wait(50)

    second_text = search_tab.sexp_input.toPlainText().strip()
    assert (
        second_text ==
        "(and (probabilistic_tag general castle 0.5) (probabilistic_tag general castle 0.5))"
    )

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_rewrite.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]


def test_search_sexp_invalid_input(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that invalid S-expressions show an error instead of crashing."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    search_tab.sexp_input.setPlainText("(and (incomplete")
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_invalid.png")

    assert "Parse error" in search_tab.status_label.text()
    results = search_tab.get_result_images()
    assert len(results) == 0


def test_search_sexp_description_with_quotes(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test description search with quoted string in S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.set_description(
        image_id=image_id1, description="a beautiful sunset over the ocean")

    target2 = image_directory / "sub1" / "sub12" / "image_31_255_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.set_description(
        image_id=image_id2, description="a beautiful mountain landscape")

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = '(description "beautiful")'
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget,
                    screenshot_dir / "search_sexp_description.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]


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

    label = dialog.findChild(QLabel)
    assert label is not None
    assert "1 items" in label.text()
    assert "sub1" in label.text()

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

    label = dialog.findChild(QLabel)
    assert label is not None
    assert "2 items" in label.text()
    assert "sub2" in label.text()

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

    label = dialog.findChild(QLabel)
    assert label is not None
    assert "sub1" in label.text()

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_second_output_dialog.png")

    assert file_to_move.exists()
    dialog.accept()
    qtbot.wait(150)

    assert not file_to_move.exists()
    assert (sub1_dir / "image_255_0_239.png").exists()


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

    label = dialog.findChild(QLabel)
    assert label is not None
    assert "sub12" in label.text()

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_navigated_dialog.png")

    assert file_to_move.exists()
    dialog.accept()
    qtbot.wait(150)

    assert not file_to_move.exists()
    assert (sub12_dir / "image_255_0_239.png").exists()


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

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
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
    assert "sub1" in dialog.windowTitle()

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "move_search_dialog.png")

    assert target.exists()
    dialog.accept()
    qtbot.wait(150)

    assert not target.exists()
    assert (sub1_dir / "image_255_0_239.png").exists()


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

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
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

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
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

    search_tab = _setup_search_tab(gui_app_instance, qtbot)
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

    right_panel = gui_app_instance.window.right_panel
    preview_widget = cast(DirectoryPreviewWidget,
                          right_panel.splitter.widget(0))
    target_dir = preview_widget.current_dir
    assert (target_dir / f"random.png").exists()
    assert (target_dir / f"random_{file2_md5[:8]}.png").exists() or (
        target_dir / f"random_{file1_md5[:8]}.png").exists()

from beartype import beartype
from utils import take_screenshot
from conftest import AppInstanceRes
from pathlib import Path
from pytestqt.qtbot import QtBot
import logging
from PySide6.QtCore import Qt, QPoint
from PySide6.QtTest import QTest, QSignalSpy


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

    logging.info(f"saved screenshot to {screenshot_path}")

    # Wait for images to load
    qtbot.wait(1000)

    screenshot_path2 = screenshot_dir / "status_panel_after_load.png"
    take_screenshot(central_widget, screenshot_path2)

    logging.info(f"saved screenshot to {screenshot_path2}")

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

    logging.info(f"saved screenshot to {screenshot_path}")

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
    subdirs = [d for d in image_directory.iterdir() if d.is_dir()]

    subdir = subdirs[0]
    toggle_rect = widget.get_toggle_rect(subdir)

    # If not found, scroll to make header visible
    if toggle_rect is None:
        header_rect = widget.get_header_rect(subdir)
        if header_rect:
            widget.scroll_to_content_y(header_rect.center().y())
            qtbot.wait(50)
            toggle_rect = widget.get_toggle_rect(subdir)

    assert toggle_rect is not None, f"Could not find toggle for directory {subdir}"

    # Verify children are not visible (directory collapsed)
    child_images = list(subdir.glob("*.png"))
    if child_images:
        assert widget.get_tile_rect(
            child_images[0]) is None, "Directory should start collapsed"

    # Click toggle to expand
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(toggle_rect.center().x(),
                       toggle_rect.center().y() - scroll_y)
    QTest.mouseClick(widget.viewport(),
                     Qt.MouseButton.LeftButton,
                     pos=click_pos)
    qtbot.wait(50)

    screenshot_path = screenshot_dir / "directory_expanded.png"
    take_screenshot(central_widget, screenshot_path)
    logging.info(f"saved screenshot to {screenshot_path}")

    # Verify tiles are now accessible
    if child_images:
        for img in child_images[:3]:
            rect = widget.get_tile_rect(img)
            assert rect is not None, f"Tile {img.name} should be visible after expansion"


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
    QTest.mouseClick(widget.viewport(),
                    Qt.MouseButton.LeftButton,
                    Qt.KeyboardModifier.ControlModifier,
                    pos2)
    qtbot.wait(50)

    screenshot_path = screenshot_dir / "multi_select.png"
    take_screenshot(central_widget, screenshot_path)
    logging.info(f"saved screenshot to {screenshot_path}")

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
    logging.info(f"saved screenshot to {screenshot_path}")

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
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    target = widget.tile_hits[0].file_path
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
    logging.info(f"saved screenshot to {screenshot_path}")

    assert spy.count() == 1
    assert spy.at(0)[0] == str(target)
    assert target in widget.selected_files

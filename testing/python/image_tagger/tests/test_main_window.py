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
    qtbot.wait(1000)


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
    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=click_pos)
    
    screenshot_path = screenshot_dir / "status_panel.png"
    take_screenshot(central_widget, screenshot_path)

    logging.info(f"saved screenshot to {screenshot_path}")


    assert spy.count() == 1
    assert spy.at(0)[0] == target
    assert target in widget.selected_files

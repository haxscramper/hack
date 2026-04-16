from beartype import beartype
from utils import take_screenshot
from conftest import AppInstanceRes
from pathlib import Path
from pytestqt.qtbot import QtBot
import logging


def test_capture_specific_widget(
    gui_app_instance: AppInstanceRes, screenshot_dir: Path, qtbot: QtBot
):
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
    qtbot.wait(2000)

    screenshot_path2 = screenshot_dir / "status_panel_after_load.png"
    take_screenshot(central_widget, screenshot_path2)

    logging.info(f"saved screenshot to {screenshot_path2}")

    assert screenshot_path.exists()
    assert screenshot_path2.exists()

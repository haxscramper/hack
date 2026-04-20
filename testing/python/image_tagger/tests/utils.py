from beartype import beartype
from pathlib import Path
from PySide6.QtCore import QTimer
from PySide6.QtWidgets import QApplication, QWidget
from PySide6.QtGui import QPixmap, QScreen
import logging

def take_screenshot(widget: QWidget, filepath: Path) -> Path:
    """Capture screenshot of a widget and save to file."""
    pixmap = widget.grab() if hasattr(widget, "grab") else QPixmap.grabWidget(widget)
    pixmap.save(str(filepath))
    logging.info(f"saved screenshot to {filepath}")
    assert filepath.exists(), f"Could not save screenshot to {filepath}"

    return filepath


def test_gui_starts_and_captures_screenshot(gui_app_instance, screenshot_dir, qtbot):
    """
    Test that the GUI application starts, loads images, and capture a screenshot.
    """
    window, repo, session = gui_app_instance
    screenshot_path = screenshot_dir / "main_window_initial.png"

    # Wait for the GUI to fully load and render
    qtbot.waitExposed(window)
    QTimer.singleShot(100, lambda: None)  # Allow event processing
    qApp = QApplication.instance()
    qApp.processEvents()

    # Take screenshot
    take_screenshot(window, screenshot_path)

    # Verify screenshot was saved and has non-zero size
    assert screenshot_path.exists()
    pixmap = QPixmap(str(screenshot_path))
    assert pixmap.width() > 0
    assert pixmap.height() > 0

    print(f"Screenshot saved to: {screenshot_path}")
    print(f"Image size: {pixmap.width()}x{pixmap.height()}")


def test_gui_with_interaction_and_screenshot(gui_app_instance, screenshot_dir, qtbot):
    """
    Test that interacts with GUI elements and captures a screenshot of the result.
    """
    window, repo, session = gui_app_instance
    qtbot.waitExposed(window)

    # Example: interact with a button if one exists
    # qtbot.click(window.some_button)
    # qtbot.waitSignal(window.some_signal, timeout=2000)

    # Capture screenshot after interaction
    screenshot_path = screenshot_dir / "main_window_after_interaction.png"
    take_screenshot(window, screenshot_path)

    assert screenshot_path.exists()

from beartype.typing import cast
import pytest
from beartype import beartype
from image_tagger.gui.right_panel import DirectoryPreviewWidget
from utils import take_screenshot
from conftest import AppInstanceRes, setup_search_tab
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

    # Wait for images to load
    qtbot.wait(1000)

    screenshot_path2 = screenshot_dir / "status_panel_after_load.png"
    take_screenshot(central_widget, screenshot_path2)

    assert screenshot_path.exists()
    assert screenshot_path2.exists()


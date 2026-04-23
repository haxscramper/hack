import pytest
import colorsys
from PySide6.QtCore import QPoint, QTimer, Qt
from PySide6.QtWidgets import QApplication, QDialog, QDialogButtonBox
from PySide6.QtGui import QPixmap, QScreen
import shutil
from pathlib import Path
from PIL import Image
from image_tagger.db.repository import Repository
from image_tagger.gui.main_window import MainWindow
from image_tagger.utils.utils import _populate_template_directory, TEMPLATE_DIR
from beartype.typing import Generator
from sqlalchemy.orm.session import Session
from dataclasses import dataclass
from pytestqt.qtbot import QtBot
from beartype import beartype
import logging
from beartype.typing import Any
from PySide6.QtTest import QTest


@beartype
def pytest_configure(config: Any) -> None:
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
    )


def get_test_dir(
        request: pytest.FixtureRequest,
        test_dir_root: Path = Path("/tmp/image_tagger_tests/test_out"),
) -> Path:
    import hashlib
    from pathlib import Path

    # Get test file path relative to tests directory
    test_file_path = Path(request.path)
    tests_root = None

    # Find the 'tests' directory in the path
    for parent in test_file_path.parents:
        if parent.name == "tests":
            tests_root = parent
            break

    if tests_root is None:
        raise ValueError(
            f"Could not find 'tests' directory in path: {test_file_path}")

    # Get relative path from tests directory, without .py extension
    rel_path = test_file_path.relative_to(tests_root).with_suffix("")

    # Build base directory path
    base_dir = test_dir_root / rel_path

    # Add test function name
    test_name = request.node.name

    # Handle parametrized tests
    if hasattr(request.node, "callspec") and request.node.callspec.params:
        params_items = sorted(request.node.callspec.params.items())
        params_str = "_".join(f"{k}={v}" for k, v in params_items)

        if len(params_str) <= 32:
            # Use parameters as-is if short enough
            final_dir = base_dir / test_name / params_str
        else:
            # Use first 24 chars + hex digest for long parameters
            params_prefix = params_str[:24]
            params_hash = hashlib.md5(params_str.encode()).hexdigest()[:8]
            final_dir = base_dir / test_name / f"{params_prefix}_{params_hash}"
    else:
        final_dir = base_dir / test_name

    return final_dir


@pytest.fixture
def stable_test_dir(request: pytest.FixtureRequest) -> Path:
    import shutil

    final_dir = get_test_dir(request)

    # Clean and create directory
    if final_dir.exists():
        shutil.rmtree(final_dir)

    final_dir.mkdir(parents=True, exist_ok=True)

    return final_dir


@pytest.fixture(scope="session", autouse=True)
def setup_session():
    if TEMPLATE_DIR.exists():
        shutil.rmtree(TEMPLATE_DIR)


@pytest.fixture
def image_directory(request: pytest.FixtureRequest):
    """
    Fixture providing a temporary directory with 256 monotone color images (512x512).

    Uses a template directory at /tmp/image_tagger_tests/image_template_directory
    to cache generated images and improve test speed by copying instead of regenerating.
    """
    # Ensure template directory is populated
    if not TEMPLATE_DIR.exists():
        _populate_template_directory(TEMPLATE_DIR)

    # Copy template to temporary directory
    dest_dir = get_test_dir(request) / "images"
    if dest_dir.exists():
        shutil.rmtree(dest_dir)

    shutil.copytree(TEMPLATE_DIR, dest_dir)

    yield dest_dir


@pytest.fixture(scope="session")
def qapp():
    """Fixture providing QApplication instance."""
    from PySide6.QtWidgets import QApplication

    app = QApplication.instance() or QApplication([])
    yield app
    app.quit()


@pytest.fixture
def screenshot_dir(request: pytest.FixtureRequest):
    """Fixture providing directory for test screenshots."""
    screenshot_dir = get_test_dir(request) / "screenshots"
    screenshot_dir.mkdir(exist_ok=True, parents=True)
    yield screenshot_dir


@dataclass
@beartype
class AppInstanceRes:
    window: MainWindow
    repo: Repository
    session: Session
    root_dir: Path

    @beartype
    def get_image_id(self, path: Path) -> int:
        assert path.exists()
        return self.repo.upsert_image(self.root_dir, path).id


@pytest.fixture
def gui_app_instance(image_directory: Path,
                     qtbot: QtBot) -> Generator[AppInstanceRes]:
    """
    Fixture that initializes the GUI application with test data.
    Returns tuple of (window, repo) for interaction.
    """
    import image_tagger.config as config
    from image_tagger.db.session import init_db, make_session_factory

    root_dir = image_directory.resolve()
    config.init_config(root_dir)

    engine = init_db(root_dir / config.config.SQLITE_FILENAME)
    session_factory = make_session_factory(engine)
    session = session_factory()
    repo = Repository(session)

    window = MainWindow(root_dir, repo)
    qtbot.addWidget(window)
    window.show()
    qtbot.waitExposed(window)

    yield AppInstanceRes(
        window=window,
        repo=repo,
        session=session,
        root_dir=root_dir,
    )

    # Cleanup
    window.close()
    session.close()


def setup_search_tab(gui_app_instance: AppInstanceRes, qtbot: QtBot):
    """Switch to the search tab and return the SearchTab widget."""
    left_panel = gui_app_instance.window.left_panel
    left_panel.tabs.setCurrentIndex(1)
    qtbot.wait(50)
    return left_panel.search_view


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


def _get_visible_dialog() -> QDialog:
    """Return the currently visible QDialog, or raise AssertionError."""
    dialog = next(
        (widget for widget in QApplication.topLevelWidgets()
         if isinstance(widget, QDialog) and widget.isVisible()),
        None,
    )
    assert dialog is not None, "No visible dialog found"
    return dialog

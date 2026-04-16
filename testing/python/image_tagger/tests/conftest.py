import pytest
import colorsys
from PySide6.QtCore import QTimer
from PySide6.QtWidgets import QApplication
from PySide6.QtGui import QPixmap, QScreen
import shutil
from pathlib import Path
from PIL import Image
from image_tagger.db.repository import Repository
from image_tagger.gui.main_window import MainWindow
from typing import Generator
from sqlalchemy.orm.session import Session
from dataclasses import dataclass
from pytestqt.qtbot import QtBot
from beartype import beartype
import logging
from beartype.typing import Any


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


TEMPLATE_DIR = Path("/tmp/image_tagger_tests/image_template_directory")

DIRECTORY_STRUCTURE = {
    "sub1": {
        "sub11": 0,
        "sub12": 8,
        "sub13": 2,
    },
    "sub2": {
        "sub21": 0,
        "sub22": 0,
        "sub23": {
            "sub231": 2,
            "sub232": 2,
        },
    },
}


def _generate_monotone_image(
        path: Path,
        size: tuple[int, int] = (512, 512),
        color: tuple[int, int, int] = (128, 128, 128),
):
    """Generate a monotone color image and save it to the given path."""
    img = Image.new("RGB", size, color)
    img.save(path)


def _count_specified_images(structure: dict) -> int:
    """Recursively count total images specified in directory structure."""
    total = 0
    for key, value in structure.items():
        if isinstance(value, dict):
            total += _count_specified_images(value)
        else:
            total += value
    return total


def _populate_template_directory(
        template_dir: Path,
        num_images: int = 48,
        size: tuple[int, int] = (512, 512),
):
    """Populate the template directory with monotone color images."""
    import shutil
    if template_dir.exists():
        shutil.rmtree(template_dir)
    template_dir.mkdir(parents=True)

    # Count images needed for subdirectories
    specified_count = _count_specified_images(DIRECTORY_STRUCTURE)
    root_level_count = num_images - specified_count

    # Define where each image should go
    image_destinations: list[Path | None] = []

    def collect_destinations(structure: dict, parent: Path):
        for name, value in structure.items():
            subdir = parent / name
            if isinstance(value, dict):
                collect_destinations(value, subdir)
            else:
                for _ in range(value):
                    image_destinations.append(subdir)

    collect_destinations(DIRECTORY_STRUCTURE, template_dir)
    # Fill rest with root level images
    for _ in range(root_level_count):
        image_destinations.append(template_dir)

    # Generate and place images
    for i, dest in enumerate(image_destinations):
        if dest is not None:
            dest.mkdir(parents=True, exist_ok=True)
            hue = i / num_images
            color = tuple(int(c * 255) for c in colorsys.hsv_to_rgb(hue, 1.0, 1.0))
            _generate_monotone_image(dest / f"image_{color[0]}_{color[1]}_{color[2]}.png", size, color)


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
class AppInstanceRes():
    window: MainWindow
    repo: Repository
    session: Session


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

    yield AppInstanceRes(window=window, repo=repo, session=session)

    # Cleanup
    window.close()
    session.close()

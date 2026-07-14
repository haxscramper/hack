import logging
from pathlib import Path
import re
import shutil

import pytest
from beartype.typing import Any, Generator

from index_service.gui.common.directory_view_utils import TEMPLATE_DIR, _populate_template_directory
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import BaseResource, RunContext
from index_service.services.resources.flm_server import FlmRequest, FlmResponse, FlmServerResource
from index_service.services.resources.text_summary import (
    SummarizeRequest,
    TextSummaryResource,
    TextSummaryResult,
)
from index_service.services.utils import get_custom_traceback_handler, stfu_logs
import logging

log = logging.getLogger(__name__)

ARANGO_HOST = "http://localhost:8529"
ARANGO_USER = "root"
ARANGO_PASSWORD = "test"

handler = get_custom_traceback_handler(show_args=False,)


def pytest_configure(config: Any) -> None:
    "nodoc"
    stfu_logs()

    logging.getLogger("index_service.services.core.job_runtime").setLevel(logging.INFO)


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    outcome = yield
    report = outcome.get_result()

    if report.when != "call" or report.passed:
        return

    excinfo = call.excinfo
    if excinfo is None:
        return

    exc_type, exc_value, exc_tb = excinfo._excinfo
    report.longrepr = str(handler(exc_type, exc_value, exc_tb))


def _safe_name(raw: str) -> str:
    return re.sub(r"[^a-zA-Z0-9_]+", "_", raw).lower()


@pytest.fixture
def db(request) -> IndexDatabase:
    db_name = f"index_test_{_safe_name(request.node.name)}"
    IndexDatabase.reset_database(
        host=ARANGO_HOST,
        db_name=db_name,
        username=ARANGO_USER,
        password=ARANGO_PASSWORD,
    )
    return IndexDatabase(
        host=ARANGO_HOST,
        db_name=db_name,
        username=ARANGO_USER,
        password=ARANGO_PASSWORD,
    )


class MockFlmServerResource(FlmServerResource):

    def __init__(self,
                 base_url: str | None = None,
                 api_key: str = "flm",
                 host: str | None = None,
                 port: int | None = None,
                 serve_cmd: list[str] | None = None,
                 startup_timeout_sec: float = 20) -> None:
        pass

    def handle(self, ctx: RunContext, request: FlmRequest,
               resources: dict[str, BaseResource]) -> FlmResponse:
        return FlmResponse(
            model=f"flm-response-to",
            content=f"flm-content:({request.messages[-1].content})",
            finish_reason="reason",
            usage=dict(),
        )


@pytest.fixture
def runtime(db) -> Generator[IndexRuntime, None, None]:
    from index_service.services.default_job_types import (
        DEFAULT_CONVERTER_TYPES,
        DEFAULT_INDEXER_TYPES,
        DEFAULT_RESOURCE_TYPES,
    )

    ctx = RunContext(db)

    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        resource_types=[t() for t in DEFAULT_RESOURCE_TYPES] + [MockFlmServerResource()],
        indexer_types=[t(should_load_cache=False) for t in DEFAULT_INDEXER_TYPES],
        converter_types=[t() for t in DEFAULT_CONVERTER_TYPES],
    )

    yield rt


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
        raise ValueError(f"Could not find 'tests' directory in path: {test_file_path}")

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
    from PyQt6.QtWidgets import QApplication

    app = QApplication.instance() or QApplication([])
    yield app
    app.quit()


@pytest.fixture
def screenshot_dir(request: pytest.FixtureRequest):
    """Fixture providing directory for test screenshots."""
    screenshot_dir = get_test_dir(request) / "screenshots"
    screenshot_dir.mkdir(exist_ok=True, parents=True)
    yield screenshot_dir

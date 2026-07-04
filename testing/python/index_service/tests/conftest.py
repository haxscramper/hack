import logging
import re

import pytest
from beartype.typing import Any, Generator

from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import BaseResource, RunContext
from index_service.services.resources.text_summary import (
    SummarizeRequest,
    TextSummaryResult,
)
from index_service.services.utils import get_custom_traceback_handler, stfu_logs

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


class MockTextSummaryResource(BaseResource):
    resource_key = "text_summary"

    def handle(
        self,
        ctx: RunContext,
        request: SummarizeRequest,
        resources: dict[str, BaseResource],
    ) -> TextSummaryResult:
        text = request.text.strip().replace("\n", " ")
        return TextSummaryResult(summary=f"mock-summary: {text[:48]}")


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
        resource_types=[
            MockTextSummaryResource() if t.resource_key == "text_summary" else t()
            for t in DEFAULT_RESOURCE_TYPES
        ],
        indexer_types=[t(should_load_cache=False) for t in DEFAULT_INDEXER_TYPES],
        converter_types=[t() for t in DEFAULT_CONVERTER_TYPES],
    )

    yield rt

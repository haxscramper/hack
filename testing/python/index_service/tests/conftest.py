import logging
import re

import pytest
from beartype.typing import Any, Generator

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
        log.info(request)
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

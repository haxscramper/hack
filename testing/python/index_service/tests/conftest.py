import logging
import os
from pathlib import Path
import re
import traceback
from dataclasses import dataclass

import pytest
from beartype.typing import Any

from index_service.services.db import IndexDatabase
from index_service.services.harness import BaseResource
from index_service.services.resources.flm_gemma import FlmSummaryResult, SummarizeRequest
from index_service.services.runtime import IndexRuntime
from index_service.services.utils import get_custom_traceback_handler

ARANGO_HOST = "http://localhost:8529"
ARANGO_USER = "root"
ARANGO_PASSWORD = "test"

handler = get_custom_traceback_handler(show_args=False, )


def pytest_configure(config: Any) -> None:
    "nodoc"
    for logger_name in [
            "openai._base_client",
            "git.cmd",
            "alembic.runtime.plugins",
            "alembic.runtime.migration",
            "git.util",
            # toggle this to see database interactions in tests
            "urllib3.connectionpool",
            # openai uses this for connection
            "httpcore.connection",
            # each individual resource actor creation and start
            "pykka",
            # execution of individual steps is printed to stderr?
            "dagster",
            "dagster.builtin",
            "asyncio",
    ]:
        logger = logging.getLogger(logger_name)
        logger.disabled = True


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


class MockFlmGemmaResource(BaseResource):
    resource_key = "flm_gemma"

    def handle(self, request: SummarizeRequest) -> FlmSummaryResult:
        text = request.text.strip().replace("\n", " ")
        return FlmSummaryResult(summary=f"mock-summary: {text[:48]}")


@pytest.fixture
def runtime() -> IndexRuntime:
    rt = IndexRuntime(resource_overrides={"flm_gemma": MockFlmGemmaResource})
    try:
        yield rt
    finally:
        rt.stop()

import re

import pytest
from index_service.db import IndexDatabase

ARANGO_HOST = "http://localhost:8529"
ARANGO_USER = "root"
ARANGO_PASSWORD = "test"


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

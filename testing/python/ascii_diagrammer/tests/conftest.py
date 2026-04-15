import pytest

from typing import Any
import logging


def pytest_configure(config: Any) -> None:
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
    )


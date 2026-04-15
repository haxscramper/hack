import pytest

from beartype import beartype
from beartype.typing import Any
import logging


@beartype
def pytest_configure(config: Any) -> None:
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
    )


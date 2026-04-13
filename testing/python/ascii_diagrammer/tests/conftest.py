import pytest
from beartype import beartype

from diagrammer.parser import parse
from diagrammer.resolver import resolve
from diagrammer.renderer import render
from typing import Any
import logging


@beartype
def pytest_configure(config: Any) -> None:
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
    )


@pytest.fixture
@beartype
def pipeline():
    """Returns a function that takes DSL source and returns rendered string."""

    def _run(
        source: str,
        charset: str = "unicode",
        scale: float = 1.0,
        canvas_width: float | None = None,
        canvas_height: float | None = None,
    ) -> str:
        statements = parse(source)
        scene = resolve(
            statements, canvas_width=canvas_width, canvas_height=canvas_height
        )
        return render(scene, charset=charset, scale=scale)

    return _run

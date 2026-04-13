import pytest

from diagrammer.parser import parse
from diagrammer.resolver import resolve
from diagrammer.renderer import render


@pytest.fixture
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
        scene = resolve(statements,
                        canvas_width=canvas_width,
                        canvas_height=canvas_height)
        return render(scene, charset=charset, scale=scale)

    return _run

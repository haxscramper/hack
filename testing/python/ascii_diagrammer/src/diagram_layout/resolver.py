"""High-level public API for diagram resolution."""

from __future__ import annotations

import json
from pathlib import Path
from beartype import beartype

from pydantic import ValidationError as PydanticValidationError

from .expand import expand_diagram
from .graph import build_dependency_graph
from .schema import DiagramInput
from .solver import ConstraintBuilder
from .types import ResolvedDiagram


@beartype
def resolve_diagram(diagram: DiagramInput) -> ResolvedDiagram:
    """Resolve a validated diagram input into absolute shapes."""
    expanded = expand_diagram(diagram)
    _ = build_dependency_graph(expanded)
    builder = ConstraintBuilder(expanded)
    builder.build()
    return builder.solve()


@beartype
def resolve_json_bytes(data: bytes) -> ResolvedDiagram:
    """Parse JSON bytes into a DiagramInput and resolve it."""
    parsed = DiagramInput.model_validate_json(data)
    return resolve_diagram(parsed)


@beartype
def resolve_json_file(path: str | Path) -> ResolvedDiagram:
    """Load, validate, and resolve a diagram from a JSON file."""
    return resolve_json_bytes(Path(path).read_bytes())


@beartype
def resolved_to_json(diagram: ResolvedDiagram) -> str:
    """Serialize resolved output to JSON."""
    return diagram.model_dump_json(indent=2)


@beartype
def parse_json_bytes(data: bytes) -> DiagramInput:
    """Parse bytes into the Pydantic input model.

    This helper exists mainly for tests and CLI wiring.
    """
    try:
        raw = json.loads(data)
    except json.JSONDecodeError:
        raise
    try:
        return DiagramInput.model_validate(raw)
    except PydanticValidationError:
        raise

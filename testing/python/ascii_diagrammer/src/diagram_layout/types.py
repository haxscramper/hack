"""Internal resolved/output data structures."""

from __future__ import annotations

from typing import Optional

from pydantic import BaseModel


class ResolvedPoint(BaseModel):
    """Absolute point in final resolved coordinates."""

    x: float
    y: float


class ResolvedShape(BaseModel):
    """Resolved absolute geometry for a single shape."""

    id: str
    shape_type: str
    x: float
    y: float
    w: float
    h: float
    text: Optional[str] = None
    points: Optional[list[ResolvedPoint]] = None


class ResolvedDiagram(BaseModel):
    """Top-level resolved result."""

    canvas_width: float
    canvas_height: float
    shapes: list[ResolvedShape]

"""Dependency graph extraction.

The dependency graph is used for diagnostics and processing insight only.
Cycles are allowed and are not treated as invalid by themselves.
"""

from __future__ import annotations
from beartype import beartype

from .expand import ExpandedDiagram
from .schema import (
    Add,
    Conjunction,
    HorizontalAlign,
    MultiShapeConstraint,
    PercentOfRefPos,
    PercentOfRefSize,
    RelativePlacement,
    Scale,
    SpacedBy,
    Sub,
    VecOffset,
    VerticalAlign,
)


@beartype
def build_dependency_graph(expanded: ExpandedDiagram) -> dict[str, set[str]]:
    """Build a shape->dependencies graph from shape expressions and global constraints."""
    graph: dict[str, set[str]] = {shape_id: set() for shape_id in expanded.shapes}

    @beartype
    def collect_position(expr) -> set[str]:
        refs: set[str] = set()
        match expr:
            case PercentOfRefPos(ref=ref) if ref is not None:
                refs.add(ref)
            case RelativePlacement():
                refs.add(expr.target.shape_id)
            case _ if expr.type == "inside-of":
                refs.add(expr.target)
            case _ if expr.type == "align-with":
                refs.update(a.shape_id for a in expr.anchors)
            case VecOffset():
                refs.update(collect_position(expr.base))
            case Add() | Sub():
                refs.update(collect_position(expr.left))
                refs.update(collect_position(expr.right))
            case Scale():
                refs.update(collect_position(expr.expr))
            case Conjunction():
                for sub in expr.exprs:
                    refs.update(collect_position(sub))
        return refs

    for shape_id, shape in expanded.shapes.items():
        graph[shape_id].update(collect_position(shape.position))
        if isinstance(shape.size, PercentOfRefSize):
            if shape.size.w.pct is not None and shape.size.w.ref is not None:
                graph[shape_id].add(shape.size.w.ref)
            if shape.size.h.pct is not None and shape.size.h.ref is not None:
                graph[shape_id].add(shape.size.h.ref)

    for constraint in expanded.constraints:
        if isinstance(constraint, SpacedBy | HorizontalAlign | VerticalAlign):
            refs = {a.shape_id for a in constraint.anchors}
            for ref in refs:
                graph.setdefault(ref, set()).update(refs - {ref})

    return graph

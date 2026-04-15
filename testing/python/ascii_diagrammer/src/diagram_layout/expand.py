"""Expansion and semantic validation.

This stage flattens the nested input tree, creates parent/child mappings,
injects automatic containment constraints for nested children, resolves
implicit parent references, and validates reference legality.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
from typing import Iterable

from .errors import ValidationError
from .schema import (
    Add,
    AlignWith,
    AnchorRef,
    AxisValue,
    BBoxAnchor,
    Conjunction,
    DiagramInput,
    HorizontalAlign,
    InsideOf,
    MultiShapeConstraint,
    PercentOfRefPos,
    PercentOfRefSize,
    PointAccessor,
    PositionExpr,
    RelativePlacement,
    Scale,
    ShapeDefinition,
    ShapeType,
    SpacedBy,
    Sub,
    VecOffset,
    VerticalAlign,
)


CANVAS_ID = "__canvas__"


@dataclass(slots=True)
class ExpandedDiagram:
    """Flattened and validated diagram representation."""

    canvas_width: float
    canvas_height: float
    shapes: dict[str, ShapeDefinition]
    parent_of: dict[str, str]
    children_of: dict[str, list[str]]
    constraints: list[MultiShapeConstraint]


def _resolve_axis_value(axis: AxisValue, parent_id: str) -> AxisValue:
    """Resolve implicit parent references in axis values."""
    count = sum(v is not None for v in (axis.fixed, axis.ref, axis.pct))
    if count != 1:
        raise ValidationError(
            "AxisValue must specify exactly one of fixed, ref, pct semantics "
            "(fixed alone or pct with optional ref)."
        )
    if axis.pct is not None and axis.ref is None:
        return axis.model_copy(update={"ref": parent_id})
    return axis


def _resolve_size_expr(shape: ShapeDefinition, parent_id: str) -> ShapeDefinition:
    """Resolve implicit parent references in size expressions."""
    size = shape.size
    if isinstance(size, PercentOfRefSize):
        w = _resolve_axis_value(size.w, parent_id)
        h = _resolve_axis_value(size.h, parent_id)
        size = size.model_copy(update={"w": w, "h": h})
    return shape.model_copy(update={"size": size})


def _resolve_position_expr(expr: PositionExpr, parent_id: str) -> PositionExpr:
    """Recursively resolve implicit parent references in position expressions."""
    if isinstance(expr, PercentOfRefPos):
        if expr.ref is None:
            return expr.model_copy(update={"ref": parent_id})
        return expr
    if isinstance(expr, VecOffset):
        return expr.model_copy(update={"base": _resolve_position_expr(expr.base, parent_id)})
    if isinstance(expr, Add):
        return expr.model_copy(
            update={
                "left": _resolve_position_expr(expr.left, parent_id),
                "right": _resolve_position_expr(expr.right, parent_id),
            }
        )
    if isinstance(expr, Sub):
        return expr.model_copy(
            update={
                "left": _resolve_position_expr(expr.left, parent_id),
                "right": _resolve_position_expr(expr.right, parent_id),
            }
        )
    if isinstance(expr, Scale):
        return expr.model_copy(update={"expr": _resolve_position_expr(expr.expr, parent_id)})
    if isinstance(expr, Conjunction):
        return expr.model_copy(
            update={"exprs": [_resolve_position_expr(sub, parent_id) for sub in expr.exprs]}
        )
    return expr


def _inject_inside_of(position: PositionExpr, parent_id: str) -> PositionExpr:
    """Inject containment for nested children as a conjunction."""
    inside = InsideOf(target=parent_id)
    if isinstance(position, Conjunction):
        return position.model_copy(update={"exprs": [inside, *position.exprs]})
    return Conjunction(exprs=[inside, position])


def _collect_refs_from_position(expr: PositionExpr) -> set[str]:
    """Collect referenced shape ids from a position expression tree."""
    refs: set[str] = set()
    if isinstance(expr, PercentOfRefPos):
        if expr.ref is not None:
            refs.add(expr.ref)
    elif isinstance(expr, RelativePlacement):
        refs.add(expr.target.shape_id)
    elif isinstance(expr, InsideOf):
        refs.add(expr.target)
    elif isinstance(expr, AlignWith):
        refs.update(a.shape_id for a in expr.anchors)
    elif isinstance(expr, VecOffset):
        refs.update(_collect_refs_from_position(expr.base))
    elif isinstance(expr, Add | Sub):
        refs.update(_collect_refs_from_position(expr.left))
        refs.update(_collect_refs_from_position(expr.right))
    elif isinstance(expr, Scale):
        refs.update(_collect_refs_from_position(expr.expr))
    elif isinstance(expr, Conjunction):
        for sub in expr.exprs:
            refs.update(_collect_refs_from_position(sub))
    return refs


def _collect_refs_from_size(shape: ShapeDefinition) -> set[str]:
    """Collect referenced shape ids from a size expression tree."""
    refs: set[str] = set()
    size = shape.size
    if isinstance(size, PercentOfRefSize):
        if size.w.pct is not None and size.w.ref is not None:
            refs.add(size.w.ref)
        if size.h.pct is not None and size.h.ref is not None:
            refs.add(size.h.ref)
    return refs


def _collect_refs_from_multi(constraint: MultiShapeConstraint) -> set[str]:
    """Collect referenced shape ids from multi-shape constraints."""
    if isinstance(constraint, SpacedBy | HorizontalAlign | VerticalAlign):
        return {a.shape_id for a in constraint.anchors}
    return set()


def _iter_descendants(children_of: dict[str, list[str]], shape_id: str) -> Iterable[str]:
    """Yield all descendants of a shape using the flattened child mapping."""
    for child in children_of.get(shape_id, []):
        yield child
        yield from _iter_descendants(children_of, child)


def _ancestor_chain(parent_of: dict[str, str], shape_id: str) -> list[str]:
    """Return ancestors from immediate parent upward."""
    chain: list[str] = []
    cur = shape_id
    while cur in parent_of:
        cur = parent_of[cur]
        chain.append(cur)
        if cur == CANVAS_ID:
            break
    return chain


def _validate_scope(
    owner_id: str,
    ref_id: str,
    parent_of: dict[str, str],
    children_of: dict[str, list[str]],
) -> None:
    """Validate the allowed reference scope.

    Supported references:
    - self
    - parent
    - siblings (same parent)
    - ancestors
    - descendants are disallowed
    - unrelated nodes in another branch are disallowed
    """
    if ref_id == owner_id:
        return

    descendants = set(_iter_descendants(children_of, owner_id))
    if ref_id in descendants:
        raise ValidationError(
            f"Shape '{owner_id}' may not reference its descendant '{ref_id}'."
        )

    owner_parent = parent_of.get(owner_id)
    ref_parent = parent_of.get(ref_id)

    if ref_id == owner_parent:
        return

    if ref_parent == owner_parent:
        return

    if ref_id in _ancestor_chain(parent_of, owner_id):
        return

    raise ValidationError(
        f"Shape '{owner_id}' may not reference out-of-scope shape '{ref_id}'."
    )


def _validate_anchor_ref(anchor: AnchorRef, known_ids: set[str]) -> None:
    """Validate that an anchor target exists."""
    if anchor.shape_id not in known_ids:
        raise ValidationError(f"Unknown shape reference '{anchor.shape_id}'.")


def _validate_position_refs(
    owner_id: str,
    expr: PositionExpr,
    known_ids: set[str],
    parent_of: dict[str, str],
    children_of: dict[str, list[str]],
) -> None:
    """Validate shape references inside a position expression."""
    if isinstance(expr, PercentOfRefPos):
        assert expr.ref is not None
        if expr.ref not in known_ids:
            raise ValidationError(f"Unknown shape reference '{expr.ref}'.")
        _validate_scope(owner_id, expr.ref, parent_of, children_of)
    elif isinstance(expr, RelativePlacement):
        _validate_anchor_ref(expr.target, known_ids)
        _validate_scope(owner_id, expr.target.shape_id, parent_of, children_of)
    elif isinstance(expr, InsideOf):
        if expr.target not in known_ids:
            raise ValidationError(f"Unknown shape reference '{expr.target}'.")
        _validate_scope(owner_id, expr.target, parent_of, children_of)
    elif isinstance(expr, AlignWith):
        for anchor in expr.anchors:
            _validate_anchor_ref(anchor, known_ids)
            _validate_scope(owner_id, anchor.shape_id, parent_of, children_of)
    elif isinstance(expr, VecOffset):
        _validate_position_refs(owner_id, expr.base, known_ids, parent_of, children_of)
    elif isinstance(expr, Add | Sub):
        _validate_position_refs(owner_id, expr.left, known_ids, parent_of, children_of)
        _validate_position_refs(owner_id, expr.right, known_ids, parent_of, children_of)
    elif isinstance(expr, Scale):
        _validate_position_refs(owner_id, expr.expr, known_ids, parent_of, children_of)
    elif isinstance(expr, Conjunction):
        for sub in expr.exprs:
            _validate_position_refs(owner_id, sub, known_ids, parent_of, children_of)


def _validate_shape_semantics(shape: ShapeDefinition) -> None:
    """Validate shape-local semantic invariants."""
    if shape.children and shape.shape_type is not ShapeType.GROUP:
        raise ValidationError(f"Only GROUP shapes may have children: '{shape.id}'.")
    if shape.shape_type is ShapeType.LINE:
        if shape.line is None or len(shape.line.points) < 2:
            raise ValidationError(f"LINE shape '{shape.id}' must define at least two points.")
    else:
        if shape.line is not None:
            raise ValidationError(f"Non-LINE shape '{shape.id}' must not define line data.")


def expand_diagram(diagram: DiagramInput) -> ExpandedDiagram:
    """Expand nested diagram input into a validated flattened representation."""
    shapes: dict[str, ShapeDefinition] = {}
    parent_of: dict[str, str] = {}
    children_of: dict[str, list[str]] = {CANVAS_ID: []}

    def visit(shape: ShapeDefinition, parent_id: str) -> None:
        if shape.id in shapes or shape.id == CANVAS_ID:
            raise ValidationError(f"Duplicate shape id '{shape.id}'.")

        _validate_shape_semantics(shape)

        resolved = _resolve_size_expr(shape, parent_id)
        resolved_pos = _resolve_position_expr(resolved.position, parent_id)
        if parent_id != CANVAS_ID:
            resolved_pos = _inject_inside_of(resolved_pos, parent_id)
        resolved = resolved.model_copy(update={"position": resolved_pos, "children": []})

        shapes[shape.id] = resolved
        parent_of[shape.id] = parent_id
        children_of.setdefault(parent_id, []).append(shape.id)
        children_of.setdefault(shape.id, [])

        for child in shape.children:
            visit(child, shape.id)

    for top in diagram.shapes:
        visit(top, CANVAS_ID)

    known_ids = set(shapes) | {CANVAS_ID}

    for shape_id, shape in shapes.items():
        _validate_position_refs(shape_id, shape.position, known_ids, parent_of, children_of)

        for ref_id in _collect_refs_from_size(shape):
            if ref_id not in known_ids:
                raise ValidationError(f"Unknown shape reference '{ref_id}'.")
            _validate_scope(shape_id, ref_id, parent_of, children_of)

    for constraint in diagram.constraints:
        for ref_id in _collect_refs_from_multi(constraint):
            if ref_id not in known_ids:
                raise ValidationError(f"Unknown shape reference '{ref_id}'.")

    return ExpandedDiagram(
        canvas_width=diagram.canvas_width,
        canvas_height=diagram.canvas_height,
        shapes=shapes,
        parent_of=parent_of,
        children_of=children_of,
        constraints=diagram.constraints,
    )

"""Constraint compilation and solver execution."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

import kiwisolver as kiwi

from .errors import ResolutionError, ValidationError
from .expand import CANVAS_ID, ExpandedDiagram
from .schema import (
    AbsolutePos,
    Add,
    AlignAxis,
    AlignWith,
    AnchorRef,
    BBoxAnchor,
    Conjunction,
    HorizontalAlign,
    InsideOf,
    PercentOfRefPos,
    PercentOfRefSize,
    PointAccessor,
    PointAdd,
    PointExpr,
    PointLiteral,
    PointScale,
    PointSub,
    RelativePlacement,
    Scale,
    ShapeDefinition,
    SpatialRelation,
    SpacedBy,
    Sub,
    VecLiteral,
    VecOffset,
    VerticalAlign,
)
from .types import ResolvedDiagram, ResolvedPoint, ResolvedShape


REQUIRED = kiwi.strength.required
WEAK = kiwi.strength.weak


@dataclass(slots=True)
class ShapeVars:
    """All kiwi variables for a shape's solved geometry."""

    x: kiwi.Variable
    y: kiwi.Variable
    w: kiwi.Variable
    h: kiwi.Variable
    right: kiwi.Variable
    bottom: kiwi.Variable
    center_x: kiwi.Variable
    center_y: kiwi.Variable


@dataclass(slots=True)
class AxisExpr:
    """Represents an optional expression for x and/or y coordinates.

    Position-expression compilation produces coordinate expressions to bind
    the current shape's origin. Some expressions constrain one axis only.
    """

    x: Optional[kiwi.Expression] = None
    y: Optional[kiwi.Expression] = None


class ConstraintBuilder:
    """Compiles expanded diagram expressions into kiwi constraints."""

    def __init__(self, expanded: ExpandedDiagram) -> None:
        self.expanded = expanded
        self.solver = kiwi.Solver()
        self.vars: dict[str, ShapeVars] = {}
        self.axis_constrained: dict[str, set[str]] = {shape_id: set() for shape_id in expanded.shapes}

    def build(self) -> None:
        """Allocate variables and emit all constraints."""
        self._allocate_canvas()
        self._allocate_shapes()
        self._compile_sizes()
        self._compile_positions()
        self._compile_multi_constraints()
        self._add_weak_defaults()

    def _make_shape_vars(self, prefix: str) -> ShapeVars:
        """Create a full variable set for a shape."""
        return ShapeVars(
            x=kiwi.Variable(f"{prefix}.x"),
            y=kiwi.Variable(f"{prefix}.y"),
            w=kiwi.Variable(f"{prefix}.w"),
            h=kiwi.Variable(f"{prefix}.h"),
            right=kiwi.Variable(f"{prefix}.right"),
            bottom=kiwi.Variable(f"{prefix}.bottom"),
            center_x=kiwi.Variable(f"{prefix}.center_x"),
            center_y=kiwi.Variable(f"{prefix}.center_y"),
        )

    def _add(self, constraint) -> None:
        """Add a kiwi constraint to the solver."""
        self.solver.addConstraint(constraint)

    def _allocate_canvas(self) -> None:
        """Create a fixed implicit canvas parent rectangle."""
        sv = self._make_shape_vars(CANVAS_ID)
        self.vars[CANVAS_ID] = sv

        self._add((sv.x == 0) | REQUIRED)
        self._add((sv.y == 0) | REQUIRED)
        self._add((sv.w == self.expanded.canvas_width) | REQUIRED)
        self._add((sv.h == self.expanded.canvas_height) | REQUIRED)
        self._emit_structural_constraints(sv)

    def _allocate_shapes(self) -> None:
        """Allocate variables and structural constraints for all user shapes."""
        for shape_id in self.expanded.shapes:
            sv = self._make_shape_vars(shape_id)
            self.vars[shape_id] = sv
            self._emit_structural_constraints(sv)

    def _emit_structural_constraints(self, sv: ShapeVars) -> None:
        """Emit invariant bbox relationships."""
        self._add((sv.right == sv.x + sv.w) | REQUIRED)
        self._add((sv.bottom == sv.y + sv.h) | REQUIRED)
        self._add((sv.center_x == sv.x + sv.w / 2.0) | REQUIRED)
        self._add((sv.center_y == sv.y + sv.h / 2.0) | REQUIRED)
        self._add((sv.w >= 0) | REQUIRED)
        self._add((sv.h >= 0) | REQUIRED)

    def _compile_sizes(self) -> None:
        """Compile all size expressions."""
        for shape_id, shape in self.expanded.shapes.items():
            sv = self.vars[shape_id]
            size = shape.size

            if size.type == "fixed":
                self._compile_axis_value_fixed(sv.w, size.w, "width", shape_id)
                self._compile_axis_value_fixed(sv.h, size.h, "height", shape_id)
            elif isinstance(size, PercentOfRefSize):
                self._compile_axis_size(sv.w, size.w, "w", shape_id)
                self._compile_axis_size(sv.h, size.h, "h", shape_id)
            else:
                raise ValidationError(f"Unsupported size expression for '{shape_id}'.")

    def _compile_axis_value_fixed(
        self, var: kiwi.Variable, axis_value, axis_name: str, shape_id: str
    ) -> None:
        """Compile a fixed-size axis."""
        if axis_value.fixed is None or axis_value.ref is not None or axis_value.pct is not None:
            raise ValidationError(f"Fixed size for '{shape_id}' {axis_name} must use fixed only.")
        self._add((var == axis_value.fixed) | REQUIRED)

    def _compile_axis_size(self, var: kiwi.Variable, axis_value, dim: str, shape_id: str) -> None:
        """Compile one size axis from fixed or percentage-of-reference."""
        if axis_value.fixed is not None:
            self._add((var == axis_value.fixed) | REQUIRED)
            return
        if axis_value.pct is not None:
            if axis_value.ref is None:
                raise ValidationError(f"Percentage size axis for '{shape_id}' must have a ref.")
            ref = self.vars[axis_value.ref]
            source = ref.w if dim == "w" else ref.h
            self._add((var == source * (axis_value.pct / 100.0)) | REQUIRED)
            return
        raise ValidationError(f"Size axis for '{shape_id}' is incomplete.")

    def _compile_positions(self) -> None:
        """Compile all shape position expressions."""
        for shape_id, shape in self.expanded.shapes.items():
            compiled = self._compile_position_expr(shape_id, shape.position)
            sv = self.vars[shape_id]
            if compiled.x is not None:
                self._add((sv.x == compiled.x) | REQUIRED)
                self.axis_constrained[shape_id].add("x")
            if compiled.y is not None:
                self._add((sv.y == compiled.y) | REQUIRED)
                self.axis_constrained[shape_id].add("y")

    def _compile_position_expr(self, shape_id: str, expr) -> AxisExpr:
        """Compile a position expression recursively into coordinate expressions.

        Some expressions emit direct constraints and return no axis expression.
        Others synthesize x and/or y expressions to attach to the current shape origin.
        """
        if isinstance(expr, AbsolutePos):
            return AxisExpr(x=kiwi.Expression(expr.x), y=kiwi.Expression(expr.y))

        if isinstance(expr, VecLiteral):
            return AxisExpr(x=kiwi.Expression(expr.x), y=kiwi.Expression(expr.y))

        if isinstance(expr, VecOffset):
            base = self._compile_position_expr(shape_id, expr.base)
            return AxisExpr(
                x=(base.x + expr.offset.x) if base.x is not None else None,
                y=(base.y + expr.offset.y) if base.y is not None else None,
            )

        if isinstance(expr, PercentOfRefPos):
            ref = self.vars[expr.ref]
            return AxisExpr(
                x=(ref.x + ref.w * (expr.x_pct / 100.0)) if expr.x_pct is not None else None,
                y=(ref.y + ref.h * (expr.y_pct / 100.0)) if expr.y_pct is not None else None,
            )

        if isinstance(expr, RelativePlacement):
            return self._compile_relative(shape_id, expr)

        if isinstance(expr, InsideOf):
            target = self.vars[expr.target]
            sv = self.vars[shape_id]
            self._add((sv.x >= target.x) | REQUIRED)
            self._add((sv.right <= target.right) | REQUIRED)
            self._add((sv.y >= target.y) | REQUIRED)
            self._add((sv.bottom <= target.bottom) | REQUIRED)
            return AxisExpr()

        if isinstance(expr, AlignWith):
            self._compile_align_with(expr)
            return AxisExpr()

        if isinstance(expr, Add):
            left = self._compile_position_expr(shape_id, expr.left)
            right = self._compile_position_expr(shape_id, expr.right)
            return AxisExpr(
                x=(left.x + right.x) if left.x is not None and right.x is not None else left.x or right.x,
                y=(left.y + right.y) if left.y is not None and right.y is not None else left.y or right.y,
            )

        if isinstance(expr, Sub):
            left = self._compile_position_expr(shape_id, expr.left)
            right = self._compile_position_expr(shape_id, expr.right)
            return AxisExpr(
                x=(left.x - right.x) if left.x is not None and right.x is not None else left.x,
                y=(left.y - right.y) if left.y is not None and right.y is not None else left.y,
            )

        if isinstance(expr, Scale):
            inner = self._compile_position_expr(shape_id, expr.expr)
            return AxisExpr(
                x=(inner.x * expr.factor) if inner.x is not None else None,
                y=(inner.y * expr.factor) if inner.y is not None else None,
            )

        if isinstance(expr, Conjunction):
            merged = AxisExpr()
            for sub in expr.exprs:
                part = self._compile_position_expr(shape_id, sub)
                if part.x is not None:
                    merged.x = part.x if merged.x is None else merged.x
                if part.y is not None:
                    merged.y = part.y if merged.y is None else merged.y
            return merged

        raise ValidationError(f"Unsupported position expression for '{shape_id}'.")

    def _anchor_component(self, anchor: AnchorRef, axis: str) -> kiwi.Expression:
        """Resolve a single anchor coordinate component.

        axis:
            "x" => return x-coordinate
            "y" => return y-coordinate
        """
        sv = self.vars[anchor.shape_id]
        a = anchor.anchor

        if isinstance(a, BBoxAnchor):
            if a is BBoxAnchor.LEFT:
                if axis != "x":
                    raise ValidationError("bbox-left does not provide Y coordinate.")
                return sv.x
            if a is BBoxAnchor.RIGHT:
                if axis != "x":
                    raise ValidationError("bbox-right does not provide Y coordinate.")
                return sv.right
            if a is BBoxAnchor.TOP:
                if axis != "y":
                    raise ValidationError("bbox-top does not provide X coordinate.")
                return sv.y
            if a is BBoxAnchor.BOTTOM:
                if axis != "y":
                    raise ValidationError("bbox-bottom does not provide X coordinate.")
                return sv.bottom
            if a is BBoxAnchor.CENTER_X:
                if axis != "x":
                    raise ValidationError("bbox-center-x does not provide Y coordinate.")
                return sv.center_x
            if a is BBoxAnchor.CENTER_Y:
                if axis != "y":
                    raise ValidationError("bbox-center-y does not provide X coordinate.")
                return sv.center_y
            if a is BBoxAnchor.CENTER:
                return sv.center_x if axis == "x" else sv.center_y

        if isinstance(a, PointAccessor):
            shape = self.expanded.shapes.get(anchor.shape_id)
            if shape is None or shape.shape_type.value != "line":
                raise ValidationError(
                    f"Point anchor '{a.value}' requires LINE target '{anchor.shape_id}'."
                )
            if a is PointAccessor.START_POINT:
                return sv.x if axis == "x" else sv.y
            if a is PointAccessor.END_POINT:
                point = self._eval_point_expr(shape.line.points[-1])
                return (sv.x + point.x) if axis == "x" else (sv.y + point.y)

        raise ValidationError(f"Unsupported anchor {anchor} for axis {axis}.")

    def _anchor_default_for_relation(self, anchor: AnchorRef, relation: SpatialRelation) -> kiwi.Expression:
        """Resolve anchor coordinates for relative placement semantics."""
        a = anchor.anchor
        if relation in (SpatialRelation.LEFT_OF, SpatialRelation.RIGHT_OF):
            if isinstance(a, BBoxAnchor):
                if a is BBoxAnchor.LEFT:
                    return self._anchor_component(anchor, "x")
                if a is BBoxAnchor.RIGHT:
                    return self._anchor_component(anchor, "x")
                if a is BBoxAnchor.CENTER_X:
                    return self._anchor_component(anchor, "x")
                if a is BBoxAnchor.CENTER:
                    return self._anchor_component(anchor, "x")
                raise ValidationError(
                    f"Anchor '{a.value}' is not compatible with horizontal relation '{relation.value}'."
                )
            return self._anchor_component(anchor, "x")

        if relation in (SpatialRelation.ABOVE, SpatialRelation.BELOW):
            if isinstance(a, BBoxAnchor):
                if a is BBoxAnchor.TOP:
                    return self._anchor_component(anchor, "y")
                if a is BBoxAnchor.BOTTOM:
                    return self._anchor_component(anchor, "y")
                if a is BBoxAnchor.CENTER_Y:
                    return self._anchor_component(anchor, "y")
                if a is BBoxAnchor.CENTER:
                    return self._anchor_component(anchor, "y")
                raise ValidationError(
                    f"Anchor '{a.value}' is not compatible with vertical relation '{relation.value}'."
                )
            return self._anchor_component(anchor, "y")

        raise ValidationError(f"Unsupported relation '{relation.value}'.")

    def _compile_relative(self, shape_id: str, expr: RelativePlacement) -> AxisExpr:
        """Compile a relative placement expression."""
        target = self._anchor_default_for_relation(expr.target, expr.relation)

        if expr.relation is SpatialRelation.LEFT_OF:
            sv = self.vars[shape_id]
            self._add((sv.right + expr.gap == target) | REQUIRED)
            self.axis_constrained[shape_id].add("x")
            return AxisExpr()

        if expr.relation is SpatialRelation.RIGHT_OF:
            return AxisExpr(x=target + expr.gap)

        if expr.relation is SpatialRelation.ABOVE:
            sv = self.vars[shape_id]
            self._add((sv.bottom + expr.gap == target) | REQUIRED)
            self.axis_constrained[shape_id].add("y")
            return AxisExpr()

        if expr.relation is SpatialRelation.BELOW:
            return AxisExpr(y=target + expr.gap)

        raise ValidationError(f"Unsupported relation '{expr.relation.value}'.")

    def _compile_align_with(self, expr: AlignWith) -> None:
        """Compile pairwise anchor equality along the requested axis."""
        if len(expr.anchors) < 2:
            return
        axis = "x" if expr.axis is AlignAxis.X else "y"
        coords = [self._anchor_component(anchor, axis) for anchor in expr.anchors]
        first = coords[0]
        for coord in coords[1:]:
            self._add((coord == first) | REQUIRED)

    def _compile_multi_constraints(self) -> None:
        """Compile diagram-level multi-shape constraints."""
        for idx, constraint in enumerate(self.expanded.constraints):
            if isinstance(constraint, SpacedBy):
                self._compile_spaced_by(constraint)
            elif isinstance(constraint, HorizontalAlign):
                self._compile_horizontal_align(constraint, idx)
            elif isinstance(constraint, VerticalAlign):
                self._compile_vertical_align(constraint, idx)
            else:
                raise ValidationError(f"Unsupported multi-shape constraint '{constraint}'.")

    def _compile_spaced_by(self, constraint: SpacedBy) -> None:
        """Compile fixed step spacing between consecutive anchors."""
        for left, right in zip(constraint.anchors, constraint.anchors[1:]):
            lx = self._anchor_component(left, "x")
            ly = self._anchor_component(left, "y")
            rx = self._anchor_component(right, "x")
            ry = self._anchor_component(right, "y")
            self._add((rx == lx + constraint.spacing.x) | REQUIRED)
            self._add((ry == ly + constraint.spacing.y) | REQUIRED)

    def _compile_horizontal_align(self, constraint: HorizontalAlign, idx: int) -> None:
        """Compile alignment to a common horizontal line."""
        ys = [self._anchor_component(anchor, "y") for anchor in constraint.anchors]
        if not ys:
            return
        if constraint.max_offset == 0:
            first = ys[0]
            for y in ys[1:]:
                self._add((y == first) | REQUIRED)
            return

        line_y = kiwi.Variable(f"multi.horizontal_align[{idx}].line_y")
        for y in ys:
            self._add((y >= line_y - constraint.max_offset) | REQUIRED)
            self._add((y <= line_y + constraint.max_offset) | REQUIRED)
            self._add((y == line_y) | WEAK)

    def _compile_vertical_align(self, constraint: VerticalAlign, idx: int) -> None:
        """Compile alignment to a common vertical line."""
        xs = [self._anchor_component(anchor, "x") for anchor in constraint.anchors]
        if not xs:
            return
        if constraint.max_offset == 0:
            first = xs[0]
            for x in xs[1:]:
                self._add((x == first) | REQUIRED)
            return

        line_x = kiwi.Variable(f"multi.vertical_align[{idx}].line_x")
        for x in xs:
            self._add((x >= line_x - constraint.max_offset) | REQUIRED)
            self._add((x <= line_x + constraint.max_offset) | REQUIRED)
            self._add((x == line_x) | WEAK)

    def _add_weak_defaults(self) -> None:
        """Add weak defaults for unconstrained position axes."""
        for shape_id in self.expanded.shapes:
            sv = self.vars[shape_id]
            parent_id = self.expanded.parent_of[shape_id]
            parent = self.vars[parent_id]

            if "x" not in self.axis_constrained[shape_id]:
                target = 0 if parent_id == CANVAS_ID else parent.x
                self._add((sv.x == target) | WEAK)

            if "y" not in self.axis_constrained[shape_id]:
                target = 0 if parent_id == CANVAS_ID else parent.y
                self._add((sv.y == target) | WEAK)

    def _eval_point_expr(self, expr: PointExpr):
        """Evaluate a line-local point expression numerically.

        Line point expressions are purely local arithmetic and do not depend on
        solver variables, so they are evaluated after parsing rather than
        compiled into solver constraints.
        """
        if isinstance(expr, PointLiteral):
            return expr
        if isinstance(expr, PointAdd):
            left = self._eval_point_expr(expr.left)
            right = self._eval_point_expr(expr.right)
            return PointLiteral(x=left.x + right.x, y=left.y + right.y)
        if isinstance(expr, PointSub):
            left = self._eval_point_expr(expr.left)
            right = self._eval_point_expr(expr.right)
            return PointLiteral(x=left.x - right.x, y=left.y - right.y)
        if isinstance(expr, PointScale):
            inner = self._eval_point_expr(expr.expr)
            return PointLiteral(x=inner.x * expr.factor, y=inner.y * expr.factor)
        raise ValidationError(f"Unsupported point expression '{expr}'.")

    def solve(self) -> ResolvedDiagram:
        """Solve the full system and produce resolved absolute geometry."""
        try:
            self.solver.updateVariables()
        except Exception as exc:  # pragma: no cover - safety net around solver backends
            raise ResolutionError(str(exc)) from exc

        shapes: list[ResolvedShape] = []
        for shape_id, shape in self.expanded.shapes.items():
            sv = self.vars[shape_id]
            points = None
            if shape.shape_type.value == "line":
                points = []
                for point_expr in shape.line.points:
                    local = self._eval_point_expr(point_expr)
                    points.append(
                        ResolvedPoint(
                            x=sv.x.value() + local.x,
                            y=sv.y.value() + local.y,
                        )
                    )

            shapes.append(
                ResolvedShape(
                    id=shape_id,
                    shape_type=shape.shape_type.value,
                    x=sv.x.value(),
                    y=sv.y.value(),
                    w=sv.w.value(),
                    h=sv.h.value(),
                    text=shape.text,
                    points=points,
                )
            )

        return ResolvedDiagram(
            canvas_width=self.expanded.canvas_width,
            canvas_height=self.expanded.canvas_height,
            shapes=shapes,
        )

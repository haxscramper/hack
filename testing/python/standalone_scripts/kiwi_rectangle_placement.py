#!/usr/bin/env python
# /// script
# dependencies = [
#   "kiwisolver",
#   "pytest",
#   "svgwrite",
#   "graphviz",
# ]
# ///

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum, auto
from pathlib import Path
from typing import Dict, Iterable, List, Sequence, Tuple

import graphviz
import kiwisolver as kiwi
import pytest
import svgwrite


class Expr:

    def __init__(self, value) -> None:
        self.value = value

    def to_kiwi(self):
        return self.value

    def __add__(self, other):
        other = ensure_expr(other)
        return Expr(self.value + other.value)

    def __radd__(self, other):
        other = ensure_expr(other)
        return Expr(other.value + self.value)

    def __sub__(self, other):
        other = ensure_expr(other)
        return Expr(self.value - other.value)

    def __rsub__(self, other):
        other = ensure_expr(other)
        return Expr(other.value - self.value)

    def __mul__(self, other):
        if isinstance(other, Expr):
            raise TypeError("Kiwi supports only linear expressions")
        return Expr(self.value * other)

    def __rmul__(self, other):
        if isinstance(other, Expr):
            raise TypeError("Kiwi supports only linear expressions")
        return Expr(other * self.value)

    def __neg__(self):
        return Expr(-self.value)


def ensure_expr(value) -> Expr:
    if isinstance(value, Expr):
        return value
    return Expr(value)


class Axis(Enum):
    X = auto()
    Y = auto()


class Anchor(Enum):
    LEFT = auto()
    HCENTER = auto()
    RIGHT = auto()
    TOP = auto()
    VCENTER = auto()
    BOTTOM = auto()


class RectAttr(Enum):
    X = auto()
    Y = auto()
    WIDTH = auto()
    HEIGHT = auto()
    LEFT = auto()
    HCENTER = auto()
    RIGHT = auto()
    TOP = auto()
    VCENTER = auto()
    BOTTOM = auto()


class Relation(Enum):
    EQ = auto()
    LE = auto()
    GE = auto()


class Strength(Enum):
    REQUIRED = kiwi.strength.required
    STRONG = kiwi.strength.strong
    MEDIUM = kiwi.strength.medium
    WEAK = kiwi.strength.weak

    def kiwi_value(self):
        return self.value


def anchor_axis(anchor: Anchor) -> Axis:
    if anchor in (Anchor.LEFT, Anchor.HCENTER, Anchor.RIGHT):
        return Axis.X
    return Axis.Y


@dataclass
class Rect:
    rect_id: str
    x0: float | None = None
    y0: float | None = None
    width0: float | None = None
    height0: float | None = None
    x: kiwi.Variable = field(init=False)
    y: kiwi.Variable = field(init=False)
    width: kiwi.Variable = field(init=False)
    height: kiwi.Variable = field(init=False)

    def __post_init__(self):
        self.x = kiwi.Variable(f"{self.rect_id}.x")
        self.y = kiwi.Variable(f"{self.rect_id}.y")
        self.width = kiwi.Variable(f"{self.rect_id}.width")
        self.height = kiwi.Variable(f"{self.rect_id}.height")

    def expr(self, name: RectAttr) -> Expr:
        if name == RectAttr.X:
            return Expr(self.x)
        if name == RectAttr.Y:
            return Expr(self.y)
        if name == RectAttr.WIDTH:
            return Expr(self.width)
        if name == RectAttr.HEIGHT:
            return Expr(self.height)
        if name == RectAttr.LEFT:
            return Expr(self.x)
        if name == RectAttr.HCENTER:
            return Expr(self.x + 0.5 * self.width)
        if name == RectAttr.RIGHT:
            return Expr(self.x + self.width)
        if name == RectAttr.TOP:
            return Expr(self.y)
        if name == RectAttr.VCENTER:
            return Expr(self.y + 0.5 * self.height)
        if name == RectAttr.BOTTOM:
            return Expr(self.y + self.height)
        raise ValueError(name)

    def anchor_expr(self, anchor: Anchor) -> Expr:
        if anchor == Anchor.LEFT:
            return self.expr(RectAttr.LEFT)
        if anchor == Anchor.HCENTER:
            return self.expr(RectAttr.HCENTER)
        if anchor == Anchor.RIGHT:
            return self.expr(RectAttr.RIGHT)
        if anchor == Anchor.TOP:
            return self.expr(RectAttr.TOP)
        if anchor == Anchor.VCENTER:
            return self.expr(RectAttr.VCENTER)
        if anchor == Anchor.BOTTOM:
            return self.expr(RectAttr.BOTTOM)
        raise ValueError(anchor)


class ConstraintBase(ABC):

    def __init__(self, strength: Strength = Strength.REQUIRED):
        self.strength = strength

    @abstractmethod
    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        raise NotImplementedError

    @abstractmethod
    def describe_edges(self) -> List[Tuple[str, str, str]]:
        raise NotImplementedError


@dataclass
class AlignItem:
    rect_id: str
    offset: float = 0.0


class AlignConstraint(ConstraintBase):

    def __init__(
        self,
        anchor: Anchor,
        items: Sequence[AlignItem],
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.anchor = anchor
        self.items = list(items)

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        assert len(self.items) >= 2
        base = self.items[0]
        base_expr = rects[base.rect_id].anchor_expr(self.anchor) - base.offset
        result = []
        for item in self.items[1:]:
            expr = rects[item.rect_id].anchor_expr(self.anchor) - item.offset
            result.append((expr.to_kiwi() == base_expr.to_kiwi())
                          | self.strength.kiwi_value())
        return result

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        edges = []
        base = self.items[0].rect_id
        for item in self.items[1:]:
            edges.append((base, item.rect_id, f"align:{self.anchor.name}"))
        return edges


class SeparateConstraint(ConstraintBase):

    def __init__(
        self,
        first_rect_id: str,
        first_anchor: Anchor,
        second_rect_id: str,
        second_anchor: Anchor,
        offset: float,
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.first_rect_id = first_rect_id
        self.first_anchor = first_anchor
        self.second_rect_id = second_rect_id
        self.second_anchor = second_anchor
        self.offset = offset

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        first = rects[self.first_rect_id].anchor_expr(self.first_anchor)
        second = rects[self.second_rect_id].anchor_expr(self.second_anchor)
        c = (first.to_kiwi()
             == (second + self.offset).to_kiwi()) | self.strength.kiwi_value()
        return [c]

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        return [(
            self.second_rect_id,
            self.first_rect_id,
            f"separate:{self.second_anchor.name}->{self.first_anchor.name}+{self.offset}",
        )]


class MultiSeparateConstraint(ConstraintBase):

    def __init__(
        self,
        groups: Sequence[Sequence[str]],
        anchor: Anchor,
        step: float,
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.groups = [list(group) for group in groups]
        self.anchor = anchor
        self.step = step

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        constraints: List[kiwi.Constraint] = []
        for idx in range(len(self.groups) - 1):
            g1 = self.groups[idx]
            g2 = self.groups[idx + 1]
            if not g1 or not g2:
                continue
            if len(g1) != len(g2):
                raise ValueError(
                    "MultiSeparateConstraint requires corresponding group sizes"
                )
            for a, b in zip(g1, g2):
                expr_a = rects[b].anchor_expr(self.anchor)
                expr_b = rects[a].anchor_expr(self.anchor)
                constraints.append((expr_a.to_kiwi() == (expr_b +
                                                         self.step).to_kiwi())
                                   | self.strength.kiwi_value())
        return constraints

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        edges = []
        for idx in range(len(self.groups) - 1):
            for a, b in zip(self.groups[idx], self.groups[idx + 1]):
                edges.append(
                    (a, b, f"multi-separate:{self.anchor.name}+{self.step}"))
        return edges


class ParentWrapConstraint(ConstraintBase):

    def __init__(
        self,
        parent_rect_id: str,
        child_rect_ids: Sequence[str],
        padding_left: float = 0.0,
        padding_top: float = 0.0,
        padding_right: float = 0.0,
        padding_bottom: float = 0.0,
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.parent_rect_id = parent_rect_id
        self.child_rect_ids = list(child_rect_ids)
        self.padding_left = padding_left
        self.padding_top = padding_top
        self.padding_right = padding_right
        self.padding_bottom = padding_bottom

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        if not self.child_rect_ids:
            return []

        parent = rects[self.parent_rect_id]
        constraints: List[kiwi.Constraint] = []

        left_exprs = [
            rects[child_id].anchor_expr(Anchor.LEFT)
            for child_id in self.child_rect_ids
        ]
        top_exprs = [
            rects[child_id].anchor_expr(Anchor.TOP)
            for child_id in self.child_rect_ids
        ]
        right_exprs = [
            rects[child_id].anchor_expr(Anchor.RIGHT)
            for child_id in self.child_rect_ids
        ]
        bottom_exprs = [
            rects[child_id].anchor_expr(Anchor.BOTTOM)
            for child_id in self.child_rect_ids
        ]

        for child_left in left_exprs:
            constraints.append((parent.anchor_expr(Anchor.LEFT).to_kiwi() <= (
                child_left - self.padding_left).to_kiwi())
                               | self.strength.kiwi_value())

        for child_top in top_exprs:
            constraints.append((parent.anchor_expr(Anchor.TOP).to_kiwi() <= (
                child_top - self.padding_top).to_kiwi())
                               | self.strength.kiwi_value())

        for child_right in right_exprs:
            constraints.append((parent.anchor_expr(Anchor.RIGHT).to_kiwi() >= (
                child_right + self.padding_right).to_kiwi())
                               | self.strength.kiwi_value())

        for child_bottom in bottom_exprs:
            constraints.append((parent.anchor_expr(Anchor.BOTTOM).to_kiwi() >=
                                (child_bottom + self.padding_bottom).to_kiwi())
                               | self.strength.kiwi_value())

        constraints.append((parent.anchor_expr(Anchor.LEFT).to_kiwi() == (
            left_exprs[0] - self.padding_left).to_kiwi())
                           | self.strength.kiwi_value())
        constraints.append((parent.anchor_expr(Anchor.TOP).to_kiwi() == (
            top_exprs[0] - self.padding_top).to_kiwi())
                           | self.strength.kiwi_value())
        constraints.append((parent.anchor_expr(Anchor.RIGHT).to_kiwi() == (
            right_exprs[-1] + self.padding_right).to_kiwi())
                           | self.strength.kiwi_value())
        constraints.append((parent.anchor_expr(Anchor.BOTTOM).to_kiwi() == (
            bottom_exprs[-1] + self.padding_bottom).to_kiwi())
                           | self.strength.kiwi_value())

        return constraints

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        return [(child_id, self.parent_rect_id, "wrap-parent")
                for child_id in self.child_rect_ids]


class ChildRelativeToParentConstraint(ConstraintBase):

    def __init__(
        self,
        child_rect_id: str,
        parent_rect_id: str,
        width_factor: float | None = None,
        height_factor: float | None = None,
        x_anchor: Anchor = Anchor.LEFT,
        y_anchor: Anchor = Anchor.TOP,
        x_offset: float = 0.0,
        y_offset: float = 0.0,
        child_x_anchor: Anchor = Anchor.LEFT,
        child_y_anchor: Anchor = Anchor.TOP,
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.child_rect_id = child_rect_id
        self.parent_rect_id = parent_rect_id
        self.width_factor = width_factor
        self.height_factor = height_factor
        self.x_anchor = x_anchor
        self.y_anchor = y_anchor
        self.x_offset = x_offset
        self.y_offset = y_offset
        self.child_x_anchor = child_x_anchor
        self.child_y_anchor = child_y_anchor

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        child = rects[self.child_rect_id]
        parent = rects[self.parent_rect_id]
        constraints: List[kiwi.Constraint] = []

        if self.width_factor is not None:
            constraints.append((child.expr(RectAttr.WIDTH).to_kiwi() == (
                parent.expr(RectAttr.WIDTH) * self.width_factor).to_kiwi())
                               | self.strength.kiwi_value())
        if self.height_factor is not None:
            constraints.append((child.expr(RectAttr.HEIGHT).to_kiwi() == (
                parent.expr(RectAttr.HEIGHT) * self.height_factor).to_kiwi())
                               | self.strength.kiwi_value())

        constraints.append((child.anchor_expr(self.child_x_anchor).to_kiwi() ==
                            (parent.anchor_expr(self.x_anchor) +
                             self.x_offset).to_kiwi())
                           | self.strength.kiwi_value())
        constraints.append((child.anchor_expr(self.child_y_anchor).to_kiwi() ==
                            (parent.anchor_expr(self.y_anchor) +
                             self.y_offset).to_kiwi())
                           | self.strength.kiwi_value())
        return constraints

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        return [(self.parent_rect_id, self.child_rect_id, "child-relative")]


class EvenGapConstraint(ConstraintBase):

    def __init__(
        self,
        rect_ids: Sequence[str],
        axis: Axis,
        anchor: Anchor,
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.rect_ids = list(rect_ids)
        self.axis = axis
        self.anchor = anchor

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        if len(self.rect_ids) < 3:
            return []
        constraints: List[kiwi.Constraint] = []
        for i in range(1, len(self.rect_ids) - 1):
            a = rects[self.rect_ids[i - 1]].anchor_expr(self.anchor)
            b = rects[self.rect_ids[i]].anchor_expr(self.anchor)
            c = rects[self.rect_ids[i + 1]].anchor_expr(self.anchor)
            constraints.append((((b - a) - (c - b)).to_kiwi() == 0)
                               | self.strength.kiwi_value())
        return constraints

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        return [(self.rect_ids[i], self.rect_ids[i + 1],
                 f"even-gap:{self.anchor.name}")
                for i in range(len(self.rect_ids) - 1)]


class EqualSizeConstraint(ConstraintBase):

    def __init__(
        self,
        rect_a_id: str,
        rect_b_id: str,
        match_width: bool = False,
        match_height: bool = False,
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.rect_a_id = rect_a_id
        self.rect_b_id = rect_b_id
        self.match_width = match_width
        self.match_height = match_height

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        a = rects[self.rect_a_id]
        b = rects[self.rect_b_id]
        constraints: List[kiwi.Constraint] = []
        if self.match_width:
            constraints.append((a.expr(RectAttr.WIDTH).to_kiwi() == b.expr(
                RectAttr.WIDTH).to_kiwi())
                               | self.strength.kiwi_value())
        if self.match_height:
            constraints.append((a.expr(RectAttr.HEIGHT).to_kiwi() == b.expr(
                RectAttr.HEIGHT).to_kiwi())
                               | self.strength.kiwi_value())
        return constraints

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        label = []
        if self.match_width:
            label.append("width")
        if self.match_height:
            label.append("height")
        return [(self.rect_a_id, self.rect_b_id,
                 f"equal-size:{'+'.join(label)}")]


class LinearConstraint(ConstraintBase):

    def __init__(
        self,
        left: Expr,
        relation: Relation,
        right: Expr,
        strength: Strength = Strength.REQUIRED,
    ):
        super().__init__(strength)
        self.left = left
        self.relation = relation
        self.right = right

    def build(self, rects: Dict[str, Rect]) -> List[kiwi.Constraint]:
        if self.relation == Relation.EQ:
            c = (self.left.to_kiwi()
                 == self.right.to_kiwi()) | self.strength.kiwi_value()
        elif self.relation == Relation.LE:
            c = (self.left.to_kiwi()
                 <= self.right.to_kiwi()) | self.strength.kiwi_value()
        elif self.relation == Relation.GE:
            c = (self.left.to_kiwi()
                 >= self.right.to_kiwi()) | self.strength.kiwi_value()
        else:
            raise ValueError(self.relation)
        return [c]

    def describe_edges(self) -> List[Tuple[str, str, str]]:
        return []


class Layout:

    def __init__(self, rects: Iterable[Rect],
                 constraints: Iterable[ConstraintBase]):
        self.rects = {rect.rect_id: rect for rect in rects}
        self.constraints = list(constraints)

    def solve(self) -> Dict[str, Dict[str, float]]:
        solver = kiwi.Solver()
        added: List[kiwi.Constraint] = []

        for rect in self.rects.values():
            solver.addConstraint((rect.expr(RectAttr.WIDTH).to_kiwi() >= 0)
                                 | kiwi.strength.required)
            solver.addConstraint((rect.expr(RectAttr.HEIGHT).to_kiwi() >= 0)
                                 | kiwi.strength.required)
            added.extend([])
            if rect.x0 is not None:
                solver.addConstraint((
                    rect.expr(RectAttr.X).to_kiwi() == rect.x0)
                                     | kiwi.strength.required)
            if rect.y0 is not None:
                solver.addConstraint((
                    rect.expr(RectAttr.Y).to_kiwi() == rect.y0)
                                     | kiwi.strength.required)
            if rect.width0 is not None:
                solver.addConstraint((
                    rect.expr(RectAttr.WIDTH).to_kiwi() == rect.width0)
                                     | kiwi.strength.required)
            if rect.height0 is not None:
                solver.addConstraint((
                    rect.expr(RectAttr.HEIGHT).to_kiwi() == rect.height0)
                                     | kiwi.strength.required)

        for item in self.constraints:
            for c in item.build(self.rects):
                solver.addConstraint(c)
                added.append(c)

        solver.updateVariables()

        return {
            rect_id: {
                "x": rect.x.value(),
                "y": rect.y.value(),
                "width": rect.width.value(),
                "height": rect.height.value(),
            }
            for rect_id, rect in self.rects.items()
        }

    def to_svg(self, path: str | Path, title: str = "layout") -> None:
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)

        solved = self.solve()
        max_x = max(v["x"] + v["width"]
                    for v in solved.values()) if solved else 100
        max_y = max(v["y"] + v["height"]
                    for v in solved.values()) if solved else 100

        dwg = svgwrite.Drawing(str(path), size=(max_x + 40, max_y + 40))
        dwg.add(
            dwg.rect(insert=(0, 0),
                     size=(max_x + 40, max_y + 40),
                     fill="white"))
        dwg.add(
            dwg.text(title, insert=(10, 18), fill="black", font_size="14px"))

        colors = [
            "#ffcccc", "#ccffcc", "#ccccff", "#fff0cc", "#f0ccff", "#ccfff7"
        ]
        for idx, (rect_id, g) in enumerate(solved.items()):
            color = colors[idx % len(colors)]
            dwg.add(
                dwg.rect(
                    insert=(g["x"] + 20, g["y"] + 20),
                    size=(g["width"], g["height"]),
                    fill=color,
                    stroke="black",
                ))
            dwg.add(
                dwg.text(
                    rect_id,
                    insert=(g["x"] + 24, g["y"] + 36),
                    fill="black",
                    font_size="12px",
                ))

        dwg.save()

    def to_graphviz(self) -> graphviz.Digraph:
        dot = graphviz.Digraph("layout", format="svg")
        dot.attr(rankdir="LR")

        for rect_id in self.rects:
            dot.node(f"rect:{rect_id}", rect_id, shape="box")

        for idx, constraint in enumerate(self.constraints):
            cid = f"constraint:{idx}"
            dot.node(cid, constraint.__class__.__name__, shape="ellipse")
            for src, dst, label in constraint.describe_edges():
                dot.edge(f"rect:{src}", cid, label=label)
                dot.edge(cid, f"rect:{dst}")

        return dot

    def write_graphviz(self, path: str | Path) -> None:
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        dot = self.to_graphviz()
        dot.render(filename=str(path), cleanup=True)


def assert_close(a: float, b: float, eps: float = 1e-6):
    assert abs(a - b) <= eps


def write_outputs(name: str, layout: Layout):
    out_dir = Path("/tmp/kiwi-place")
    out_dir.mkdir(parents=True, exist_ok=True)
    layout.to_svg(out_dir / f"{name}.svg", title=name)
    layout.write_graphviz(out_dir / f"{name}-graph")


def test_alignment_and_relative_positioning():
    rects = [
        Rect("a", x0=10, y0=20, width0=40, height0=30),
        Rect("b", width0=40, height0=30),
        Rect("c", width0=40, height0=30),
    ]
    constraints = [
        AlignConstraint(
            Anchor.TOP,
            [AlignItem("a"), AlignItem("b"),
             AlignItem("c")]),
        SeparateConstraint("b", Anchor.LEFT, "a", Anchor.RIGHT, 15),
        SeparateConstraint("c", Anchor.LEFT, "b", Anchor.RIGHT, 15),
    ]
    layout = Layout(rects, constraints)
    solved = layout.solve()
    write_outputs("test_alignment_and_relative_positioning", layout)

    assert_close(solved["a"]["y"], solved["b"]["y"])
    assert_close(solved["b"]["y"], solved["c"]["y"])
    assert_close(solved["b"]["x"],
                 solved["a"]["x"] + solved["a"]["width"] + 15)
    assert_close(solved["c"]["x"],
                 solved["b"]["x"] + solved["b"]["width"] + 15)


def test_even_gap_shape_placement():
    rects = [
        Rect("a", x0=10, y0=10, width0=20, height0=20),
        Rect("b", y0=10, width0=20, height0=20),
        Rect("c", x0=110, y0=10, width0=20, height0=20),
    ]
    constraints = [
        EvenGapConstraint(["a", "b", "c"], Axis.X, Anchor.LEFT),
    ]
    layout = Layout(rects, constraints)
    solved = layout.solve()
    write_outputs("test_even_gap_shape_placement", layout)

    assert_close(solved["b"]["x"], 60.0)


def test_parent_and_child_relative_constraints():
    rects = [
        Rect("child1", x0=20, y0=30, width0=30, height0=10),
        Rect("child2", x0=70, y0=60, width0=20, height0=20),
        Rect("parent"),
        Rect("inner"),
    ]
    constraints = [
        ParentWrapConstraint(
            "parent",
            ["child1", "child2"],
            padding_left=5,
            padding_top=10,
            padding_right=15,
            padding_bottom=20,
        ),
        ChildRelativeToParentConstraint(
            "inner",
            "parent",
            width_factor=0.2,
            height_factor=0.5,
            x_anchor=Anchor.HCENTER,
            y_anchor=Anchor.VCENTER,
            x_offset=20,
            y_offset=0,
            child_x_anchor=Anchor.LEFT,
            child_y_anchor=Anchor.TOP,
        ),
    ]
    layout = Layout(rects, constraints)
    solved = layout.solve()
    write_outputs("test_parent_and_child_relative_constraints", layout)

    assert_close(solved["parent"]["x"], 15.0)
    assert_close(solved["parent"]["y"], 20.0)
    assert_close(solved["parent"]["width"], 90.0)
    assert_close(solved["parent"]["height"], 80.0)
    assert_close(solved["inner"]["width"], solved["parent"]["width"] * 0.2)
    assert_close(solved["inner"]["height"], solved["parent"]["height"] * 0.5)
    assert_close(solved["inner"]["x"],
                 solved["parent"]["x"] + solved["parent"]["width"] * 0.5 + 20)
    assert_close(solved["inner"]["y"],
                 solved["parent"]["y"] + solved["parent"]["height"] * 0.5)


def test_linear_constraint_and_equal_size():
    rects = [
        Rect("a", x0=10, y0=10, width0=40, height0=25),
        Rect("b", y0=50),
    ]
    a = rects[0]
    b = rects[1]
    constraints = [
        EqualSizeConstraint("a", "b", match_height=True),
        LinearConstraint(b.expr(RectAttr.X), Relation.EQ,
                         a.expr(RectAttr.RIGHT) + 30),
        LinearConstraint(b.expr(RectAttr.WIDTH), Relation.EQ,
                         a.expr(RectAttr.WIDTH) + 20),
    ]
    layout = Layout(rects, constraints)
    solved = layout.solve()
    write_outputs("test_linear_constraint_and_equal_size", layout)

    assert_close(solved["b"]["x"], 80.0)
    assert_close(solved["b"]["width"], 60.0)
    assert_close(solved["b"]["height"], 25.0)


def test_multi_separate_several_lines_of_evenly_spaced_rectangles():
    rects = [
        Rect("r00", x0=0, y0=0, width0=20, height0=10),
        Rect("r01", y0=0, width0=20, height0=10),
        Rect("r02", x0=80, y0=0, width0=20, height0=10),
        Rect("r10", y0=30, width0=20, height0=10),
        Rect("r11", y0=30, width0=20, height0=10),
        Rect("r12", y0=30, width0=20, height0=10),
        Rect("r20", y0=60, width0=20, height0=10),
        Rect("r21", y0=60, width0=20, height0=10),
        Rect("r22", y0=60, width0=20, height0=10),
    ]
    constraints = [
        EvenGapConstraint(["r00", "r01", "r02"], Axis.X, Anchor.LEFT),
        AlignConstraint(Anchor.LEFT,
                        [AlignItem("r00"),
                         AlignItem("r10"),
                         AlignItem("r20")]),
        AlignConstraint(Anchor.LEFT,
                        [AlignItem("r01"),
                         AlignItem("r11"),
                         AlignItem("r21")]),
        AlignConstraint(Anchor.LEFT,
                        [AlignItem("r02"),
                         AlignItem("r12"),
                         AlignItem("r22")]),
        MultiSeparateConstraint(
            [["r00", "r01", "r02"], ["r10", "r11", "r12"],
             ["r20", "r21", "r22"]],
            Anchor.TOP,
            30,
        ),
    ]
    layout = Layout(rects, constraints)
    solved = layout.solve()
    write_outputs(
        "test_multi_separate_several_lines_of_evenly_spaced_rectangles",
        layout)

    assert_close(solved["r01"]["x"], 40.0)
    assert_close(solved["r10"]["y"], 30.0)
    assert_close(solved["r20"]["y"], 60.0)


def test_two_orthogonal_multi_separate_grid():
    ids = [
        ["g00", "g01", "g02"],
        ["g10", "g11", "g12"],
        ["g20", "g21", "g22"],
    ]
    rects = [Rect("g00", x0=0, y0=0, width0=10, height0=10)]
    for row in ids:
        for rid in row:
            if rid != "g00":
                rects.append(Rect(rid, width0=10, height0=10))

    constraints: List[ConstraintBase] = []
    constraints.append(MultiSeparateConstraint(ids, Anchor.TOP, 25))
    col_groups = [[ids[0][0], ids[1][0], ids[2][0]],
                  [ids[0][1], ids[1][1], ids[2][1]],
                  [ids[0][2], ids[1][2], ids[2][2]]]
    constraints.append(MultiSeparateConstraint(col_groups, Anchor.LEFT, 35))
    for row in ids:
        constraints.append(
            AlignConstraint(Anchor.TOP, [AlignItem(r) for r in row]))
    for col in col_groups:
        constraints.append(
            AlignConstraint(Anchor.LEFT, [AlignItem(r) for r in col]))

    layout = Layout(rects, constraints)
    solved = layout.solve()
    write_outputs("test_two_orthogonal_multi_separate_grid", layout)

    assert_close(solved["g12"]["x"], 70.0)
    assert_close(solved["g12"]["y"], 25.0)
    assert_close(solved["g21"]["x"], 35.0)
    assert_close(solved["g21"]["y"], 50.0)


def test_even_gap_parent_rectangles_and_children_relative():
    rects = [
        Rect("p1", x0=0, y0=0, width0=100, height0=60),
        Rect("p2", y0=0, width0=100, height0=60),
        Rect("p3", x0=300, y0=0, width0=100, height0=60),
        Rect("c1"),
        Rect("c2"),
        Rect("c3"),
    ]
    constraints = [
        EvenGapConstraint(["p1", "p2", "p3"], Axis.X, Anchor.LEFT),
        ChildRelativeToParentConstraint(
            "c1",
            "p1",
            width_factor=0.2,
            height_factor=0.5,
            x_anchor=Anchor.HCENTER,
            y_anchor=Anchor.VCENTER,
            x_offset=10,
            y_offset=0,
            child_x_anchor=Anchor.LEFT,
            child_y_anchor=Anchor.TOP,
        ),
        ChildRelativeToParentConstraint(
            "c2",
            "p2",
            width_factor=0.2,
            height_factor=0.5,
            x_anchor=Anchor.HCENTER,
            y_anchor=Anchor.VCENTER,
            x_offset=10,
            y_offset=0,
            child_x_anchor=Anchor.LEFT,
            child_y_anchor=Anchor.TOP,
        ),
        ChildRelativeToParentConstraint(
            "c3",
            "p3",
            width_factor=0.2,
            height_factor=0.5,
            x_anchor=Anchor.HCENTER,
            y_anchor=Anchor.VCENTER,
            x_offset=10,
            y_offset=0,
            child_x_anchor=Anchor.LEFT,
            child_y_anchor=Anchor.TOP,
        ),
    ]
    layout = Layout(rects, constraints)
    solved = layout.solve()
    write_outputs("test_even_gap_parent_rectangles_and_children_relative",
                  layout)

    assert_close(solved["p2"]["x"], 150.0)
    assert_close(solved["c2"]["width"], 20.0)
    assert_close(solved["c2"]["height"], 30.0)
    assert_close(solved["c2"]["x"], solved["p2"]["x"] + 50 + 10)
    assert_close(solved["c2"]["y"], solved["p2"]["y"] + 30)


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__]))

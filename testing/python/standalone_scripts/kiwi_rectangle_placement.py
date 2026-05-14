#!/usr/bin/env python
# /// script
# dependencies = ["kiwisolver", "pytest"]
# ///

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Iterable

import kiwisolver as kiwi


class Axis(str, Enum):
    X = "x"
    Y = "y"


class Anchor(str, Enum):
    LEFT = "left"
    RIGHT = "right"
    HCENTER = "hcenter"
    TOP = "top"
    BOTTOM = "bottom"
    VCENTER = "vcenter"


@dataclass
class Rectangle:
    id: str
    x: float | None = None
    y: float | None = None
    width: float | None = None
    height: float | None = None
    x_var: kiwi.Variable = field(init=False)
    y_var: kiwi.Variable = field(init=False)
    width_var: kiwi.Variable = field(init=False)
    height_var: kiwi.Variable = field(init=False)

    def __post_init__(self) -> None:
        self.x_var = kiwi.Variable(f"{self.id}.x")
        self.y_var = kiwi.Variable(f"{self.id}.y")
        self.width_var = kiwi.Variable(f"{self.id}.width")
        self.height_var = kiwi.Variable(f"{self.id}.height")

    def anchor_expr(self, anchor: Anchor) -> kiwi.Expression:
        if anchor == Anchor.LEFT:
            return self.x_var
        if anchor == Anchor.RIGHT:
            return self.x_var + self.width_var
        if anchor == Anchor.HCENTER:
            return self.x_var + self.width_var * 0.5
        if anchor == Anchor.TOP:
            return self.y_var
        if anchor == Anchor.BOTTOM:
            return self.y_var + self.height_var
        if anchor == Anchor.VCENTER:
            return self.y_var + self.height_var * 0.5
        raise ValueError(f"Unsupported anchor: {anchor}")

    def intrinsic_constraints(self) -> list[kiwi.Constraint]:
        constraints = [
            self.width_var >= 0,
            self.height_var >= 0,
        ]
        if self.x is not None:
            constraints.append(self.x_var == self.x)
        if self.y is not None:
            constraints.append(self.y_var == self.y)
        if self.width is not None:
            constraints.append(self.width_var == self.width)
        if self.height is not None:
            constraints.append(self.height_var == self.height)
        return constraints

    def solved(self) -> dict[str, float]:
        return {
            "x": self.x_var.value(),
            "y": self.y_var.value(),
            "width": self.width_var.value(),
            "height": self.height_var.value(),
        }


class ConstraintSpec(ABC):

    @abstractmethod
    def to_kiwi(self, rects: dict[str, Rectangle]) -> list[kiwi.Constraint]:
        raise NotImplementedError


@dataclass
class AlignToLine(ConstraintSpec):
    rect_id: str
    anchor: Anchor
    position: float

    def to_kiwi(self, rects: dict[str, Rectangle]) -> list[kiwi.Constraint]:
        rect = rects[self.rect_id]
        return [rect.anchor_expr(self.anchor) == self.position]


@dataclass
class RelativeOffset(ConstraintSpec):
    rect_id: str
    rect_anchor: Anchor
    other_rect_id: str
    other_anchor: Anchor
    offset: float

    def to_kiwi(self, rects: dict[str, Rectangle]) -> list[kiwi.Constraint]:
        rect = rects[self.rect_id]
        other = rects[self.other_rect_id]
        return [
            rect.anchor_expr(
                self.rect_anchor) == other.anchor_expr(self.other_anchor) +
            self.offset
        ]


@dataclass
class EvenlySpacedGroups(ConstraintSpec):
    groups: list[list[str]]
    axis: Axis
    anchor: Anchor
    spacing: float

    def to_kiwi(self, rects: dict[str, Rectangle]) -> list[kiwi.Constraint]:
        constraints: list[kiwi.Constraint] = []
        if len(self.groups) < 2:
            return constraints

        for left_group, right_group in zip(self.groups, self.groups[1:]):
            if len(left_group) != len(right_group):
                raise ValueError("Adjacent groups must have equal length")
            for left_id, right_id in zip(left_group, right_group):
                constraints.extend(
                    RelativeOffset(
                        rect_id=right_id,
                        rect_anchor=self.anchor,
                        other_rect_id=left_id,
                        other_anchor=self.anchor,
                        offset=self.spacing,
                    ).to_kiwi(rects))
        return constraints


def solve_layout(
    rectangles: Iterable[Rectangle],
    constraints: Iterable[ConstraintSpec],
) -> dict[str, dict[str, float]]:
    rects = {rect.id: rect for rect in rectangles}
    solver = kiwi.Solver()

    for rect in rects.values():
        for constraint in rect.intrinsic_constraints():
            solver.addConstraint(constraint)

    for spec in constraints:
        for constraint in spec.to_kiwi(rects):
            solver.addConstraint(constraint)

    solver.updateVariables()
    return {rect_id: rect.solved() for rect_id, rect in rects.items()}


def test_align_centers_and_resolve_missing_position() -> None:
    rects = [
        Rectangle("a", width=40, height=20),
        Rectangle("b", x=10, y=30, width=40, height=20),
    ]
    constraints = [
        AlignToLine("a", Anchor.VCENTER, 100),
        RelativeOffset("a", Anchor.HCENTER, "b", Anchor.HCENTER, 0),
    ]

    solved = solve_layout(rects, constraints)

    assert solved["a"]["y"] == 90
    assert solved["a"]["x"] == 10


def test_anchor_offset_between_rectangles() -> None:
    rects = [
        Rectangle("top", x=0, y=0, width=30, height=10),
        Rectangle("bottom", width=30, height=10),
    ]
    constraints = [
        RelativeOffset("bottom", Anchor.TOP, "top", Anchor.BOTTOM, 25),
        RelativeOffset("bottom", Anchor.LEFT, "top", Anchor.LEFT, 0),
    ]

    solved = solve_layout(rects, constraints)

    assert solved["bottom"]["x"] == 0
    assert solved["bottom"]["y"] == 35


def test_evenly_spaced_groups() -> None:
    rects = [
        Rectangle("g1a", x=0, y=0, width=10, height=10),
        Rectangle("g1b", x=0, y=20, width=10, height=10),
        Rectangle("g2a", y=0, width=10, height=10),
        Rectangle("g2b", y=20, width=10, height=10),
        Rectangle("g3a", y=0, width=10, height=10),
        Rectangle("g3b", y=20, width=10, height=10),
    ]
    constraints = [
        EvenlySpacedGroups(
            groups=[["g1a", "g1b"], ["g2a", "g2b"], ["g3a", "g3b"]],
            axis=Axis.X,
            anchor=Anchor.LEFT,
            spacing=50,
        )
    ]

    solved = solve_layout(rects, constraints)

    assert solved["g2a"]["x"] == 50
    assert solved["g2b"]["x"] == 50
    assert solved["g3a"]["x"] == 100
    assert solved["g3b"]["x"] == 100


def test_align_to_right_and_bottom_lines() -> None:
    rects = [
        Rectangle("r", width=20, height=15),
    ]
    constraints = [
        AlignToLine("r", Anchor.RIGHT, 70),
        AlignToLine("r", Anchor.BOTTOM, 90),
    ]

    solved = solve_layout(rects, constraints)

    assert solved["r"]["x"] == 50
    assert solved["r"]["y"] == 75

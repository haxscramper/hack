# /// script
# dependencies = [
#   "kiwisolver",
#   "pytest",
# ]
# ///
#!/usr/bin/env python

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Iterable

import kiwisolver as kiwi
import pytest


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


STRENGTHS = {
    "required": kiwi.strength.required,
    "strong": kiwi.strength.strong,
    "medium": kiwi.strength.medium,
    "weak": kiwi.strength.weak,
}


@dataclass
class Rect:
    id: str
    x: float | None = None
    y: float | None = None
    width: float | None = None
    height: float | None = None

    def __post_init__(self) -> None:
        self.x_var = kiwi.Variable(f"{self.id}.x")
        self.y_var = kiwi.Variable(f"{self.id}.y")
        self.width_var = kiwi.Variable(f"{self.id}.width")
        self.height_var = kiwi.Variable(f"{self.id}.height")

    def anchor_expr(self, anchor: Anchor):
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
        raise ValueError(anchor)

    def value_dict(self) -> dict[str, float]:
        return {
            "x": self.x_var.value(),
            "y": self.y_var.value(),
            "width": self.width_var.value(),
            "height": self.height_var.value(),
        }


class ConstraintSpec(ABC):

    def __init__(self, strength: str = "required") -> None:
        self.strength = STRENGTHS[strength]

    @abstractmethod
    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        pass

    def _add(self, solver: kiwi.Solver, constraint) -> None:
        solver.addConstraint(constraint | self.strength)


class RectGeometryConstraint(ConstraintSpec):

    def __init__(
        self,
        rect_id: str,
        *,
        x: float | None = None,
        y: float | None = None,
        width: float | None = None,
        height: float | None = None,
        strength: str = "required",
    ) -> None:
        super().__init__(strength)
        self.rect_id = rect_id
        self.x = x
        self.y = y
        self.width = width
        self.height = height

    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        rect = rects[self.rect_id]
        if self.x is not None:
            self._add(solver, rect.x_var == self.x)
        if self.y is not None:
            self._add(solver, rect.y_var == self.y)
        if self.width is not None:
            self._add(solver, rect.width_var == self.width)
        if self.height is not None:
            self._add(solver, rect.height_var == self.height)


class RectMinSizeConstraint(ConstraintSpec):

    def __init__(
        self,
        rect_id: str,
        *,
        min_width: float = 0,
        min_height: float = 0,
        strength: str = "required",
    ) -> None:
        super().__init__(strength)
        self.rect_id = rect_id
        self.min_width = min_width
        self.min_height = min_height

    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        rect = rects[self.rect_id]
        self._add(solver, rect.width_var >= self.min_width)
        self._add(solver, rect.height_var >= self.min_height)


class AlignConstraint(ConstraintSpec):

    def __init__(
        self,
        rect_id: str,
        anchor: Anchor,
        axis: Axis,
        position: float,
        strength: str = "required",
    ) -> None:
        super().__init__(strength)
        self.rect_id = rect_id
        self.anchor = anchor
        self.axis = axis
        self.position = position

    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        rect = rects[self.rect_id]
        expr = rect.anchor_expr(self.anchor)
        self._add(solver, expr == self.position)


class RelativeAnchorConstraint(ConstraintSpec):

    def __init__(
        self,
        rect_id: str,
        rect_anchor: Anchor,
        other_rect_id: str,
        other_anchor: Anchor,
        offset: float,
        strength: str = "required",
    ) -> None:
        super().__init__(strength)
        self.rect_id = rect_id
        self.rect_anchor = rect_anchor
        self.other_rect_id = other_rect_id
        self.other_anchor = other_anchor
        self.offset = offset

    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        rect = rects[self.rect_id]
        other = rects[self.other_rect_id]
        self._add(
            solver,
            rect.anchor_expr(
                self.rect_anchor) == other.anchor_expr(self.other_anchor) +
            self.offset,
        )


class EvenlySpacedSequenceConstraint(ConstraintSpec):

    def __init__(
        self,
        rect_ids: list[str],
        anchor: Anchor,
        offset: float,
        strength: str = "required",
    ) -> None:
        super().__init__(strength)
        self.rect_ids = rect_ids
        self.anchor = anchor
        self.offset = offset

    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        for left, right in zip(self.rect_ids, self.rect_ids[1:]):
            self._add(
                solver,
                rects[right].anchor_expr(
                    self.anchor) == rects[left].anchor_expr(self.anchor) +
                self.offset,
            )


class ParentWrapConstraint(ConstraintSpec):

    def __init__(
        self,
        parent_id: str,
        child_ids: list[str],
        *,
        padding_left: float = 0,
        padding_top: float = 0,
        padding_right: float = 0,
        padding_bottom: float = 0,
        strength: str = "required",
    ) -> None:
        super().__init__(strength)
        self.parent_id = parent_id
        self.child_ids = child_ids
        self.padding_left = padding_left
        self.padding_top = padding_top
        self.padding_right = padding_right
        self.padding_bottom = padding_bottom

    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        parent = rects[self.parent_id]
        children = [rects[child_id] for child_id in self.child_ids]

        for child in children:
            self._add(
                solver,
                child.anchor_expr(Anchor.LEFT)
                >= parent.anchor_expr(Anchor.LEFT) + self.padding_left,
            )
            self._add(
                solver,
                child.anchor_expr(Anchor.TOP)
                >= parent.anchor_expr(Anchor.TOP) + self.padding_top,
            )
            self._add(
                solver,
                child.anchor_expr(Anchor.RIGHT)
                <= parent.anchor_expr(Anchor.RIGHT) - self.padding_right,
            )
            self._add(
                solver,
                child.anchor_expr(Anchor.BOTTOM)
                <= parent.anchor_expr(Anchor.BOTTOM) - self.padding_bottom,
            )

        if children:
            first = children[0]
            self._add(
                solver,
                parent.anchor_expr(
                    Anchor.LEFT) == first.anchor_expr(Anchor.LEFT) -
                self.padding_left,
            )
            self._add(
                solver,
                parent.anchor_expr(
                    Anchor.TOP) == first.anchor_expr(Anchor.TOP) -
                self.padding_top,
            )

            right_expr = first.anchor_expr(Anchor.RIGHT)
            bottom_expr = first.anchor_expr(Anchor.BOTTOM)
            for child in children[1:]:
                self._add(
                    solver,
                    parent.anchor_expr(Anchor.RIGHT)
                    >= child.anchor_expr(Anchor.RIGHT) + self.padding_right,
                )
                self._add(
                    solver,
                    parent.anchor_expr(Anchor.BOTTOM)
                    >= child.anchor_expr(Anchor.BOTTOM) + self.padding_bottom,
                )
                right_expr = child.anchor_expr(Anchor.RIGHT)
                bottom_expr = child.anchor_expr(Anchor.BOTTOM)

            self._add(
                solver,
                parent.anchor_expr(Anchor.RIGHT) == right_expr +
                self.padding_right)
            self._add(
                solver,
                parent.anchor_expr(Anchor.BOTTOM) == bottom_expr +
                self.padding_bottom)


class ChildFromParentConstraint(ConstraintSpec):

    def __init__(
        self,
        rect_id: str,
        parent_id: str,
        *,
        width_ratio: float | None = None,
        height_ratio: float | None = None,
        x_offset: float = 0,
        y_offset: float = 0,
        x_anchor: Anchor = Anchor.LEFT,
        y_anchor: Anchor = Anchor.TOP,
        strength: str = "required",
    ) -> None:
        super().__init__(strength)
        self.rect_id = rect_id
        self.parent_id = parent_id
        self.width_ratio = width_ratio
        self.height_ratio = height_ratio
        self.x_offset = x_offset
        self.y_offset = y_offset
        self.x_anchor = x_anchor
        self.y_anchor = y_anchor

    def add_to_solver(self, solver: kiwi.Solver, rects: dict[str,
                                                             Rect]) -> None:
        rect = rects[self.rect_id]
        parent = rects[self.parent_id]

        if self.width_ratio is not None:
            self._add(solver,
                      rect.width_var == parent.width_var * self.width_ratio)
        if self.height_ratio is not None:
            self._add(solver,
                      rect.height_var == parent.height_var * self.height_ratio)

        self._add(
            solver,
            rect.anchor_expr(
                self.x_anchor) == parent.anchor_expr(self.x_anchor) +
            self.x_offset,
        )
        self._add(
            solver,
            rect.anchor_expr(
                self.y_anchor) == parent.anchor_expr(self.y_anchor) +
            self.y_offset,
        )


class Layout:

    def __init__(self, rects: Iterable[Rect],
                 constraints: Iterable[ConstraintSpec]) -> None:
        self.rects = {rect.id: rect for rect in rects}
        self.constraints = list(constraints)

    def solve(self) -> dict[str, dict[str, float]]:
        solver = kiwi.Solver()

        for rect in self.rects.values():
            solver.addConstraint(rect.width_var >= 0)
            solver.addConstraint(rect.height_var >= 0)

            if rect.x is not None:
                solver.addConstraint(rect.x_var == rect.x)
            if rect.y is not None:
                solver.addConstraint(rect.y_var == rect.y)
            if rect.width is not None:
                solver.addConstraint(rect.width_var == rect.width)
            if rect.height is not None:
                solver.addConstraint(rect.height_var == rect.height)

        for constraint in self.constraints:
            constraint.add_to_solver(solver, self.rects)

        solver.updateVariables()
        return {
            rect_id: rect.value_dict()
            for rect_id, rect in self.rects.items()
        }

    def write_svg(self, path: str | Path) -> None:
        path = Path(path)
        values = {
            rect_id: rect.value_dict()
            for rect_id, rect in self.rects.items()
        }

        min_x = min(v["x"] for v in values.values())
        min_y = min(v["y"] for v in values.values())
        max_x = max(v["x"] + v["width"] for v in values.values())
        max_y = max(v["y"] + v["height"] for v in values.values())

        pad = 20
        width = max_x - min_x + pad * 2
        height = max_y - min_y + pad * 2

        palette = [
            "#d95f02",
            "#1b9e77",
            "#7570b3",
            "#e7298a",
            "#66a61e",
            "#e6ab02",
            "#a6761d",
            "#666666",
        ]

        lines = [
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">',
            '<rect width="100%" height="100%" fill="white"/>',
        ]

        for idx, (rect_id, v) in enumerate(values.items()):
            color = palette[idx % len(palette)]
            x = v["x"] - min_x + pad
            y = v["y"] - min_y + pad
            w = v["width"]
            h = v["height"]
            lines.append(
                f'<rect x="{x}" y="{y}" width="{w}" height="{h}" fill="{color}" fill-opacity="0.2" stroke="{color}" stroke-width="2"/>'
            )
            lines.append(
                f'<text x="{x + 4}" y="{y + 16}" font-family="monospace" font-size="12" fill="{color}">{rect_id}</text>'
            )

        lines.append("</svg>")
        path.write_text("\n".join(lines))


def test_alignment_and_relative_positioning() -> None:
    rects = [
        Rect("a", width=100, height=40),
        Rect("b", width=80, height=30),
    ]
    layout = Layout(
        rects,
        [
            AlignConstraint("a", Anchor.LEFT, Axis.X, 10),
            AlignConstraint("a", Anchor.TOP, Axis.Y, 20),
            RelativeAnchorConstraint("b", Anchor.LEFT, "a", Anchor.RIGHT, 25),
            RelativeAnchorConstraint("b", Anchor.VCENTER, "a", Anchor.VCENTER,
                                     0),
        ],
    )

    values = layout.solve()
    layout.write_svg(
        "/tmp/kiwi-place-test_alignment_and_relative_positioning.svg")

    assert values["a"]["x"] == pytest.approx(10)
    assert values["a"]["y"] == pytest.approx(20)
    assert values["b"]["x"] == pytest.approx(135)
    assert values["b"]["y"] == pytest.approx(25)


def test_even_spacing_of_aligned_columns() -> None:
    rects = [
        Rect("a", width=40, height=20),
        Rect("b", width=40, height=20),
        Rect("c", width=40, height=20),
    ]
    layout = Layout(
        rects,
        [
            AlignConstraint("a", Anchor.TOP, Axis.Y, 10),
            AlignConstraint("b", Anchor.TOP, Axis.Y, 10),
            AlignConstraint("c", Anchor.TOP, Axis.Y, 10),
            AlignConstraint("a", Anchor.LEFT, Axis.X, 0),
            EvenlySpacedSequenceConstraint(["a", "b", "c"], Anchor.LEFT, 50),
        ],
    )

    values = layout.solve()
    layout.write_svg(
        "/tmp/kiwi-place-test_even_spacing_of_aligned_columns.svg")

    assert values["a"]["x"] == pytest.approx(0)
    assert values["b"]["x"] == pytest.approx(50)
    assert values["c"]["x"] == pytest.approx(100)
    assert values["a"]["y"] == pytest.approx(10)
    assert values["b"]["y"] == pytest.approx(10)
    assert values["c"]["y"] == pytest.approx(10)


def test_parent_wrap_and_child_from_parent() -> None:
    rects = [
        Rect("parent"),
        Rect("c1", width=60, height=20),
        Rect("c2", width=70, height=30),
        Rect("overlay"),
    ]
    layout = Layout(
        rects,
        [
            AlignConstraint("c1", Anchor.LEFT, Axis.X, 20),
            AlignConstraint("c1", Anchor.TOP, Axis.Y, 30),
            RelativeAnchorConstraint("c2", Anchor.LEFT, "c1", Anchor.LEFT, 0),
            RelativeAnchorConstraint("c2", Anchor.TOP, "c1", Anchor.BOTTOM,
                                     15),
            ParentWrapConstraint(
                "parent",
                ["c1", "c2"],
                padding_left=10,
                padding_top=5,
                padding_right=12,
                padding_bottom=7,
            ),
            ChildFromParentConstraint(
                "overlay",
                "parent",
                width_ratio=0.2,
                height_ratio=0.5,
                x_anchor=Anchor.HCENTER,
                y_anchor=Anchor.VCENTER,
                x_offset=20,
                y_offset=0,
            ),
        ],
    )

    values = layout.solve()
    layout.write_svg(
        "/tmp/kiwi-place-test_parent_wrap_and_child_from_parent.svg")

    assert values["parent"]["x"] == pytest.approx(10)
    assert values["parent"]["y"] == pytest.approx(25)
    assert values["parent"]["width"] == pytest.approx(92)
    assert values["parent"]["height"] == pytest.approx(77)
    assert values["overlay"]["width"] == pytest.approx(16.4)
    assert values["overlay"]["height"] == pytest.approx(38.5)
    assert values["overlay"]["x"] + values["overlay"][
        "width"] * 0.5 == pytest.approx(values["parent"]["x"] +
                                        values["parent"]["width"] * 0.5 + 20)
    assert values["overlay"]["y"] + values["overlay"][
        "height"] * 0.5 == pytest.approx(values["parent"]["y"] +
                                         values["parent"]["height"] * 0.5)

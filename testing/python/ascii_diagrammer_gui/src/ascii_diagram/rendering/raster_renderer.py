from __future__ import annotations

import math

from typing import Callable, Dict, List, Optional, Tuple

from PySide6.QtCore import QPoint, QPointF

from ascii_diagram.model.enums import (
    EdgeType,
    HJustify,
    OverlapMode,
    ShapeType,
    VAlign,
)
from ascii_diagram.model.scene_item import SceneItem
from ascii_diagram.model.scene_model import SceneModel
from ascii_diagram.model.shape_properties import (
    EdgeData,
    EllipseData,
    RectData,
    ShapeProperties,
    TextData,
)
from ascii_diagram.rendering.ascii_grid import AsciiGrid


def render_scene_to_grid(model: SceneModel) -> AsciiGrid:
    grid = AsciiGrid()
    items = model.all_items()
    for item in items:
        render_item(grid, item)
    return grid


def render_item(grid: AsciiGrid, item: SceneItem) -> None:
    abs_pos = item.absolute_position()
    props = item.properties
    mode = props.overlap_mode

    if isinstance(props, RectData):
        _stamp_rectangle(grid, abs_pos.x(), abs_pos.y(), props, mode)
    elif isinstance(props, EdgeData):
        _stamp_edge(grid, abs_pos.x(), abs_pos.y(), props, mode)
    elif isinstance(props, EllipseData):
        _stamp_ellipse(grid, abs_pos.x(), abs_pos.y(), props, mode)
    elif isinstance(props, TextData):
        _stamp_text(grid, abs_pos.x(), abs_pos.y(), props, mode)


def _stamp_rectangle(
    grid: AsciiGrid,
    abs_x: int,
    abs_y: int,
    data: RectData,
    mode: OverlapMode,
) -> None:
    x = abs_x
    y = abs_y
    w = data.width
    h = data.height
    if w <= 0 or h <= 0:
        return
    for col in range(x, x + w):
        grid.write(col, y, data.top_ch, mode)
        if h > 1:
            grid.write(col, y + h - 1, data.bottom_ch, mode)
    for row in range(y + 1, y + h - 1):
        grid.write(x, row, data.left_ch, mode)
        if w > 1:
            grid.write(x + w - 1, row, data.right_ch, mode)
    if w > 0 and h > 0:
        grid.write(x, y, data.corner_ch, mode)
        if w > 1:
            grid.write(x + w - 1, y, data.corner_ch, mode)
        if h > 1:
            grid.write(x, y + h - 1, data.corner_ch, mode)
            if w > 1:
                grid.write(x + w - 1, y + h - 1, data.corner_ch, mode)


def _stamp_edge(
    grid: AsciiGrid,
    abs_x: int,
    abs_y: int,
    data: EdgeData,
    mode: OverlapMode,
) -> None:
    sx = abs_x + data.start.x()
    sy = abs_y + data.start.y()
    ex = abs_x + data.end.x()
    ey = abs_y + data.end.y()

    if data.edge_type == EdgeType.ORTHOGONAL:
        _stamp_orthogonal(grid, sx, sy, ex, ey, data, mode)
    elif data.edge_type == EdgeType.SPLINE:
        _stamp_spline(grid, sx, sy, ex, ey, data, mode)
    else:
        _stamp_polyline(grid, sx, sy, ex, ey, data, mode)


def _segment_char(x1: int, y1: int, x2: int, y2: int, data: EdgeData) -> str:
    dx = abs(x2 - x1)
    dy = abs(y2 - y1)
    return data.h_char if dx >= dy else data.v_char


def _draw_line_segment(
    grid: AsciiGrid,
    x1: int,
    y1: int,
    x2: int,
    y2: int,
    ch: str,
    mode: OverlapMode,
) -> None:
    dx = abs(x2 - x1)
    dy = abs(y2 - y1)
    sx_step = 1 if x1 < x2 else -1
    sy_step = 1 if y1 < y2 else -1
    err = dx - dy

    cx, cy = x1, y1
    while True:
        grid.write(cx, cy, ch, mode)
        if cx == x2 and cy == y2:
            break
        e2 = 2 * err
        if e2 > -dy:
            err -= dy
            cx += sx_step
        if e2 < dx:
            err += dx
            cy += sy_step


def _draw_straight_line(
    grid: AsciiGrid,
    x1: int,
    y1: int,
    x2: int,
    y2: int,
    ch: str,
    mode: OverlapMode,
) -> None:
    if x1 == x2:
        step = 1 if y2 > y1 else -1
        for y in range(y1, y2 + step, step):
            grid.write(x1, y, ch, mode)
    else:
        step = 1 if x2 > x1 else -1
        for x in range(x1, x2 + step, step):
            grid.write(x, y1, ch, mode)


def _stamp_polyline(
    grid: AsciiGrid,
    sx: int,
    sy: int,
    ex: int,
    ey: int,
    data: EdgeData,
    mode: OverlapMode,
) -> None:
    points = [(sx, sy)]
    for b in data.bends:
        bx = sx + b.x()
        by = sy + b.y()
        points.append((bx, by))
    points.append((ex, ey))

    for i in range(len(points) - 1):
        x1, y1 = points[i]
        x2, y2 = points[i + 1]
        ch = _segment_char(x1, y1, x2, y2, data)
        _draw_line_segment(grid, x1, y1, x2, y2, ch, mode)
        if i < len(points) - 2 and data.bend_char:
            grid.write(x2, y2, data.bend_char, mode)

    if data.start_arrow:
        grid.write(sx, sy, data.start_arrow, mode)
    if data.end_arrow:
        grid.write(ex, ey, data.end_arrow, mode)


def _stamp_orthogonal(
    grid: AsciiGrid,
    sx: int,
    sy: int,
    ex: int,
    ey: int,
    data: EdgeData,
    mode: OverlapMode,
) -> None:
    if data.bends:
        points = [(sx, sy)]
        for b in data.bends:
            bx = sx + b.x()
            by = sy + b.y()
            points.append((bx, by))
        points.append((ex, ey))

        for i in range(len(points) - 1):
            x1, y1 = points[i]
            x2, y2 = points[i + 1]
            if x1 == x2:
                _draw_straight_line(grid, x1, y1, x2, y2, data.v_char, mode)
            elif y1 == y2:
                _draw_straight_line(grid, x1, y1, x2, y2, data.h_char, mode)
            else:
                _draw_straight_line(grid, x1, y1, x2, y1, data.h_char, mode)
                _draw_straight_line(grid, x2, y1, x2, y2, data.v_char, mode)
                if data.bend_char:
                    grid.write(x2, y1, data.bend_char, mode)
    else:
        _draw_straight_line(grid, sx, sy, ex, sy, data.h_char, mode)
        _draw_straight_line(grid, ex, sy, ex, ey, data.v_char, mode)
        if data.bend_char:
            grid.write(ex, sy, data.bend_char, mode)

    if data.start_arrow:
        grid.write(sx, sy, data.start_arrow, mode)
    if data.end_arrow:
        grid.write(ex, ey, data.end_arrow, mode)


def _stamp_spline(
    grid: AsciiGrid,
    sx: int,
    sy: int,
    ex: int,
    ey: int,
    data: EdgeData,
    mode: OverlapMode,
) -> None:
    nbends = len(data.bends)
    if nbends == 0:
        ch = _segment_char(sx, sy, ex, ey, data)
        _draw_line_segment(grid, sx, sy, ex, ey, ch, mode)
    elif nbends == 1:
        cp = data.bends[0]
        cx = sx + cp.x()
        cy = sy + cp.y()
        _bezier_quadratic(grid, sx, sy, cx, cy, ex, ey, data, mode)
    elif nbends == 2:
        cp1 = data.bends[0]
        cp2 = data.bends[1]
        cx1 = sx + cp1.x()
        cy1 = sy + cp1.y()
        cx2 = sx + cp2.x()
        cy2 = sy + cp2.y()
        _bezier_cubic(grid, sx, sy, cx1, cy1, cx2, cy2, ex, ey, data, mode)
    else:
        _stamp_polyline(grid, sx, sy, ex, ey, data, mode)

    if data.start_arrow:
        grid.write(sx, sy, data.start_arrow, mode)
    if data.end_arrow:
        grid.write(ex, ey, data.end_arrow, mode)


def _bezier_quadratic(
    grid: AsciiGrid,
    x0: int,
    y0: int,
    cx: int,
    cy: int,
    x2: int,
    y2: int,
    data: EdgeData,
    mode: OverlapMode,
) -> None:
    steps = max(
        abs(x2 - x0) + abs(y2 - y0) + abs(cx - x0) + abs(cy - y0),
        10,
    )
    prev_x, prev_y = x0, y0
    for i in range(1, steps + 1):
        t = i / steps
        u = 1 - t
        px = round(u * u * x0 + 2 * u * t * cx + t * t * x2)
        py = round(u * u * y0 + 2 * u * t * cy + t * t * y2)
        ch = _segment_char(prev_x, prev_y, px, py, data)
        _draw_line_segment(grid, prev_x, prev_y, px, py, ch, mode)
        prev_x, prev_y = px, py


def _bezier_cubic(
    grid: AsciiGrid,
    x0: int,
    y0: int,
    cx1: int,
    cy1: int,
    cx2: int,
    cy2: int,
    x3: int,
    y3: int,
    data: EdgeData,
    mode: OverlapMode,
) -> None:
    steps = max(
        abs(x3 - x0) + abs(y3 - y0) + abs(cx1 - x0) + abs(cy1 - y0) +
        abs(cx2 - x0) + abs(cy2 - y0),
        10,
    )
    prev_x, prev_y = x0, y0
    for i in range(1, steps + 1):
        t = i / steps
        u = 1 - t
        px = round(u**3 * x0 + 3 * u**2 * t * cx1 + 3 * u * t**2 * cx2 +
                   t**3 * x3)
        py = round(u**3 * y0 + 3 * u**2 * t * cy1 + 3 * u * t**2 * cy2 +
                   t**3 * y3)
        ch = _segment_char(prev_x, prev_y, px, py, data)
        _draw_line_segment(grid, prev_x, prev_y, px, py, ch, mode)
        prev_x, prev_y = px, py


def _stamp_ellipse(
    grid: AsciiGrid,
    abs_x: int,
    abs_y: int,
    data: EllipseData,
    mode: OverlapMode,
) -> None:
    cx = abs_x + data.width / 2.0
    cy = abs_y + data.height / 2.0
    rx = data.width / 2.0
    ry = data.height / 2.0
    if rx <= 0 or ry <= 0:
        return

    steps = max(int(2 * math.pi * max(rx, ry)), 16)
    prev_x = round(cx + rx)
    prev_y = round(cy)
    for i in range(1, steps + 1):
        angle = 2 * math.pi * i / steps
        px = round(cx + rx * math.cos(angle))
        py = round(cy + ry * math.sin(angle))
        _draw_line_segment(grid, prev_x, prev_y, px, py, data.char, mode)
        prev_x, prev_y = px, py


def _stamp_text(
    grid: AsciiGrid,
    abs_x: int,
    abs_y: int,
    data: TextData,
    mode: OverlapMode,
) -> None:
    x = abs_x
    y = abs_y
    w = data.width
    h = data.height
    if w <= 0 or h <= 0:
        return

    lines = []
    remaining = data.text
    while remaining and len(lines) < h:
        if len(remaining) <= w:
            lines.append(remaining)
            break
        lines.append(remaining[:w])
        remaining = remaining[w:]

    if data.v_align == VAlign.CENTER:
        y_offset = (h - len(lines)) // 2
    elif data.v_align == VAlign.BOTTOM:
        y_offset = h - len(lines)
    else:
        y_offset = 0

    for li, line in enumerate(lines):
        row = y + y_offset + li
        if row >= y + h:
            break

        if data.h_justify == HJustify.CENTER:
            start_col = x + (w - len(line)) // 2
        elif data.h_justify == HJustify.RIGHT:
            start_col = x + w - len(line)
        else:
            start_col = x

        for ci, ch in enumerate(line):
            grid.write(start_col + ci, row, ch, mode)

    for r in range(y, y + h):
        for c in range(x, x + w):
            if grid.get(c, r) == " ":
                grid.set(c, r, " ")

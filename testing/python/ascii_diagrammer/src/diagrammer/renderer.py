from __future__ import annotations

import math

from diagrammer.charsets import (
    ASCII_CHARSET,
    ASCII_LINE_CHARSET,
    UNICODE_CHARSET,
    UNICODE_LINE_CHARSET,
    BoxCharset,
    LineCharset,
    make_single_char_box_charset,
    make_single_char_line_charset,
)
from diagrammer.ir import ResolvedShape, Scene, ShapeKind


class Grid:
    def __init__(self, width: int, height: int):
        self.width = width
        self.height = height
        self.cells: list[list[str]] = [[" " for _ in range(width)] for _ in range(height)]

    def set(self, x: int, y: int, ch: str) -> None:
        if 0 <= x < self.width and 0 <= y < self.height:
            self.cells[y][x] = ch

    def get(self, x: int, y: int) -> str:
        if 0 <= x < self.width and 0 <= y < self.height:
            return self.cells[y][x]
        return " "

    def to_string(self) -> str:
        lines = []
        for row in self.cells:
            line = "".join(row).rstrip()
            lines.append(line)
        # Strip trailing empty lines
        while lines and lines[-1] == "":
            lines.pop()
        return "\n".join(lines) + "\n"


def _get_box_charset(shape: ResolvedShape, default: BoxCharset) -> BoxCharset:
    if shape.charset_override is not None:
        if len(shape.charset_override) == 1:
            return make_single_char_box_charset(shape.charset_override)
    return default


def _get_line_charset(shape: ResolvedShape, default: LineCharset) -> LineCharset:
    if shape.charset_override is not None:
        if len(shape.charset_override) == 1:
            return make_single_char_line_charset(shape.charset_override)
    return default


def _draw_rect(grid: Grid, shape: ResolvedShape, box_charset: BoxCharset, scale: float) -> None:
    x = round(shape.box.x * scale)
    y = round(shape.box.y * scale)
    w = round(shape.box.width * scale)
    h = round(shape.box.height * scale)

    if w < 1 or h < 1:
        return

    cs = _get_box_charset(shape, box_charset)

    # Top edge
    grid.set(x, y, cs.top_left)
    for i in range(1, w - 1):
        grid.set(x + i, y, cs.horizontal)
    if w > 1:
        grid.set(x + w - 1, y, cs.top_right)

    # Bottom edge
    if h > 1:
        grid.set(x, y + h - 1, cs.bottom_left)
        for i in range(1, w - 1):
            grid.set(x + i, y + h - 1, cs.horizontal)
        if w > 1:
            grid.set(x + w - 1, y + h - 1, cs.bottom_right)

    # Sides
    for j in range(1, h - 1):
        grid.set(x, y + j, cs.vertical)
        if w > 1:
            grid.set(x + w - 1, y + j, cs.vertical)


def _draw_ellipse(grid: Grid, shape: ResolvedShape, box_charset: BoxCharset, scale: float) -> None:
    x = round(shape.box.x * scale)
    y = round(shape.box.y * scale)
    w = round(shape.box.width * scale)
    h = round(shape.box.height * scale)

    if w < 1 or h < 1:
        return

    cs = _get_box_charset(shape, box_charset)

    cx = x + w / 2.0
    cy = y + h / 2.0
    rx = w / 2.0
    ry = h / 2.0

    for py in range(y, y + h):
        for px in range(x, x + w):
            dx = (px + 0.5 - cx) / rx if rx > 0 else 0
            dy = (py + 0.5 - cy) / ry if ry > 0 else 0
            dist = dx * dx + dy * dy
            # Draw boundary
            if 0.5 < dist <= 1.5:
                # Determine which character to use based on position
                angle = math.atan2(py + 0.5 - cy, px + 0.5 - cx)
                deg = math.degrees(angle) % 360
                if (315 <= deg or deg < 45) or (135 <= deg < 225):
                    grid.set(px, py, cs.vertical)
                else:
                    grid.set(px, py, cs.horizontal)


def _draw_line(grid: Grid, shape: ResolvedShape, line_charset: LineCharset, scale: float) -> None:
    cs = _get_line_charset(shape, line_charset)
    points = shape.line_points

    i = 0
    while i < len(points) - 1:
        if points[i] is None:
            i += 1
            continue
        if points[i + 1] is None:
            i += 2
            continue

        x0, y0 = points[i]
        x1, y1 = points[i + 1]

        sx0 = round(x0 * scale)
        sy0 = round(y0 * scale)
        sx1 = round(x1 * scale)
        sy1 = round(y1 * scale)

        _bresenham_line(grid, sx0, sy0, sx1, sy1, cs)
        i += 1


def _bresenham_line(
    grid: Grid, x0: int, y0: int, x1: int, y1: int, cs: LineCharset
) -> None:
    dx = abs(x1 - x0)
    dy = abs(y1 - y0)
    sx = 1 if x0 < x1 else -1
    sy = 1 if y0 < y1 else -1

    if dx == 0:
        ch = cs.vertical
    elif dy == 0:
        ch = cs.horizontal
    else:
        ch = cs.horizontal if dx > dy else cs.vertical

    err = dx - dy
    while True:
        grid.set(x0, y0, ch)
        if x0 == x1 and y0 == y1:
            break
        e2 = 2 * err
        if e2 > -dy:
            err -= dy
            x0 += sx
        if e2 < dx:
            err += dx
            y0 += sy


def _draw_text(grid: Grid, shape: ResolvedShape, scale: float) -> None:
    if shape.text is None:
        return

    x = round(shape.box.x * scale)
    y = round(shape.box.y * scale)

    if shape.wrap_width is not None:
        lines = _wrap_text(shape.text, shape.wrap_width)
    else:
        lines = [shape.text]

    for row_idx, line in enumerate(lines):
        for col_idx, ch in enumerate(line):
            grid.set(x + col_idx, y + row_idx, ch)


def _wrap_text(text: str, width: int) -> list[str]:
    words = text.split()
    lines: list[str] = []
    current_line = ""
    for word in words:
        if current_line:
            if len(current_line) + 1 + len(word) <= width:
                current_line += " " + word
            else:
                lines.append(current_line)
                current_line = word
        else:
            current_line = word
    if current_line:
        lines.append(current_line)
    return lines


def _draw_shape(
    grid: Grid,
    shape: ResolvedShape,
    box_charset: BoxCharset,
    line_charset: LineCharset,
    scale: float,
) -> None:
    match shape.kind:
        case ShapeKind.RECT:
            _draw_rect(grid, shape, box_charset, scale)
        case ShapeKind.CIRCLE | ShapeKind.ELLIPSE:
            _draw_ellipse(grid, shape, box_charset, scale)
        case ShapeKind.LINE:
            _draw_line(grid, shape, line_charset, scale)
        case ShapeKind.TEXT:
            _draw_text(grid, shape, scale)
        case ShapeKind.GROUP:
            pass  # invisible, just a container

    # Draw subnodes on top
    for subnode in shape.subnodes:
        _draw_shape(grid, subnode, box_charset, line_charset, scale)


def render(scene: Scene, charset: str = "unicode", scale: float = 1.0) -> str:
    if charset == "unicode":
        box_cs = UNICODE_CHARSET
        line_cs = UNICODE_LINE_CHARSET
    else:
        box_cs = ASCII_CHARSET
        line_cs = ASCII_LINE_CHARSET

    canvas_w = scene.width if scene.width else 80
    canvas_h = scene.height if scene.height else 24

    grid_w = round(canvas_w * scale)
    grid_h = round(canvas_h * scale)

    # Add small margin
    grid_w = max(grid_w, 1)
    grid_h = max(grid_h, 1)

    grid = Grid(grid_w, grid_h)

    for shape in scene.shapes:
        _draw_shape(grid, shape, box_cs, line_cs, scale)

    return grid.to_string()

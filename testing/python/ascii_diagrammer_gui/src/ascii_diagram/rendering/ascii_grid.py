from __future__ import annotations

from typing import Callable, Dict, List, Optional, Tuple

from ascii_diagram.model.enums import OverlapMode

DEFAULT_INTERSECTION_RULES: Dict[Tuple[str, str], str] = {
    ("|", "-"): "+",
    ("-", "|"): "+",
    ("|", "+"): "+",
    ("-", "+"): "+",
    ("+", "|"): "+",
    ("+", "-"): "+",
}


class AsciiGrid:

    def __init__(self) -> None:
        self._grid: Dict[Tuple[int, int], str] = {}

    def clear(self) -> None:
        self._grid.clear()

    def get(self, x: int, y: int) -> str:
        return self._grid.get((x, y), " ")

    def set(self, x: int, y: int, char: str) -> None:
        if char:
            self._grid[(x, y)] = char

    def write(
        self,
        x: int,
        y: int,
        char: str,
        mode: OverlapMode = OverlapMode.OVERWRITE,
        intersection_rules: Optional[Dict[Tuple[str, str], str]] = None,
    ) -> None:
        if not char:
            return
        if mode == OverlapMode.OVERWRITE:
            self._grid[(x, y)] = char
        elif mode == OverlapMode.EMPTY_ONLY:
            if self.get(x, y) == " ":
                self._grid[(x, y)] = char
        elif mode == OverlapMode.MERGE:
            existing = self.get(x, y)
            if existing == " ":
                self._grid[(x, y)] = char
            else:
                rules = (intersection_rules if intersection_rules is not None
                         else DEFAULT_INTERSECTION_RULES)
                merged = rules.get((existing, char))
                if merged is not None:
                    self._grid[(x, y)] = merged

    def bounds(self) -> Tuple[int, int, int, int]:
        if not self._grid:
            return (0, 0, 0, 0)
        xs = [p[0] for p in self._grid]
        ys = [p[1] for p in self._grid]
        return (min(xs), min(ys), max(xs) + 1, max(ys) + 1)

    def to_string(self, crop_to_bounds: bool = True) -> str:
        if not self._grid:
            return ""
        if crop_to_bounds:
            min_x, min_y, max_x, max_y = self.bounds()
        else:
            min_x = min(p[0] for p in self._grid)
            min_y = min(p[1] for p in self._grid)
            max_x = max(p[0] for p in self._grid) + 1
            max_y = max(p[1] for p in self._grid) + 1
        width = max_x - min_x
        if width <= 0:
            return ""
        lines = []
        for row in range(min_y, max_y):
            line = "".join(self.get(col, row) for col in range(min_x, max_x))
            lines.append(line)
        return "\n".join(lines)

    def to_lines(self, crop_to_bounds: bool = True) -> List[str]:
        s = self.to_string(crop_to_bounds)
        if s:
            return s.split("\n")
        return []

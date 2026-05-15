from __future__ import annotations

import base64
import mimetypes
import os
import re
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from dominate.util import raw

from flask import Flask, jsonify, request
from dominate import document
from dominate.tags import div, h1, img, script, style

from dynamic_window_view.main_walker import get_structure, Rect, SplitPane, TagInfo, WindowInfo, print_tags

CSS = """
* {
  box-sizing: border-box;
}

body {
  margin: 0;
  padding: 16px;
  font-family: sans-serif;
  background: #111;
  color: #eee;
}

h1 {
  margin: 0 0 16px 0;
  font-size: 18px;
}

#layout-root {
  position: relative;
  width: 100%;
  min-height: 600px;
  background: #1a1a1a;
  overflow: auto;
  padding: 8px;
}

.monitor {
  position: absolute;
  border: 2px solid #3b82f6;
  background: rgba(59, 130, 246, 0.05);
}

.tag {
  position: absolute;
  inset: 0;
  border: 2px solid #3b82f6;
  pointer-events: auto;
}

.window {
  position: absolute;
  border: 2px solid #3b82f6;
  overflow: hidden;
  background: rgba(255, 255, 255, 0.02);
}

.window-image {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  object-fit: cover;
  opacity: 0.85;
  pointer-events: none;
}

.window-label,
.split-label,
.tag-label {
  position: absolute;
  left: 4px;
  top: 4px;
  font-size: 12px;
  line-height: 1.2;
  padding: 2px 4px;
  background: rgba(0, 0, 0, 0.65);
  color: #fff;
  z-index: 10;
  max-width: calc(100% - 8px);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.split {
  position: absolute;
  border: 2px solid #22c55e;
  background: rgba(34, 197, 94, 0.04);
}

.clickable {
  cursor: pointer;
}
"""

JS = """
document.addEventListener('click', async (event) => {
  const el = event.target.closest('[data-kind]');
  if (!el) {
    return;
  }

  const payload = {
    kind: el.dataset.kind,
    screen: el.dataset.screen || null,
    tag: el.dataset.tag || null,
    window_id: el.dataset.windowId || null,
    split_path: el.dataset.splitPath || null,
    title: el.dataset.title || null,
    role: el.dataset.role || null,
    name: el.dataset.name || null,
  };

  try {
    await fetch('/click', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload),
    });
  } catch (err) {
    console.error(err);
  }
});
"""


@dataclass
class MonitorInfo:
    name: str
    x: int
    y: int
    width: int
    height: int


def parse_xrandr_monitors() -> list[MonitorInfo]:
    output = subprocess.check_output(["xrandr", "--listmonitors"], text=True)
    monitors: list[MonitorInfo] = []
    pattern = re.compile(
        r"\s*\d+:\s+\+\*?([^\s]+)\s+(\d+)/\d+x(\d+)/\d+\+(-?\d+)\+(-?\d+)\s+(.+)$"
    )

    for line in output.splitlines():
        match = pattern.match(line)
        if not match:
            continue

        monitors.append(
            MonitorInfo(
                name=match.group(1),
                width=int(match.group(2)),
                height=int(match.group(3)),
                x=int(match.group(4)),
                y=int(match.group(5)),
            ))

    return monitors


def get_visible_tags(tags: list[TagInfo]) -> list[TagInfo]:
    return [
        tag for tag in tags if tag.selected and any(win.visible_on_selected_tag
                                                    for win in tag.windows)
    ]


def compute_bounds(monitors: list[MonitorInfo]) -> Rect:
    min_x = min(m.x for m in monitors)
    min_y = min(m.y for m in monitors)
    max_x = max(m.x + m.width for m in monitors)
    max_y = max(m.y + m.height for m in monitors)
    return Rect(x=min_x, y=min_y, width=max_x - min_x, height=max_y - min_y)


def clamp_rect(rect: Rect, min_width: int = 1, min_height: int = 1) -> Rect:
    return Rect(
        x=rect.x,
        y=rect.y,
        width=max(min_width, rect.width),
        height=max(min_height, rect.height),
    )


def shrink_rect(rect: Rect, margin: int) -> Rect:
    return clamp_rect(
        Rect(
            x=rect.x + margin,
            y=rect.y + margin,
            width=rect.width - margin * 2,
            height=rect.height - margin * 2,
        ))


def rect_to_style(rect: Rect) -> str:
    return f"left:{rect.x}px; top:{rect.y}px; width:{rect.width}px; height:{rect.height}px;"


def take_window_screenshot(wid: int) -> str | None:
    with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as tmp:
        tmp_path = Path(tmp.name)

    try:
        subprocess.run(
            ["maim", "-i", str(wid), str(tmp_path)],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        data = tmp_path.read_bytes()
    finally:
        if tmp_path.exists():
            tmp_path.unlink()

    mime, _ = mimetypes.guess_type("x.png")
    encoded = base64.b64encode(data).decode("ascii")
    return f"data:{mime};base64,{encoded}"


def build_split_node(parent, split: SplitPane, path: str, window_rect: Rect,
                     screen: int, tag_name: str, wid: int) -> None:
    split_rect = shrink_rect(split.rect, 10)

    rel_rect = clamp_rect(
        Rect(
            x=max(15, split_rect.x - window_rect.x),
            y=max(15, split_rect.y - window_rect.y),
            width=min(
                split_rect.width,
                max(
                    1, window_rect.width -
                    max(15, split_rect.x - window_rect.x) - 15)),
            height=min(
                split_rect.height,
                max(
                    1, window_rect.height -
                    max(15, split_rect.y - window_rect.y) - 15)),
        ))

    node = parent.add(
        div(
            cls="split clickable",
            style=rect_to_style(rel_rect),
            **{
                "data-kind": "split",
                "data-screen": str(screen),
                "data-tag": tag_name,
                "data-window-id": str(wid),
                "data-split-path": path,
                "data-role": split.role,
                "data-name": split.name,
            },
        ))
    node.add(div(f"{split.role} {split.name}".strip(), cls="split-label"))

    for idx, child in enumerate(split.children):
        build_split_node(node, child, f"{path}.{idx}", split_rect, screen,
                         tag_name, wid)


def build_window_node(parent, window: WindowInfo,
                      screenshot_uri: str | None) -> None:
    if not window.visible_on_selected_tag:
        return

    window_rect = shrink_rect(window.rect, 10)
    node = parent.add(
        div(
            cls="window clickable",
            style=rect_to_style(window_rect),
            **{
                "data-kind": "window",
                "data-screen": str(window.screen),
                "data-tag": ",".join(window.tags),
                "data-window-id": str(window.wid),
                "data-title": window.title,
            },
        ))

    if screenshot_uri is not None:
        node.add(img(cls="window-image", src=screenshot_uri))

    node.add(div(f"{window.title} [{window.wid}]", cls="window-label"))

    for idx, split in enumerate(window.splits):
        build_split_node(node, split, str(idx), window_rect, window.screen,
                         ",".join(window.tags), window.wid)


def build_tag_node(parent, tag: TagInfo, monitor: MonitorInfo,
                   screenshot_cache: dict[int, str | None]) -> None:
    monitor_rect = Rect(x=monitor.x,
                        y=monitor.y,
                        width=monitor.width,
                        height=monitor.height)
    monitor_node = parent.add(
        div(
            cls="monitor",
            style=rect_to_style(monitor_rect),
        ))

    tag_node = monitor_node.add(
        div(
            cls="tag clickable",
            **{
                "data-kind": "tag",
                "data-screen": str(tag.screen),
                "data-tag": tag.name,
            },
        ))
    tag_node.add(
        div(f"tag={tag.name} screen={tag.screen} layout={tag.layout}",
            cls="tag-label"))

    for window in tag.windows:
        if window.wid not in screenshot_cache:
            screenshot_cache[window.wid] = take_window_screenshot(window.wid)
        build_window_node(tag_node, window, screenshot_cache[window.wid])


def normalize_document_space(
        monitors: list[MonitorInfo],
        scale: float = 0.25) -> tuple[list[MonitorInfo], Rect]:
    bounds = compute_bounds(monitors)
    normalized: list[MonitorInfo] = []

    for monitor in monitors:
        normalized.append(
            MonitorInfo(
                name=monitor.name,
                x=int((monitor.x - bounds.x) * scale),
                y=int((monitor.y - bounds.y) * scale),
                width=max(1, int(monitor.width * scale)),
                height=max(1, int(monitor.height * scale)),
            ))

    scaled_bounds = Rect(
        x=0,
        y=0,
        width=max(m.x + m.width for m in normalized),
        height=max(m.y + m.height for m in normalized),
    )
    return normalized, scaled_bounds


def build_page(tags: list[TagInfo], monitors: list[MonitorInfo]) -> str:
    visible_tags = get_visible_tags(tags)
    monitor_by_screen = {idx: mon for idx, mon in enumerate(monitors)}
    normalized_monitors, scaled_bounds = normalize_document_space(monitors)
    normalized_by_screen = {
        idx: mon
        for idx, mon in enumerate(normalized_monitors)
    }
    screenshot_cache: dict[int, str | None] = {}

    scale_x_by_screen = {
        idx: normalized_by_screen[idx].width / monitor_by_screen[idx].width
        for idx in monitor_by_screen
    }
    scale_y_by_screen = {
        idx: normalized_by_screen[idx].height / monitor_by_screen[idx].height
        for idx in monitor_by_screen
    }

    scaled_tags: list[TagInfo] = []
    for tag in visible_tags:
        scaled_windows: list[WindowInfo] = []
        sx = scale_x_by_screen[tag.screen]
        sy = scale_y_by_screen[tag.screen]
        monitor = monitor_by_screen[tag.screen]

        for win in tag.windows:
            if not win.visible_on_selected_tag:
                continue

            scaled_splits = scale_splits(win.splits, monitor, sx, sy)
            scaled_windows.append(
                WindowInfo(
                    wid=win.wid,
                    title=win.title,
                    wm_class=win.wm_class,
                    pid=win.pid,
                    rect=Rect(
                        x=int((win.rect.x - monitor.x) * sx),
                        y=int((win.rect.y - monitor.y) * sy),
                        width=max(1, int(win.rect.width * sx)),
                        height=max(1, int(win.rect.height * sy)),
                    ),
                    splits=scaled_splits,
                    screen=win.screen,
                    tags=win.tags,
                    visible_on_selected_tag=win.visible_on_selected_tag,
                    floating=win.floating,
                    maximized=win.maximized,
                    minimized=win.minimized,
                    fullscreen=win.fullscreen,
                    urgent=win.urgent,
                    hidden=win.hidden,
                    ontop=win.ontop,
                    sticky=win.sticky,
                    above=win.above,
                    below=win.below,
                ))

        scaled_tags.append(
            TagInfo(
                name=tag.name,
                screen=tag.screen,
                selected=tag.selected,
                activated=tag.activated,
                index=tag.index,
                layout=tag.layout,
                master_count=tag.master_count,
                column_count=tag.column_count,
                master_width_factor=tag.master_width_factor,
                gap=tag.gap,
                volatile=tag.volatile,
                windows=scaled_windows,
            ))

    doc = document(title="Awesome WM layout")
    with doc.head:
        style(CSS)

    with doc:
        h1("Awesome WM visible layout")
        root = div(
            id="layout-root",
            style=
            f"width:{scaled_bounds.width + 16}px; height:{scaled_bounds.height + 16}px; position:relative;",
        )

        for tag in scaled_tags:
            build_tag_node(root, tag, normalized_by_screen[tag.screen],
                           screenshot_cache)

        with script(type="text/javascript"):
            raw(JS)

    return str(doc)


def scale_splits(splits: list[SplitPane], monitor: MonitorInfo, sx: float,
                 sy: float) -> list[SplitPane]:
    result: list[SplitPane] = []
    for split in splits:
        result.append(
            SplitPane(
                rect=Rect(
                    x=int((split.rect.x - monitor.x) * sx),
                    y=int((split.rect.y - monitor.y) * sy),
                    width=max(1, int(split.rect.width * sx)),
                    height=max(1, int(split.rect.height * sy)),
                ),
                role=split.role,
                name=split.name,
                children=scale_splits(split.children, monitor, sx, sy),
            ))
    return result


app = Flask(__name__)


@app.route("/")
def index():
    tags = get_structure()
    Path("/tmp/result.txt").write_text(print_tags(tags))
    monitors = parse_xrandr_monitors()
    return build_page(tags, monitors)


@app.route("/click", methods=["POST"])
def click():
    payload = request.get_json()
    print(payload, flush=True)
    return jsonify({"ok": True})

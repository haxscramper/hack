#!/usr/bin/env python
import json
import sys
from pathlib import Path
from typing import Any, Literal

from pydantic import BaseModel, TypeAdapter

import glfw
import imgui
from imgui.integrations.glfw import GlfwRenderer
from OpenGL.GL import glClear, glClearColor, GL_COLOR_BUFFER_BIT


class BaseEvent(BaseModel):
    id: str
    file: str | None = None
    line: int | None = None
    metadata: dict[str, Any] | None = None


class ScopeStartEvent(BaseEvent):
    event: Literal["scope_start"]
    name: str


class ScopeEndEvent(BaseEvent):
    event: Literal["scope_end"]


class InstantEvent(BaseEvent):
    event: Literal["instant"]
    name: str


LogEvent = TypeAdapter(ScopeStartEvent | ScopeEndEvent | InstantEvent)


class EventNode:

    def __init__(self,
                 title: str,
                 event: BaseEvent | None = None,
                 parent: "EventNode | None" = None):
        self.title = title
        self.event = event
        self.parent = parent
        self.children: list[EventNode] = []

    def append(self, child: "EventNode") -> None:
        child.parent = self
        self.children.append(child)


def event_title(event: BaseEvent) -> str:
    parts = [event.event, event.id]
    if hasattr(event, "name"):
        parts.append(getattr(event, "name"))
    if event.file is not None and event.line is not None:
        parts.append(f"{event.file}:{event.line}")
    elif event.file is not None:
        parts.append(event.file)
    return " | ".join(parts)


def event_details(event: BaseEvent | None) -> str:
    if event is None:
        return ""
    payload = event.model_dump(exclude_none=True)
    return json.dumps(payload, indent=2, ensure_ascii=False)


def build_tree(events: list[BaseEvent]) -> EventNode:
    root = EventNode("root")
    stack: list[EventNode] = [root]

    for event in events:
        if isinstance(event, ScopeStartEvent):
            node = EventNode(event_title(event), event)
            stack[-1].append(node)
            stack.append(node)
        elif isinstance(event, ScopeEndEvent):
            node = EventNode(event_title(event), event)
            stack[-1].append(node)
            if len(stack) == 1:
                raise ValueError(
                    f"scope_end without matching scope_start for id={event.id}"
                )
            start_node = stack[-1]
            if start_node.event is None or not isinstance(
                    start_node.event, ScopeStartEvent):
                raise ValueError(
                    f"invalid scope stack for scope_end id={event.id}")
            if start_node.event.id != event.id:
                raise ValueError(
                    f"scope_end id={event.id} does not match open scope id={start_node.event.id}"
                )
            stack.pop()
        elif isinstance(event, InstantEvent):
            node = EventNode(event_title(event), event)
            stack[-1].append(node)
        else:
            raise TypeError(f"unsupported event: {type(event)}")

    if len(stack) != 1:
        open_ids = [
            node.event.id for node in stack[1:] if node.event is not None
            and isinstance(node.event, ScopeStartEvent)
        ]
        raise ValueError(f"unclosed scopes remain: {open_ids}")

    return root


def ensure_example_file(path: Path) -> None:
    if path.exists():
        return

    records = [
        {
            "event": "scope_start",
            "id": "1",
            "name": "main",
            "file": "app.py",
            "line": 10,
            "metadata": {
                "variables": {
                    "user": "alice",
                    "mode": "debug"
                }
            },
        },
        {
            "event": "instant",
            "id": "2",
            "name": "config_loaded",
            "file": "app.py",
            "line": 12,
            "metadata": {
                "variables": {
                    "config_path": "/tmp/config.json"
                }
            },
        },
        {
            "event": "scope_start",
            "id": "3",
            "name": "worker",
            "file": "worker.py",
            "line": 21,
            "metadata": {
                "variables": {
                    "task_id": 42
                }
            },
        },
        {
            "event": "instant",
            "id": "4",
            "name": "step",
            "file": "worker.py",
            "line": 25,
            "metadata": {
                "variables": {
                    "progress": 0.5,
                    "state": "running"
                }
            },
        },
        {
            "event": "scope_end",
            "id": "3",
            "file": "worker.py",
            "line": 30,
            "metadata": {
                "variables": {
                    "status": "ok"
                }
            },
        },
        {
            "event": "instant",
            "id": "5",
            "name": "cleanup",
            "file": "app.py",
            "line": 40,
            "metadata": {
                "variables": {
                    "deleted": 3
                }
            },
        },
        {
            "event": "scope_end",
            "id": "1",
            "file": "app.py",
            "line": 50,
            "metadata": {
                "variables": {
                    "result": "success"
                }
            },
        },
    ]

    with path.open("w", encoding="utf-8") as file:
        for record in records:
            file.write(json.dumps(record, ensure_ascii=False) + "\n")


def load_events(path: Path) -> list[BaseEvent]:
    events: list[BaseEvent] = []
    with path.open("r", encoding="utf-8") as file:
        for lineno, line in enumerate(file, start=1):
            stripped = line.strip()
            if not stripped:
                continue
            try:
                payload = json.loads(stripped)
                event = LogEvent.validate_python(payload)
                events.append(event)
            except Exception as exc:
                raise ValueError(f"{path}:{lineno}: {exc}") from exc
    return events


def node_matches(node: EventNode, query: str) -> bool:
    if not query:
        return True

    haystacks = [node.title]
    if node.event is not None:
        haystacks.append(event_details(node.event))

    lowered = query.lower()
    for text in haystacks:
        if lowered in text.lower():
            return True

    for child in node.children:
        if node_matches(child, query):
            return True

    return False


def event_color(event: BaseEvent | None) -> tuple[float, float, float, float]:
    if event is None:
        return 0.7, 0.7, 0.7, 1.0
    if isinstance(event, ScopeStartEvent):
        return 0.42, 0.75, 0.31, 1.0
    if isinstance(event, ScopeEndEvent):
        return 0.8, 0.25, 0.15, 1.0
    if isinstance(event, InstantEvent):
        return 0.24, 0.52, 0.78, 1.0
    return 0.7, 0.7, 0.7, 1.0


def draw_metadata_table(metadata: dict[str, Any] | None, prefix: str) -> None:
    if not metadata:
        return

    if imgui.tree_node(f"metadata##{prefix}"):
        for key, value in metadata.items():
            if isinstance(value, dict):
                if imgui.tree_node(f"{key}##{prefix}_{key}"):
                    for subkey, subvalue in value.items():
                        imgui.bullet_text(
                            f"{subkey}: {json.dumps(subvalue, ensure_ascii=False)}"
                        )
                    imgui.tree_pop()
            else:
                imgui.bullet_text(
                    f"{key}: {json.dumps(value, ensure_ascii=False)}")
        imgui.tree_pop()


def draw_event_node(node: EventNode, query: str, path_key: str) -> None:
    if not node_matches(node, query):
        return

    color = event_color(node.event)
    has_children = len(node.children) > 0

    flags = 0
    if not has_children:
        flags |= imgui.TREE_NODE_LEAF

    opened = imgui.tree_node(f"{node.title}##{path_key}", flags)

    min_x, min_y = imgui.get_item_rect_min()
    max_x, max_y = imgui.get_item_rect_max()
    draw_list = imgui.get_window_draw_list()
    draw_list.add_rect_filled(min_x, min_y, min_x + 4, max_y,
                              imgui.get_color_u32_rgba(*color))

    if imgui.is_item_hovered() and node.event is not None:
        imgui.begin_tooltip()
        imgui.text_unformatted(event_details(node.event))
        imgui.end_tooltip()

    if opened:
        if node.event is not None:
            imgui.push_style_color(imgui.COLOR_TEXT, 0.75, 0.75, 0.75, 1.0)
            if node.event.file is not None or node.event.line is not None:
                location = node.event.file or "<unknown>"
                if node.event.line is not None:
                    location = f"{location}:{node.event.line}"
                imgui.text_unformatted(f"location: {location}")
            imgui.text_unformatted(f"id: {node.event.id}")
            imgui.text_unformatted(f"type: {node.event.event}")
            imgui.pop_style_color()

            draw_metadata_table(node.event.metadata, path_key)

        for index, child in enumerate(node.children):
            draw_event_node(child, query, f"{path_key}/{index}")

        imgui.tree_pop()


def main() -> int:
    base_dir = Path(__file__).resolve().parent
    default_jsonl = base_dir / "example_log.jsonl"
    ensure_example_file(default_jsonl)

    input_path = Path(sys.argv[1]) if len(sys.argv) > 1 else default_jsonl
    events = load_events(input_path)
    root = build_tree(events)

    if not glfw.init():
        raise RuntimeError("failed to initialize glfw")

    window = glfw.create_window(1400, 900, "Log Event Tree Viewer (ImGui)",
                                None, None)
    if not window:
        glfw.terminate()
        raise RuntimeError("failed to create window")

    glfw.make_context_current(window)
    glfw.swap_interval(1)

    imgui.create_context()
    impl = GlfwRenderer(window)

    search_value = ""
    search_buffer = search_value

    try:
        while not glfw.window_should_close(window):
            glfw.poll_events()
            impl.process_inputs()

            imgui.new_frame()

            imgui.set_next_window_position(0, 0)
            width, height = glfw.get_framebuffer_size(window)
            imgui.set_next_window_size(width, height)

            imgui.begin(
                "Log Events",
                closable=False,
                flags=imgui.WINDOW_NO_MOVE
                | imgui.WINDOW_NO_RESIZE
                | imgui.WINDOW_NO_COLLAPSE
                | imgui.WINDOW_NO_TITLE_BAR,
            )

            changed, search_buffer = imgui.input_text("Search / filter",
                                                      search_buffer, 512)
            if changed:
                search_value = search_buffer

            imgui.same_line()
            if imgui.button("Expand all"):
                imgui.set_next_item_open(True)

            imgui.same_line()
            if imgui.button("Collapse all"):
                pass

            imgui.separator()

            if imgui.begin_child("tree_region", 0, 0, border=False):
                for index, child in enumerate(root.children):
                    draw_event_node(child, search_value, str(index))
                imgui.end_child()

            imgui.end()

            glClearColor(0.1, 0.1, 0.1, 1.0)
            glClear(GL_COLOR_BUFFER_BIT)

            imgui.render()
            impl.render(imgui.get_draw_data())
            glfw.swap_buffers(window)
    finally:
        impl.shutdown()
        glfw.terminate()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

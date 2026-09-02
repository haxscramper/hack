from datetime import datetime, timedelta

from beartype import beartype
from PyQt6.QtGui import QTextDocument

from graphviz_viewer.task_graph_v1.task_graph_types import (
    Admonition,
    CalendarAllocation,
    EdgeKind,
    InputCollection,
    InputEdge,
    InputNode,
    NodeKind,
    Size,
    TodoState,
)

RICH_TEXT_WIDTH = 230.0


@beartype
def measure_rich_text(rich_text: str) -> Size:
    document = QTextDocument()
    document.setDocumentMargin(0.0)
    document.setHtml(rich_text)
    document.setTextWidth(RICH_TEXT_WIDTH)
    size = document.size()
    return Size(width=RICH_TEXT_WIDTH, height=max(28.0, size.height()))


@beartype
def make_node(
    unique_id: str,
    rich_text: str,
    lane: str,
    todo_state: TodoState | None = None,
    admonition: Admonition | None = None,
    kind: NodeKind = NodeKind.SUBTREE,
    effort_min: int | None = None,
    tags: list[tuple[str, ...]] | None = None,
    allocations: list[CalendarAllocation] | None = None,
) -> InputNode:
    return InputNode(
        unique_id=unique_id,
        kind=kind,
        rich_text=rich_text,
        content_size=measure_rich_text(rich_text),
        lane=lane,
        todo_state=todo_state,
        admonition=admonition,
        created=datetime(2026, 8, 31, 20, 0),
        tags=[] if tags is None else tags,
        effort_min=effort_min,
        calendar_allocations=[] if allocations is None else allocations,
    )


@beartype
def daily_allocations(
    start: datetime,
    duration_min: int,
    count: int,
) -> list[CalendarAllocation]:
    result: list[CalendarAllocation] = []
    for offset in range(count):
        allocation_start = start + timedelta(days=offset)
        result.append(
            CalendarAllocation(
                start=allocation_start,
                end=allocation_start + timedelta(minutes=duration_min),
            ))
    return result


@beartype
def collect_input() -> InputCollection:
    september_first = datetime(2026, 9, 1)

    nodes = [
        make_node(
            "haxorg",
            "<b>haxorg</b><br/>Org-mode parsing and visualization",
            "haxorg",
        ),
        make_node(
            "design-layout",
            "<b>Design task layout</b><br/>Define semantic placement stages.",
            "haxorg",
            TodoState.WIP,
            effort_min=240,
            tags=[("state", "active"), ("complexity", "modest_3")],
            allocations=[
                CalendarAllocation(
                    september_first.replace(hour=19),
                    september_first.replace(hour=21),
                ),
                CalendarAllocation(
                    september_first.replace(day=2, hour=19),
                    september_first.replace(day=2, hour=21),
                ),
            ],
        ),
        make_node(
            "graph-model",
            "<b>Implement graph model</b><br/>Represent groups, nodes and edges.",
            "haxorg",
            TodoState.NEXT,
            effort_min=180,
            tags=[("state", "can_take_next")],
        ),
        make_node(
            "graphviz-layout",
            "<b>Graphviz placement</b><br/>Preserve node and spline geometry.",
            "haxorg",
            TodoState.TODO,
            effort_min=240,
        ),
        make_node(
            "scene-rendering",
            "<b>Qt scene rendering</b><br/>Render rich task cards and edges.",
            "haxorg",
            TodoState.TODO,
            effort_min=300,
        ),
        make_node(
            "minimap",
            "<b>Interactive minimap</b><br/>Show viewport and support navigation.",
            "haxorg",
            TodoState.TODO,
            effort_min=120,
            tags=[("state", "consider_for_current")],
        ),
        make_node(
            "parser-warning",
            "<b>Warning:</b> malformed timestamp samples still need tests.",
            "haxorg",
            admonition=Admonition.WARNING,
            kind=NodeKind.PARAGRAPH,
        ),
        make_node(
            "layout-note",
            "<b>Layout note</b><br/>Cross-lane links must not affect ranks.",
            "haxorg",
            admonition=Admonition.NOTE,
            kind=NodeKind.PARAGRAPH,
        ),
        make_node(
            "haxdex",
            "<b>haxdex</b><br/>Personal indexing application",
            "haxdex",
        ),
        make_node(
            "index-schema",
            "<b>Index schema</b><br/>Define stored document metadata.",
            "haxdex",
            TodoState.WIP,
            effort_min=180,
            tags=[("state", "active")],
        ),
        make_node(
            "query-ui",
            "<b>Query interface</b><br/>Create interactive result filtering.",
            "haxdex",
            TodoState.TODO,
            effort_min=360,
        ),
        make_node(
            "result-preview",
            "<b>Result preview</b><br/>Render selected document excerpts.",
            "haxdex",
            TodoState.TODO,
            effort_min=180,
        ),
        make_node(
            "work-root",
            "<b>Work</b>",
            "work",
        ),
        make_node(
            "daily-work",
            "<b>Work</b>",
            "work",
            allocations=daily_allocations(
                september_first.replace(hour=9),
                8 * 60,
                5,
            ),
        ),
        make_node(
            "email",
            "<b>Check email inbox</b>",
            "work",
            TodoState.TODO,
            effort_min=10,
            allocations=daily_allocations(
                september_first.replace(hour=9),
                10,
                5,
            ),
        ),
        make_node(
            "standup",
            "<b>Team stand-up</b>",
            "work",
            allocations=daily_allocations(
                september_first.replace(hour=10),
                30,
                5,
            ),
        ),
        make_node(
            "report",
            "<b>Prepare weekly report</b>",
            "work",
            TodoState.NEXT,
            effort_min=120,
            allocations=[
                CalendarAllocation(
                    september_first.replace(day=4, hour=14),
                    september_first.replace(day=4, hour=16),
                )
            ],
        ),
        make_node(
            "chores-root",
            "<b>Chores</b>",
            "chores",
        ),
        make_node(
            "groceries",
            "<b>Buy groceries</b>",
            "chores",
            TodoState.TODO,
            effort_min=50,
            tags=[("state", "waiting_for_slot")],
            allocations=[
                CalendarAllocation(
                    september_first.replace(day=2, hour=18),
                    september_first.replace(day=2, hour=18, minute=50),
                )
            ],
        ),
        make_node(
            "clean-desk",
            "<b>Clean the desk</b>",
            "chores",
            TodoState.TODO,
            effort_min=20,
            tags=[("state", "slot_into_current")],
            allocations=[
                CalendarAllocation(
                    september_first.replace(day=3, hour=18),
                    september_first.replace(day=3, hour=18, minute=20),
                )
            ],
        ),
        make_node(
            "out-of-office",
            "<b>Out of office</b>",
            "personal",
            allocations=daily_allocations(
                september_first.replace(hour=17),
                60,
                5,
            ),
        ),
    ]

    edges = [
        InputEdge("nested-1", "haxorg", "design-layout", EdgeKind.NESTED),
        InputEdge("nested-2", "haxorg", "graph-model", EdgeKind.NESTED),
        InputEdge("nested-3", "haxorg", "graphviz-layout", EdgeKind.NESTED),
        InputEdge("nested-4", "haxorg", "scene-rendering", EdgeKind.NESTED),
        InputEdge("nested-5", "haxorg", "minimap", EdgeKind.NESTED),
        InputEdge("nested-6", "design-layout", "layout-note", EdgeKind.NESTED),
        InputEdge("nested-7", "graphviz-layout", "parser-warning",
                  EdgeKind.NESTED),
        InputEdge("block-1", "design-layout", "graph-model", EdgeKind.BLOCKED),
        InputEdge("block-2", "graph-model", "graphviz-layout",
                  EdgeKind.BLOCKED),
        InputEdge("block-3", "graphviz-layout", "scene-rendering",
                  EdgeKind.BLOCKED),
        InputEdge("block-4", "scene-rendering", "minimap", EdgeKind.BLOCKED),
        InputEdge("related-1", "layout-note", "graphviz-layout",
                  EdgeKind.RELATED),
        InputEdge("related-2", "parser-warning", "scene-rendering",
                  EdgeKind.RELATED),
        InputEdge("nested-8", "haxdex", "index-schema", EdgeKind.NESTED),
        InputEdge("nested-9", "haxdex", "query-ui", EdgeKind.NESTED),
        InputEdge("nested-10", "haxdex", "result-preview", EdgeKind.NESTED),
        InputEdge("block-5", "index-schema", "query-ui", EdgeKind.BLOCKED),
        InputEdge("block-6", "query-ui", "result-preview", EdgeKind.BLOCKED),
        InputEdge("related-3", "result-preview", "scene-rendering",
                  EdgeKind.RELATED),
        InputEdge("nested-11", "work-root", "email", EdgeKind.NESTED),
        InputEdge("nested-12", "work-root", "standup", EdgeKind.NESTED),
        InputEdge("nested-13", "work-root", "report", EdgeKind.NESTED),
        InputEdge("nested-14", "chores-root", "groceries", EdgeKind.NESTED),
        InputEdge("nested-15", "chores-root", "clean-desk", EdgeKind.NESTED),
    ]

    return InputCollection(nodes=nodes, edges=edges)

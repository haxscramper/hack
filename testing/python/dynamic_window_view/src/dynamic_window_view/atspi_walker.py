import subprocess
import json
from dataclasses import dataclass, field
from Xlib import X, display, Xatom
from Xlib.protocol.rq import Event

import gi

gi.require_version("Atspi", "2.0")
from gi.repository import Atspi


@dataclass
class Rect:
    x: int
    y: int
    width: int
    height: int


@dataclass
class SplitPane:
    rect: Rect
    role: str
    name: str
    children: list["SplitPane"] = field(default_factory=list)


@dataclass
class WindowInfo:
    wid: int
    title: str
    wm_class: str
    pid: int
    rect: Rect
    splits: list[SplitPane] = field(default_factory=list)
    screen: int = -1
    tags: list[str] = field(default_factory=list)
    visible_on_selected_tag: bool = False
    floating: bool = False
    maximized: bool = False
    minimized: bool = False
    fullscreen: bool = False
    urgent: bool = False
    hidden: bool = False
    ontop: bool = False
    sticky: bool = False
    above: bool = False
    below: bool = False


@dataclass
class TagInfo:
    name: str
    screen: int
    selected: bool
    activated: bool
    index: int
    layout: str
    master_count: int
    column_count: int
    master_width_factor: float
    gap: int
    volatile: bool
    windows: list[WindowInfo] = field(default_factory=list)


def get_x11_windows() -> list[WindowInfo]:
    d = display.Display()
    root = d.screen().root

    net_client_list = d.intern_atom("_NET_CLIENT_LIST")
    net_wm_name = d.intern_atom("_NET_WM_NAME")
    net_wm_pid = d.intern_atom("_NET_WM_PID")
    utf8_string = d.intern_atom("UTF8_STRING")

    resp = root.get_full_property(net_client_list, X.AnyPropertyType)
    if resp is None:
        return []

    windows = []
    for wid in resp.value:
        win = d.create_resource_object("window", wid)
        try:
            geom = win.get_geometry()
            # Translate to root coordinates
            coords = win.translate_coords(root, 0, 0)
            # coords.x, coords.y are the position of root's (0,0) in window coords
            # so window position in root coords is (-coords.x, -coords.y)
            abs_x = -coords.x
            abs_y = -coords.y
        except Exception:
            continue

        # Get title
        title = ""
        prop = win.get_full_property(net_wm_name, utf8_string)
        if prop:
            title = prop.value.decode("utf-8", errors="replace")
        else:
            prop = win.get_full_property(Xatom.WM_NAME, X.AnyPropertyType)
            if prop:
                title = prop.value.decode("latin-1", errors="replace")

        # Get WM_CLASS
        wm_class = ""
        prop = win.get_full_property(Xatom.WM_CLASS, X.AnyPropertyType)
        if prop:
            parts = prop.value.split(b"\x00")
            wm_class = parts[1].decode("utf-8", errors="replace") if len(
                parts) > 1 else parts[0].decode("utf-8", errors="replace")

        # Get PID
        pid = 0
        prop = win.get_full_property(net_wm_pid, X.AnyPropertyType)
        if prop:
            pid = int(prop.value[0])

        rect = Rect(x=abs_x, y=abs_y, width=geom.width, height=geom.height)
        windows.append(
            WindowInfo(
                wid=wid,
                title=title,
                wm_class=wm_class,
                pid=pid,
                rect=rect,
            ))

    d.close()
    return windows


def get_emacs_splits(win: WindowInfo) -> list[SplitPane]:
    """Use emacsclient to get precise split geometry."""
    elisp = """
    (let (result)
      (dolist (frame (frame-list))
        (when (eq (framep frame) 'x)
          (dolist (w (window-list frame nil))
            (let* ((edges (window-edges w t))
                   (buf (buffer-name (window-buffer w))))
              (push (list (nth 0 edges) (nth 1 edges)
                          (- (nth 2 edges) (nth 0 edges))
                          (- (nth 3 edges) (nth 1 edges))
                          buf)
                    result)))))
      (prin1-to-string result))
    """
    try:
        proc = subprocess.run(
            ["emacsclient", "--eval", elisp],
            capture_output=True,
            text=True,
            timeout=3,
        )
        if proc.returncode != 0:
            return []
        raw = proc.stdout.strip().strip('"').replace("\\\"", '"')
        # Parse the elisp output: ((x y w h "name") ...)
        # Simple parsing since format is predictable
        raw = raw.replace("\\n", "")
        splits = []
        # Use a rough parser for the sexp
        import re
        pane_re = re.compile(r'\((\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+"([^"]*)"\)')
        for m in pane_re.finditer(raw):
            x, y, w, h = int(m.group(1)), int(m.group(2)), int(
                m.group(3)), int(m.group(4))
            name = m.group(5)
            # These coordinates are relative to the frame (pixel coords with t arg)
            splits.append(
                SplitPane(
                    rect=Rect(x=win.rect.x + x,
                              y=win.rect.y + y,
                              width=w,
                              height=h),
                    role="emacs-window",
                    name=name,
                ))
        return splits
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return []


@dataclass
class AtspiNode:
    """Raw collected node from AT-SPI tree walk."""
    role: str
    role_enum: Atspi.Role
    name: str
    rect: Rect | None
    depth: int
    children: list["AtspiNode"] = field(default_factory=list)


def collect_atspi_tree(accessible, depth=0, max_depth=30) -> AtspiNode | None:
    """Collect the full AT-SPI tree without filtering."""
    if depth > max_depth:
        return None
    try:
        role = accessible.get_role()
        role_name = accessible.get_role_name()
        name = accessible.get_name() or ""
        n_children = accessible.get_child_count()
    except Exception:
        return None

    rect = None
    try:
        component = accessible.get_component_iface()
        if component:
            ext = component.get_extents(Atspi.CoordType.SCREEN)
            if ext.width > 0 and ext.height > 0:
                rect = Rect(x=ext.x,
                            y=ext.y,
                            width=ext.width,
                            height=ext.height)
    except Exception:
        pass

    children = []
    for i in range(n_children):
        try:
            child = accessible.get_child_at_index(i)
            if child:
                node = collect_atspi_tree(child, depth + 1, max_depth)
                if node:
                    children.append(node)
        except Exception:
            continue

    return AtspiNode(
        role=role_name,
        role_enum=role,
        name=name,
        rect=rect,
        depth=depth,
        children=children,
    )


def find_nodes(node: AtspiNode, predicate) -> list[AtspiNode]:
    """Find all nodes matching a predicate in the tree."""
    results = []
    if predicate(node):
        results.append(node)
    for child in node.children:
        results.extend(find_nodes(child, predicate))
    return results


def extract_vscode_splits(tree: AtspiNode) -> list[SplitPane]:
    """
    VSCode editor tabs have role 'page tab' with the file name as the title,
    grouped under 'page tab list' nodes. Each editor group is a separate tab list.
    We look for page tab lists, then find the active editor document within each group.
    """
    # Find all page tab lists — each represents an editor group
    tab_lists = find_nodes(tree,
                           lambda n: n.role_enum == Atspi.Role.PAGE_TAB_LIST)

    # Filter to only tab lists that contain tabs with file-like names
    # VSCode has multiple tab lists (sidebar, panel, etc.), so we filter by
    # looking for tabs that are siblings of document-web content
    editor_groups = []
    for tl in tab_lists:
        tabs = [c for c in tl.children if c.role_enum == Atspi.Role.PAGE_TAB]
        if not tabs:
            continue
        # Check if any sibling or nearby node is a document web — that indicates editor area
        # Simpler heuristic: tabs with names ending in common file extensions or
        # tabs that have a rect within the main editor area
        file_tabs = [
            t for t in tabs if "." in t.name and t.name not in ("", " ")
        ]
        if file_tabs:
            editor_groups.append((tl, file_tabs))

    if not editor_groups:
        # Fallback: look for document-web nodes with titles
        docs = find_nodes(
            tree, lambda n: (n.role_enum == Atspi.Role.DOCUMENT_WEB and n.name
                             and n.rect is not None))
        # Deduplicate by name, keep unique rects
        seen = set()
        splits = []
        for d in docs:
            key = (d.name, d.rect.x, d.rect.y, d.rect.width, d.rect.height)
            if key not in seen:
                seen.add(key)
                splits.append(SplitPane(rect=d.rect, role=d.role, name=d.name))
        return splits

    splits = []
    for tl, tabs in editor_groups:
        # The tab list rect represents the editor group area
        if tl.rect:
            # Find the currently active/focused tab
            active_tabs = [t for t in tabs if t.rect and t.rect.width > 0]
            # Each tab list = one split pane, list the tabs as children
            children = []
            for t in active_tabs:
                children.append(
                    SplitPane(
                        rect=t.rect,
                        role="editor-tab",
                        name=t.name,
                    ))
            splits.append(
                SplitPane(
                    rect=tl.rect,
                    role="editor-group",
                    name=f"{len(tabs)} tabs",
                    children=children,
                ))

    return splits


def extract_qtcreator_splits(tree: AtspiNode) -> list[SplitPane]:
    """
    Qt Creator editor splits: look for split panes that contain text editing areas.
    The actual editor content is typically a 'text' or 'edit bar' deep inside.
    We want the outermost split pane containers that hold distinct editor views.
    """
    # Strategy: find all 'text' or 'editable text' nodes with a reasonable size
    # that likely represent editor views
    editors = find_nodes(
        tree, lambda n:
        (n.role_enum in
         (Atspi.Role.TEXT, Atspi.Role.TERMINAL, Atspi.Role.DOCUMENT_TEXT, Atspi
          .Role.EDITBAR, Atspi.Role.ENTRY) and n.rect is not None and n.rect.
         width > 100 and n.rect.height > 100))

    if not editors:
        # Try to find split panes that have substantial size and are leaf-level
        all_splits = find_nodes(
            tree, lambda n:
            (n.role_enum == Atspi.Role.SPLIT_PANE and n.rect is not None and n.
             rect.width > 100 and n.rect.height > 100))
        # Keep only leaf split panes (no split pane children)
        leaf_splits = []
        for sp in all_splits:
            child_splits = find_nodes(
                sp,
                lambda n: n.role_enum == Atspi.Role.SPLIT_PANE and n is not sp)
            if not child_splits:
                leaf_splits.append(sp)

        return [
            SplitPane(rect=s.rect, role=s.role, name=s.name or "(editor)")
            for s in leaf_splits
        ]

    # Deduplicate editors with overlapping rects — keep distinct ones
    unique = []
    for e in editors:
        is_dup = False
        for u in unique:
            # Check if rects are substantially overlapping
            if (abs(e.rect.x - u.rect.x) < 20 and abs(e.rect.y - u.rect.y) < 20
                    and abs(e.rect.width - u.rect.width) < 40
                    and abs(e.rect.height - u.rect.height) < 40):
                is_dup = True
                break
        if not is_dup:
            unique.append(e)

    return [
        SplitPane(rect=e.rect, role=e.role, name=e.name or "(editor)")
        for e in unique
    ]


def extract_generic_splits(tree: AtspiNode) -> list[SplitPane]:
    """Generic fallback: find split panes and terminal/document nodes."""
    # First try split panes
    splits = find_nodes(
        tree, lambda n:
        (n.role_enum == Atspi.Role.SPLIT_PANE and n.rect is not None and n.rect
         .width > 50 and n.rect.height > 50))
    if splits:
        # Build hierarchy from the split pane structure
        results = []
        for s in splits:
            child_splits = []
            for c in s.children:
                sub = find_nodes(
                    c, lambda n: (n.role_enum == Atspi.Role.SPLIT_PANE and n.
                                  rect is not None))
                child_splits.extend(sub)
            if not child_splits:
                results.append(SplitPane(rect=s.rect, role=s.role,
                                         name=s.name))
        if results:
            return results

    # Fallback to terminal/document nodes
    leaves = find_nodes(
        tree, lambda n: (n.role_enum in (
            Atspi.Role.TERMINAL,
            Atspi.Role.DOCUMENT_FRAME,
            Atspi.Role.DOCUMENT_WEB,
            Atspi.Role.DOCUMENT_TEXT,
        ) and n.rect is not None and n.rect.width > 50 and n.rect.height > 50))
    return [SplitPane(rect=n.rect, role=n.role, name=n.name) for n in leaves]


def get_atspi_splits(win: WindowInfo) -> list[SplitPane]:
    """Walk AT-SPI tree for a given window to find split structure."""
    desktop = Atspi.get_desktop(0)
    n_apps = desktop.get_child_count()

    for i in range(n_apps):
        try:
            app = desktop.get_child_at_index(i)
            if not app:
                continue
        except Exception:
            continue

        try:
            app_pid = app.get_process_id()
        except Exception:
            continue

        if app_pid != win.pid:
            continue

        n_windows = app.get_child_count()
        for j in range(n_windows):
            try:
                atspi_win = app.get_child_at_index(j)
                if not atspi_win:
                    continue
                comp = atspi_win.get_component_iface()
                if comp:
                    ext = comp.get_extents(Atspi.CoordType.SCREEN)
                    if (abs(ext.x - win.rect.x) < 50
                            and abs(ext.y - win.rect.y) < 50):
                        tree = collect_atspi_tree(atspi_win)
                        if tree is None:
                            continue

                        wm = win.wm_class.lower()
                        if "code" in wm:
                            return extract_vscode_splits(tree)
                        elif "qtcreator" in wm:
                            return extract_qtcreator_splits(tree)
                        else:
                            return extract_generic_splits(tree)
            except Exception:
                continue

    return []


def populate_splits(win: WindowInfo) -> None:
    """Determine splits for a window using the best available method."""
    wm = win.wm_class.lower()

    if "emacs" in wm:
        splits = get_emacs_splits(win)
        if splits:
            win.splits = splits
            return

    # For VSCode, Qt Creator, and anything else: use AT-SPI
    win.splits = get_atspi_splits(win)

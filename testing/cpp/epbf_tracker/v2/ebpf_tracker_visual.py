#!/usr/bin/env python

import sys
from pathlib import Path
from collections import defaultdict
from google.protobuf.internal.decoder import _DecodeVarint32
import plotly.graph_objects as go
from pprint import pformat


import trace_event_pb2 as procmon
import logging

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s")
logging.root.setLevel(logging.NOTSET)
logging.basicConfig(level=logging.NOTSET)

def read_events(filepath: Path) -> list[procmon.Event]:
    events = []
    with open(filepath, "rb") as f:
        data = f.read()

    pos = 0
    while pos < len(data):
        msg_len, new_pos = _DecodeVarint32(data, pos)
        pos = new_pos
        msg_buf = data[pos:pos + msg_len]
        pos += msg_len

        event = procmon.Event()
        event.ParseFromString(msg_buf)
        events.append(event)

    logging.info(f"Event count {len(events)}")

    return events


def build_process_trees(events: list[procmon.Event]) -> dict[int, dict]:
    """Returns {root_shell_pid: {pid: process_info}}"""
    trees = defaultdict(dict)

    for event in events:
        if event.HasField("start"):
            start = event.start
            trees[start.root_shell_pid][start.pid] = {
                "pid": start.pid,
                "ppid": start.ppid,
                "comm": start.comm,
                "args": list(start.args),
                "start_ns": start.timestamp_ns,
                "end_ns": None,
                "exit_code": None,
                "children": [],
            }
        elif event.HasField("stop"):
            stop = event.stop
            for tree in trees.values():
                if stop.pid in tree:
                    tree[stop.pid]["end_ns"] = stop.timestamp_ns
                    tree[stop.pid]["exit_code"] = stop.exit_code
                    break

    # Build parent-child relationships
    for root_shell_pid, tree in trees.items():
        for pid, proc in tree.items():
            ppid = proc["ppid"]
            if ppid in tree and ppid != pid:
                tree[ppid]["children"].append(pid)

    return dict(trees)


def flatten_tree_dfs(tree: dict, root_pid: int) -> list[dict]:
    """DFS traversal to get vertical ordering, tracking depth."""
    result = []

    def dfs(pid: int, depth: int):
        if pid not in tree:
            return
        proc = tree[pid]
        proc["depth"] = depth
        result.append(proc)
        # Sort children by start time for consistent ordering
        children = sorted(proc["children"],
                          key=lambda p: tree[p]["start_ns"]
                          if p in tree else 0)
        for child_pid in children:
            dfs(child_pid, depth + 1)

    dfs(root_pid, 0)
    return result


def find_root_pid(tree: dict) -> int:
    """Find the root process (one whose ppid is not in the tree)."""
    pids = set(tree.keys())
    for pid, proc in tree.items():
        if proc["ppid"] not in pids:
            return pid
    # Fallback: return the one with earliest start
    return min(tree.keys(), key=lambda p: tree[p]["start_ns"])


def create_visualization(tree: dict, root_shell_pid: int, output_path: Path):
    root_pid = find_root_pid(tree)
    processes = flatten_tree_dfs(tree, root_pid)

    if not processes:
        return

    # Find time range and normalize to seconds from start
    min_time = min(p["start_ns"] for p in processes)
    max_time = max(p["end_ns"] or p["start_ns"] for p in processes)

    def to_seconds(ns: int) -> float:
        return (ns - min_time) / 1e9

    fig = go.Figure()

    # Color scale based on depth
    max_depth = max(p["depth"] for p in processes)

    shapes = []
    annotations = []

    for y_idx, proc in enumerate(processes):
        start_s = to_seconds(proc["start_ns"])
        end_s = to_seconds(
            proc["end_ns"]) if proc["end_ns"] else to_seconds(max_time)

        # Color by depth
        depth_ratio = proc["depth"] / (max_depth + 1)
        color = f"hsl({200 + depth_ratio * 60}, 70%, {50 + depth_ratio * 20}%)"

        # Border color based on exit code
        border_color = "red" if proc["exit_code"] and proc[
            "exit_code"] != 0 else "black"

        shapes.append(
            dict(
                type="rect",
                x0=start_s,
                x1=end_s,
                y0=y_idx - 0.4,
                y1=y_idx + 0.4,
                fillcolor=color,
                line=dict(color=border_color, width=1),
                layer="below",
            ))

        # Label: show comm and truncated args
        args_str = " ".join(proc["args"][:3])
        if len(proc["args"]) > 3:
            args_str += "..."
        label = f"{proc["comm"]}"
        if len(label) > 30:
            label = label[:27] + "..."

        duration_s = end_s - start_s

        annotations.append(
            dict(
                x=start_s,
                y=y_idx,
                text=label,
                showarrow=False,
                xanchor="left",
                font=dict(size=9),
            ))

    # Add invisible scatter for hover info
    hover_texts = []
    x_mids = []
    y_vals = []

    for y_idx, proc in enumerate(processes):
        start_s = to_seconds(proc["start_ns"])
        end_s = to_seconds(
            proc["end_ns"]) if proc["end_ns"] else to_seconds(max_time)
        duration_s = end_s - start_s

        args_display = " ".join(proc["args"])
        if len(args_display) > 200:
            args_display = args_display[:200] + "..."

        hover = (f"<b>{proc["comm"]}</b><br>"
                 f"PID: {proc["pid"]}<br>"
                 f"PPID: {proc["ppid"]}<br>"
                 f"Args: {args_display}<br>"
                 f"Duration: {duration_s:.3f}s<br>"
                 f"Exit code: {proc["exit_code"]}")

        hover_texts.append(hover)
        x_mids.append((start_s + end_s) / 2)
        y_vals.append(y_idx)

    fig.add_trace(
        go.Scatter(
            x=x_mids,
            y=y_vals,
            mode="markers",
            marker=dict(size=1, opacity=0),
            hoverinfo="text",
            hovertext=hover_texts,
        ))

    fig.update_layout(
        shapes=shapes,
        annotations=annotations,
        title=
        f"Process Tree (root_shell_pid={root_shell_pid}, {len(processes)} processes)",
        xaxis=dict(
            title="Time (seconds)",
            showgrid=True,
            zeroline=True,
        ),
        yaxis=dict(
            title="",
            showticklabels=False,
            showgrid=False,
            range=[-1, len(processes)],
        ),
        height=max(400,
                   len(processes) * 20),
        hovermode="closest",
    )

    fig.write_html(output_path)
    print(f"Written: {output_path}")


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <input.pb>", file=sys.stderr)
        sys.exit(1)

    input_path = Path(sys.argv[1])
    events = read_events(input_path)
    trees = build_process_trees(events)

    Path("/tmp/data.py").write_text(pformat(trees, width=120))

    output_dir = input_path.parent

    for root_shell_pid, tree in trees.items():
        output_path = output_dir / f"{input_path.stem}_tree_{root_shell_pid}.html"
        create_visualization(tree, root_shell_pid, output_path)


if __name__ == "__main__":
    main()

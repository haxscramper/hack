#!/usr/bin/env python
import argparse
import os
import shutil
import sys
from collections import defaultdict

from perfetto.trace_processor import TraceProcessor, TraceProcessorConfig

QUERY = """
SELECT d.id, d.parent_id, d.depth, d.ts, d.dur, d.name, root.id AS root_id
FROM slice root
CROSS JOIN descendant_slice(root.id) AS d
WHERE root.name = '{name}'
UNION ALL
SELECT root.id, root.parent_id, root.depth, root.ts, root.dur, root.name, root.id AS root_id
FROM slice root
WHERE root.name = '{name}'
ORDER BY root_id, ts, depth
"""


def load_rows_from_trace(trace_path, bin_path, root_name):
    if not os.path.isfile(bin_path):
        found = shutil.which(bin_path)
        if found is None:
            raise FileNotFoundError(
                f"trace_processor binary not found: '{bin_path}' is not a file and not on PATH"
            )
        bin_path = found

    config = TraceProcessorConfig(bin_path=bin_path)
    tp = TraceProcessor(trace=trace_path, config=config)
    rows = []
    for r in tp.query(QUERY.format(name=root_name)):
        rows.append({
            "id": r.id,
            "parent_id": r.parent_id,
            "depth": r.depth,
            "ts": r.ts,
            "dur": r.dur,
            "name": r.name,
            "root_id": r.root_id,
        })
    tp.close()
    return rows


def fmt_dur(ns):
    if ns is None or ns < 0:
        return "0 ns"
    if ns >= 1_000_000_000:
        return f"{ns / 1e9:.3f} s"
    if ns >= 1_000_000:
        return f"{ns / 1e6:.3f} ms"
    if ns >= 1_000:
        return f"{ns / 1e3:.3f} us"
    return f"{ns} ns"


def print_tree(rows, max_depth=None, max_count=None):
    by_root = defaultdict(list)
    all_descendant_ids = set()

    for r in rows:
        by_root[r["root_id"]].append(r)
        if r["id"] != r["root_id"]:
            all_descendant_ids.add(r["id"])

    # Identify independent top-level roots (roots that are not nested inside another root)
    top_level_root_ids = [
        rid for rid in by_root if rid not in all_descendant_ids
    ]

    # Sort the independent roots chronologically so the output matches the trace timeline
    root_ts_map = {
        rid: min((r["ts"] for r in by_root[rid] if r["id"] == rid), default=0)
        for rid in top_level_root_ids
    }
    top_level_root_ids.sort(key=lambda rid: root_ts_map[rid])

    class AggNode:

        def __init__(self, name, id_seq):
            self.name = name
            self.dur = 0
            self.children = {}
            self.id = id_seq

    id_counter = 0

    # Process each independent top-level region separately
    for top_root_id in top_level_root_ids:
        subtree = by_root[top_root_id]

        by_id = {r["id"]: r for r in subtree}
        children = defaultdict(list)

        top_root_node = None
        for r in subtree:
            if r["id"] == top_root_id:
                top_root_node = r
            pid = r["parent_id"]
            if pid in by_id and pid != r["id"]:
                children[pid].append(r)

        if not top_root_node:
            continue

        # Recursively aggregate matching call stacks within THIS specific top-level subtree
        def aggregate(orig_nodes):
            nonlocal id_counter
            if not orig_nodes:
                return None
            name = orig_nodes[0]["name"]
            node = AggNode(name, id_counter)
            id_counter += 1

            # Calculate sum of valid durations for this grouped level
            node.dur = sum(
                (n["dur"]
                 for n in orig_nodes if n["dur"] is not None and n["dur"] > 0),
                0)

            # Group all physical children of these instances by their function names
            child_nodes_by_name = defaultdict(list)
            for n in orig_nodes:
                for c in children[n["id"]]:
                    child_nodes_by_name[c["name"]].append(c)

            for c_name, c_nodes in child_nodes_by_name.items():
                node.children[c_name] = aggregate(c_nodes)

            return node

        # Aggregate starting strictly from the current top-level root
        agg_root = aggregate([top_root_node])

        # Determine which node ids are allowed to be printed based on max_count.
        all_nodes = []

        def flatten(node, parent):
            all_nodes.append((node, parent))
            for c in node.children.values():
                flatten(c, node)

        flatten(agg_root, None)

        allowed = None
        if max_count is not None:
            descendants = [n for n, p in all_nodes if n != agg_root]
            top = sorted(descendants, key=lambda x: x.dur,
                         reverse=True)[:max_count]
            allowed = {agg_root.id}
            parent_map = {n.id: p for n, p in all_nodes}
            # Include ancestors of each selected node to keep the tree connected.
            for node in top:
                cur = node
                while cur is not None and cur.id not in allowed:
                    allowed.add(cur.id)
                    cur = parent_map.get(cur.id)

        lines = []

        def collect(node, parent_dur, indent):
            if max_depth is not None and indent > max_depth:
                return
            if allowed is not None and node.id not in allowed:
                return

            pct_parent = (100.0 * node.dur /
                          parent_dur) if (parent_dur
                                          and parent_dur > 0) else float("nan")
            tree_text = "  " * indent + str(node.name or "<unknown>")
            lines.append((tree_text, pct_parent, node.dur))

            # Sort flame graph branches by duration (highest to lowest) for visual consistency
            sorted_children = sorted(node.children.values(),
                                     key=lambda x: x.dur,
                                     reverse=True)
            for c in sorted_children:
                collect(c, node.dur, indent + 1)

        collect(agg_root, 0, 0)

        if not lines:
            continue

        width = max(len(t) for t, _, _ in lines)
        print(f"\n=== {agg_root.name}  {fmt_dur(agg_root.dur)} ===")
        for tree_text, pct_parent, dur in lines:
            print(
                f"{tree_text.ljust(width)}  {pct_parent:5.1f}%  [{fmt_dur(dur)}]"
            )


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input", help="trace file (.json/.pb)")
    ap.add_argument("name", help="root slice name")
    ap.add_argument("--bin",
                    default="trace_processor",
                    help="path to trace_processor binary")
    ap.add_argument("--max-depth",
                    type=int,
                    default=None,
                    help="max relative nesting depth to print")
    ap.add_argument(
        "--max-count",
        type=int,
        default=None,
        help="max number of nested elements to show (top by duration)",
    )
    args = ap.parse_args()

    rows = load_rows_from_trace(args.input, args.bin, args.name)

    if not rows:
        print("No matching slices found.", file=sys.stderr)
        sys.exit(1)

    print_tree(rows, max_depth=args.max_depth, max_count=args.max_count)


if __name__ == "__main__":
    main()

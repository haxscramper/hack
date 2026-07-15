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
    if ns >= 1_000_000_000:
        return f"{ns / 1e9:.3f} s"
    if ns >= 1_000_000:
        return f"{ns / 1e6:.3f} ms"
    if ns >= 1_000:
        return f"{ns / 1e3:.3f} us"
    return f"{ns} ns"


def print_tree(rows, max_depth=None, max_count=None):
    by_root = defaultdict(list)
    for r in rows:
        by_root[r["root_id"]].append(r)

    for root_id, subtree in by_root.items():
        by_id = {r["id"]: r for r in subtree}
        children = defaultdict(list)
        for r in subtree:
            pid = r["parent_id"]
            if pid in by_id and pid != r["id"]:
                children[pid].append(r)

        root = by_id[root_id]

        # Determine which node ids are allowed to be printed.
        allowed = None
        if max_count is not None:
            descendants = [r for r in subtree if r["id"] != root_id]
            top = sorted(descendants, key=lambda x: x["dur"],
                         reverse=True)[:max_count]
            allowed = {root_id}
            # Include ancestors of each selected node to keep the tree connected.
            for node in top:
                cur = node
                while cur is not None and cur["id"] not in allowed:
                    allowed.add(cur["id"])
                    cur = by_id.get(cur["parent_id"])

        # First pass: collect (tree_text, pct_parent, dur) respecting limits.
        lines = []

        def collect(node, indent):
            if max_depth is not None and indent > max_depth:
                return
            if allowed is not None and node["id"] not in allowed:
                return
            parent = by_id.get(node["parent_id"])
            pct_parent = (100.0 * node["dur"] / parent["dur"] if
                          (parent and parent["dur"]) else float("nan"))
            tree_text = "  " * indent + node["name"]
            lines.append((tree_text, pct_parent, node["dur"]))
            for c in sorted(children[node["id"]], key=lambda x: x["ts"]):
                collect(c, indent + 1)

        collect(root, 0)

        # Second pass: align on the tree-formatted element.
        width = max(len(t) for t, _, _ in lines)
        print(f"\n=== {root['name']}  {fmt_dur(root['dur'])} ===")
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

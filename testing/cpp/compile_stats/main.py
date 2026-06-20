#!/usr/bin/env python

from __future__ import annotations

import argparse
from collections import defaultdict
from pathlib import Path

import duckdb
import plotly.graph_objects as go


def load_prefix_durations(db_path: str) -> list[tuple[int, list[str]]]:
    query = """
    SELECT
        e.dur,
        (
            SELECT list(s.value ORDER BY u.idx)
            FROM UNNEST(e.grouping_prefix) WITH ORDINALITY AS u(str_id, idx)
            JOIN string_ids AS s
                ON s.str_id = u.str_id
        ) AS grouping_prefix
    FROM events e
    LEFT JOIN event_parent ep
      ON ep.event_id = e.event_id
    LEFT JOIN events p
      ON p.event_id = ep.parent_id
     AND p.normal_json IS NOT NULL
    WHERE e.normal_json IS NOT NULL
      AND p.event_id IS NULL
      AND e.grouping_prefix IS NOT NULL;
    """
    with duckdb.connect(db_path, read_only=True) as conn:
        rows = conn.execute(query).fetchall()
    return rows


def build_hierarchical_sums(
    rows: list[tuple[int, list[str]]]
) -> tuple[dict[tuple[str, ...], float], float]:
    sums: dict[tuple[str, ...], float] = defaultdict(float)
    total = 0.0

    for dur, prefix in rows:
        if not prefix:
            continue
        total += float(dur)
        for i in range(1, len(prefix) + 1):
            sums[tuple(prefix[:i])] += float(dur)

    return sums, total


def make_treemap_html(
    prefix_sums: dict[tuple[str, ...], float],
    total: float,
    output_html: str,
) -> None:
    root_id = "__root__"

    ids = [root_id]
    labels = ["all"]
    parents = [""]
    values = [total]

    for path in sorted(prefix_sums.keys(), key=lambda p: (len(p), p)):
        node_id = " / ".join(path)
        parent_id = root_id if len(path) == 1 else " / ".join(path[:-1])

        ids.append(node_id)
        labels.append(path[-1])
        parents.append(parent_id)
        values.append(prefix_sums[path])

    fig = go.Figure(
        go.Treemap(
            ids=ids,
            labels=labels,
            parents=parents,
            values=values,
            branchvalues="total",
            hovertemplate="%{id}<br>sum(dur)=%{value}<extra></extra>",
        ))

    fig.update_layout(
        title="Sum of duration by grouping prefix",
        margin=dict(t=50, l=10, r=10, b=10),
    )

    fig.write_html(output_html, include_plotlyjs="cdn")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", default="/tmp/result.duckdb")
    parser.add_argument("--out", default="/tmp/prefix_duration_treemap.html")
    args = parser.parse_args()

    rows = load_prefix_durations(args.db)
    prefix_sums, total = build_hierarchical_sums(rows)

    if not prefix_sums:
        raise RuntimeError(
            "No rows with non-empty grouping_prefix were found.")

    output_path = Path(args.out)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    make_treemap_html(prefix_sums, total, str(output_path))


if __name__ == "__main__":
    main()

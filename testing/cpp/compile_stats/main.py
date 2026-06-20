#!/usr/bin/env python

from __future__ import annotations

import argparse
from pathlib import Path

import duckdb
import pandas as pd
import plotly.graph_objects as go


def humanize_us(value_us: float) -> str:
    value_us = int(round(value_us))
    if value_us < 1_000:
        return f"{value_us} µs"
    if value_us < 1_000_000:
        return f"{value_us / 1_000:.3f} ms"
    if value_us < 60_000_000:
        return f"{value_us / 1_000_000:.3f} s"
    if value_us < 3_600_000_000:
        return f"{value_us / 60_000_000:.3f} min"
    return f"{value_us / 3_600_000_000:.3f} h"


def load_events(db_path: str) -> pd.DataFrame:
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
        df = conn.execute(query).df()

    df = df.dropna(subset=["dur", "grouping_prefix"]).copy()
    df["dur"] = pd.to_numeric(df["dur"], errors="raise")
    df = df[df["grouping_prefix"].map(len) > 0].copy()
    return df


def make_treemap(df: pd.DataFrame) -> go.Figure:
    records: list[tuple[tuple[str, ...], float]] = []
    for dur, prefix in zip(df["dur"], df["grouping_prefix"], strict=True):
        parts = tuple(prefix)
        for i in range(1, len(parts) + 1):
            records.append((parts[:i], float(dur)))

    agg = (pd.DataFrame(records,
                        columns=["path", "dur_us"
                                 ]).groupby("path",
                                            as_index=False)["dur_us"].sum())

    agg["depth"] = agg["path"].map(len)
    agg = agg.sort_values(["depth", "path"], ascending=[True, True])

    root_id = "__root__"
    total_us = float(df["dur"].sum())

    ids = [root_id]
    labels = ["all"]
    parents = [""]
    values_sec = [total_us / 1_000_000.0]
    custom = [humanize_us(total_us)]

    for path, dur_us in zip(agg["path"], agg["dur_us"], strict=True):
        node_id = "::".join(path)
        parent_id = root_id if len(path) == 1 else " / ".join(path[:-1])

        ids.append(node_id)
        labels.append(path[-1])
        parents.append(parent_id)
        values_sec.append(float(dur_us) / 1_000_000.0)
        custom.append(humanize_us(float(dur_us)))

    fig = go.Figure(
        go.Treemap(
            ids=ids,
            labels=labels,
            parents=parents,
            values=values_sec,
            branchvalues="total",
            customdata=custom,
            hovertemplate="%{id}<br>duration=%{customdata}<extra></extra>",
        ))
    fig.update_layout(
        title="Duration by grouping prefix",
        margin=dict(t=60, l=10, r=10, b=10),
    )
    return fig


def make_top100(df: pd.DataFrame) -> go.Figure:
    leaf = df.copy()
    leaf["path"] = leaf["grouping_prefix"].map(tuple)

    top = (leaf.groupby("path", as_index=False)["dur"].sum().sort_values(
        "dur", ascending=False).head(100).copy())

    top["group"] = top["path"].map(lambda p: "::".join(p))
    top["dur_sec"] = top["dur"] / 1_000_000.0
    top["dur_human"] = top["dur"].map(humanize_us)

    fig = go.Figure(
        go.Bar(
            x=top["dur_sec"],
            y=top["group"],
            orientation="h",
            customdata=top["dur_human"],
            hovertemplate="%{y}<br>duration=%{customdata}<extra></extra>",
            text=top["dur_human"],
            textposition="outside",
        ))
    fig.update_layout(
        title="Top 100 most time-consuming groups",
        xaxis_title="Duration (seconds)",
        yaxis_title="Group",
        yaxis=dict(autorange="reversed"),
        margin=dict(t=60, l=10, r=10, b=10),
        height=max(900, 18 * len(top) + 140),
    )
    return fig


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", default="/tmp/result.duckdb")
    parser.add_argument(
        "--view",
        required=True,
        choices=["treemap", "top100"],
        help="Choose exactly one output type.",
    )
    parser.add_argument("--out", default="/tmp/result.html")
    args = parser.parse_args()

    df = load_events(args.db)
    if df.empty:
        raise RuntimeError("No qualifying rows found.")

    if args.view == "treemap":
        fig = make_treemap(df)
    else:
        fig = make_top100(df)

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.write_html(str(out_path), include_plotlyjs="cdn")


if __name__ == "__main__":
    main()

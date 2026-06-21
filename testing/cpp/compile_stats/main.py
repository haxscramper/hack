#!/usr/bin/env python

from __future__ import annotations

import argparse
from pathlib import Path

import dash
import duckdb
import pandas as pd
import os
import plotly.graph_objects as go
from dash import Input, Output, dcc, html


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
        e.file_id,
        f.abs_path,
        e.dur,
        (
            SELECT list(s.value ORDER BY u.idx)
            FROM UNNEST(e.grouping_prefix) WITH ORDINALITY AS u(str_id, idx)
            JOIN string_ids AS s
              ON s.str_id = u.str_id
        ) AS grouping_prefix
    FROM events e
    JOIN files f
      ON f.file_id = e.file_id
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

    df = df.dropna(
        subset=["file_id", "abs_path", "dur", "grouping_prefix"]).copy()
    df["dur"] = pd.to_numeric(df["dur"], errors="raise")
    df["file_id"] = pd.to_numeric(df["file_id"],
                                  errors="raise").astype("int64")
    df = df[df["grouping_prefix"].map(len) > 0].copy()
    return df


def make_treemap(df: pd.DataFrame, title: str) -> go.Figure:
    records: list[tuple[tuple[str, ...], float, int]] = []
    for dur, prefix in zip(df["dur"], df["grouping_prefix"], strict=True):
        parts = tuple(prefix)
        for i in range(1, len(parts) + 1):
            records.append((parts[:i], float(dur), 1))

    agg = (pd.DataFrame(records, columns=["path", "dur_us", "count"]).groupby(
        "path", as_index=False).agg(dur_us=("dur_us", "sum"),
                                    count=("count", "sum")))

    agg["depth"] = agg["path"].map(len)
    agg = agg.sort_values(["depth", "path"], ascending=[True, True])

    root_id = "__root__"
    total_us = float(df["dur"].sum())
    total_count = int(len(df))

    ids = [root_id]
    labels = ["all"]
    parents = [""]
    values_sec = [total_us / 1_000_000.0]
    custom = [[humanize_us(total_us), total_count]]

    for path, dur_us, count in zip(agg["path"],
                                   agg["dur_us"],
                                   agg["count"],
                                   strict=True):
        node_id = "::".join(path)
        parent_id = root_id if len(path) == 1 else "::".join(path[:-1])

        ids.append(node_id)
        labels.append(path[-1])
        parents.append(parent_id)
        values_sec.append(float(dur_us) / 1_000_000.0)
        custom.append([humanize_us(float(dur_us)), int(count)])

    fig = go.Figure(
        go.Treemap(
            ids=ids,
            labels=labels,
            parents=parents,
            values=values_sec,
            branchvalues="total",
            customdata=custom,
            hovertemplate=("%{id}<br>"
                           "duration=%{customdata[0]}<br>"
                           "values=%{customdata[1]}<extra></extra>"),
            texttemplate="%{label}<br>%{customdata[0]}<br>n=%{customdata[1]}",
        ))
    fig.update_layout(
        title=title,
        margin=dict(t=60, l=10, r=10, b=10),
    )
    return fig


def make_top100(df: pd.DataFrame) -> go.Figure:
    leaf = df.copy()
    leaf["path"] = leaf["grouping_prefix"].map(tuple)

    top = (leaf.groupby("path", as_index=False).agg(
        dur=("dur", "sum"),
        count=("dur", "size")).sort_values("dur",
                                           ascending=False).head(100).copy())

    top["group"] = top["path"].map(lambda p: "::".join(p))
    top["dur_sec"] = top["dur"] / 1_000_000.0
    top["dur_human"] = top["dur"].map(humanize_us)
    top["label"] = top.apply(
        lambda r: f'{r["dur_human"]} (n={int(r["count"])})', axis=1)

    fig = go.Figure(
        go.Bar(
            x=top["dur_sec"],
            y=top["group"],
            orientation="h",
            customdata=top[["dur_human", "count"]],
            hovertemplate=("%{y}<br>"
                           "duration=%{customdata[0]}<br>"
                           "values=%{customdata[1]}<extra></extra>"),
            text=top["label"],
            textposition="outside",
        ))
    fig.update_layout(
        title="Top 100 most time-consuming groups (all files)",
        xaxis_title="Duration (seconds)",
        yaxis_title="Group",
        yaxis=dict(autorange="reversed"),
        margin=dict(t=60, l=10, r=10, b=10),
        height=max(900, 18 * len(top) + 140),
    )
    return fig


def make_file_tree_treemap(file_items: pd.DataFrame,
                           common_root: Path) -> go.Figure:
    root_id = "__fs_root__"

    dir_totals: dict[tuple[str, ...], float] = {}
    file_nodes: list[tuple[tuple[str, ...], float]] = []

    for row in file_items.itertuples(index=False):
        rel_parts = Path(row.abs_path).relative_to(common_root).parts
        dur_us = float(row.top_event_duration_us)
        file_nodes.append((rel_parts, dur_us))

        for i in range(1, len(rel_parts)):
            key = rel_parts[:i]
            dir_totals[key] = dir_totals.get(key, 0.0) + dur_us

    ids = [root_id]
    labels = ["all files"]
    parents = [""]
    values_sec = [
        float(file_items["top_event_duration_us"].sum()) / 1_000_000.0
    ]
    custom = [[humanize_us(float(file_items["top_event_duration_us"].sum()))]]

    for path, dur_us in sorted(dir_totals.items(),
                               key=lambda it: (len(it[0]), it[0])):
        node_id = f"dir::{ '::'.join(path) }"
        parent_id = root_id if len(
            path) == 1 else f"dir::{ '::'.join(path[:-1]) }"
        ids.append(node_id)
        labels.append(f"{path[-1]}/")
        parents.append(parent_id)
        values_sec.append(dur_us / 1_000_000.0)
        custom.append([humanize_us(dur_us)])

    for rel_parts, dur_us in file_nodes:
        node_id = f"file::{ '::'.join(rel_parts) }"
        parent_id = root_id if len(
            rel_parts) == 1 else f"dir::{ '::'.join(rel_parts[:-1]) }"
        ids.append(node_id)
        labels.append(rel_parts[-1])
        parents.append(parent_id)
        values_sec.append(dur_us / 1_000_000.0)
        custom.append([humanize_us(dur_us)])

    fig = go.Figure(
        go.Treemap(
            ids=ids,
            labels=labels,
            parents=parents,
            values=values_sec,
            branchvalues="total",
            customdata=custom,
            hovertemplate="%{id}<br>duration=%{customdata[0]}<extra></extra>",
            texttemplate="%{label}<br>%{customdata[0]}",
        ))
    fig.update_layout(
        title="File/directory build time treemap",
        margin=dict(t=60, l=10, r=10, b=10),
    )
    return fig


def load_file_build_times(db_path: str) -> pd.DataFrame:
    query = """
    SELECT
        f.file_id,
        f.abs_path,
        COALESCE(SUM(e.dur), 0) AS top_event_duration_us
    FROM files AS f
    LEFT JOIN events AS e
        ON e.file_id = f.file_id
    LEFT JOIN event_parent AS ep
        ON ep.event_id = e.event_id
    WHERE ep.parent_id IS NULL
      AND EXISTS (
          SELECT 1
          FROM event_nested AS en
          WHERE en.parent_id = e.event_id
      )
    GROUP BY f.file_id, f.abs_path;
    """
    with duckdb.connect(db_path, read_only=True) as conn:
        df = conn.execute(query).df()

    df = df.dropna(
        subset=["file_id", "abs_path", "top_event_duration_us"]).copy()
    df["file_id"] = pd.to_numeric(df["file_id"],
                                  errors="raise").astype("int64")
    df["top_event_duration_us"] = pd.to_numeric(
        df["top_event_duration_us"], errors="raise").astype("float64")
    return df


def build_file_tree(file_items: pd.DataFrame) -> tuple[Path, dict]:
    paths = file_items["abs_path"].tolist()
    common_root = Path(os.path.commonpath(paths))

    tree = {"total_us": 0.0, "dirs": {}, "files": {}}

    for row in file_items.itertuples(index=False):
        rel_parts = Path(row.abs_path).relative_to(common_root).parts
        dur_us = float(row.top_event_duration_us)

        node = tree
        node["total_us"] += dur_us

        for part in rel_parts[:-1]:
            child = node["dirs"].setdefault(part, {
                "total_us": 0.0,
                "dirs": {},
                "files": {}
            })
            child["total_us"] += dur_us
            node = child

        node["files"][rel_parts[-1]] = {
            "file_id": int(row.file_id),
            "dur_us": dur_us,
        }

    return common_root, tree


def render_file_tree(node: dict) -> html.Ul:
    items: list[html.Li] = []

    for dirname, child in sorted(
            node["dirs"].items(),
            key=lambda it: (-it[1]["total_us"], it[0]),
    ):
        items.append(
            html.Li([
                html.Span(f"{dirname}/ ({humanize_us(child['total_us'])})"),
                render_file_tree(child),
            ]))

    for filename, meta in sorted(
            node["files"].items(),
            key=lambda it: (-it[1]["dur_us"], it[0]),
    ):
        items.append(
            html.Li(
                dcc.Link(
                    f"{filename} ({humanize_us(meta['dur_us'])})",
                    href=f"/file/{meta['file_id']}",
                )))

    return html.Ul(items)


def build_app(db_path: str) -> dash.Dash:
    df = load_events(db_path)
    if df.empty:
        raise RuntimeError("No qualifying rows found.")

    file_times = load_file_build_times(db_path)

    overall_treemap = make_treemap(df,
                                   "Duration by grouping prefix (all files)")
    overall_top100 = make_top100(df)

    file_items = (df[["file_id", "abs_path"]].drop_duplicates().merge(
        file_times[["file_id", "top_event_duration_us"]],
        on="file_id",
        how="left").fillna({
            "top_event_duration_us": 0.0
        }).sort_values("abs_path", ascending=True).reset_index(drop=True))

    common_root, file_tree = build_file_tree(file_items)
    file_time_treemap = make_file_tree_treemap(file_items, common_root)

    file_figures: dict[int, go.Figure] = {}
    for file_id, file_df in df.groupby("file_id", sort=False):
        abs_path = file_df["abs_path"].iloc[0]
        file_figures[int(file_id)] = make_treemap(
            file_df, f"Duration by grouping prefix: {abs_path}")

    app = dash.Dash(__name__)
    app.title = "Event Grouping Explorer"

    app.layout = html.Div(
        [
            dcc.Location(id="url", refresh=False),
            html.Div(id="page-content"),
        ],
        style={
            "maxWidth": "1600px",
            "margin": "0 auto",
            "padding": "12px"
        },
    )

    def home_layout() -> html.Div:
        return html.Div([
            html.H2("Grouped views"),
            dcc.Graph(figure=overall_treemap, style={"height": "820px"}),
            dcc.Graph(figure=overall_top100),
            dcc.Graph(figure=file_time_treemap, style={"height": "820px"}),
            html.H3("Per-file treemap pages"),
            html.Div(f"Common prefix dropped: {common_root}"),
            render_file_tree(file_tree),
        ])

    def file_layout(file_id: int) -> html.Div:
        fig = file_figures[file_id]
        abs_path = file_items.loc[file_items["file_id"] == file_id,
                                  "abs_path"].iloc[0]
        return html.Div([
            dcc.Link("← Back to home", href="/"),
            html.H3(abs_path),
            dcc.Graph(figure=fig, style={"height": "900px"}),
        ])

    @app.callback(Output("page-content", "children"), Input("url", "pathname"))
    def route(pathname: str) -> html.Div:
        if pathname is None or pathname == "/":
            return home_layout()

        parts = [p for p in pathname.split("/") if p]
        if len(parts) == 2 and parts[0] == "file" and parts[1].isdigit():
            file_id = int(parts[1])
            if file_id in file_figures:
                return file_layout(file_id)

        return html.Div([
            dcc.Link("← Back to home", href="/"),
            html.H3("Page not found"),
        ])

    return app


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", default="/tmp/result.duckdb")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8050)
    parser.add_argument("--debug", action="store_true")
    args = parser.parse_args()

    app = build_app(args.db)
    app.run(host=args.host, port=args.port, debug=args.debug)


if __name__ == "__main__":
    main()

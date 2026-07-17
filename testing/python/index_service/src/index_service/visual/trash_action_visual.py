#!/usr/bin/env python
from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any
import os

import plotly.graph_objects as go

from index_service.gui.file_tree.query_filter import TrashAction
from index_service.services.pydantic_utils import model_from_json_data
from pydantic import BaseModel


@dataclass
class TreemapNode:
    id: str
    parent: str
    label: str
    is_file: bool
    count: int = 0
    full_path: str = ""
    duplicate_hash: str = ""
    duplicate_matches: int = 0


def iter_files(node: Any):
    if getattr(node, "is_directory", False):
        for child in getattr(node, "nested", []):
            yield from iter_files(child)
    else:
        yield node


def duplicate_info(file_node: Any) -> tuple[str, int]:
    columns = getattr(file_node, "columns", {}) or {}
    dup = columns.get("file_duplicate")
    if dup is None:
        return "", 0

    dup_hash = getattr(dup, "hash", "") or ""
    matches = getattr(dup, "matches", []) or []
    return dup_hash, len(matches)


def load_actions(jsonl_path: Path) -> list[Any]:
    actions: list[Any] = []
    with jsonl_path.open("r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            data = json.loads(line)
            actions.append(model_from_json_data(data, TrashAction))
    return actions


def build_treemap_nodes(actions: list[Any]) -> dict[str, TreemapNode]:
    files: list[Path] = []
    file_meta: dict[Path, tuple[str, int]] = {}

    for action in actions:
        for file_node in iter_files(action.file):
            p = Path(getattr(file_node, "path"))
            files.append(p)
            file_meta[p] = duplicate_info(file_node)

    if not files:
        return {}

    common_root = Path(Path(files[0]).anchor)
    if len(files) > 1:
        common_root = Path(Path(*Path(os.path.commonpath([str(p) for p in files])).parts))

    nodes: dict[str, TreemapNode] = {}
    root_id = "__root__"
    nodes[root_id] = TreemapNode(
        id=root_id,
        parent="",
        label=str(common_root),
        is_file=False,
        count=0,
        full_path=str(common_root),
    )

    for p in files:
        rel = p.relative_to(common_root)
        parts = rel.parts

        parent_id = root_id
        nodes[root_id].count += 1

        for i, part in enumerate(parts):
            is_file = i == len(parts) - 1
            node_id = "/".join(parts[:i + 1])

            if node_id not in nodes:
                nodes[node_id] = TreemapNode(
                    id=node_id,
                    parent=parent_id,
                    label=part,
                    is_file=is_file,
                )

            nodes[node_id].count += 1
            nodes[node_id].is_file = is_file

            if is_file:
                dup_hash, dup_matches = file_meta[p]
                nodes[node_id].full_path = str(p)
                nodes[node_id].duplicate_hash = dup_hash
                nodes[node_id].duplicate_matches = dup_matches

            parent_id = node_id

    return nodes


def plot_trash_treemap(nodes: dict[str, TreemapNode]) -> go.Figure:
    if not nodes:
        raise RuntimeError("No files to visualize.")

    ids: list[str] = []
    labels: list[str] = []
    parents: list[str] = []
    values: list[int] = []
    colors: list[str] = []
    customdata: list[list[Any]] = []

    for node in nodes.values():
        ids.append(node.id)
        labels.append(node.label)
        parents.append(node.parent)
        values.append(node.count)

        if node.is_file:
            if node.duplicate_matches > 1:
                colors.append("#ef4444")  # duplicate
            else:
                colors.append("#22c55e")  # unique
        else:
            colors.append("#334155")  # directory

        customdata.append([
            node.full_path,
            node.duplicate_hash,
            node.duplicate_matches,
            "file" if node.is_file else "directory",
        ])

    fig = go.Figure(
        go.Treemap(
            ids=ids,
            labels=labels,
            parents=parents,
            values=values,
            branchvalues="total",
            marker={"colors": colors},
            customdata=customdata,
            hovertemplate=("name: %{label}<br>"
                           "type: %{customdata[3]}<br>"
                           "path: %{customdata[0]}<br>"
                           "duplicate hash: %{customdata[1]}<br>"
                           "match count: %{customdata[2]}<extra></extra>"),
        ))

    fig.update_layout(
        title="Files scheduled for removal",
        margin={
            "t": 40,
            "l": 10,
            "r": 10,
            "b": 10
        },
    )
    return fig


class TrashActionVisualConfig(BaseModel, extra="forbid"):
    json_path: Path
    out_path: Path


def visualize_trash_actions(conf: TrashActionVisualConfig) -> None:
    actions = load_actions(conf.json_path)
    nodes = build_treemap_nodes(actions)
    fig = plot_trash_treemap(nodes)

    conf.out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.write_html(str(conf.out_path), include_plotlyjs="cdn", full_html=True)

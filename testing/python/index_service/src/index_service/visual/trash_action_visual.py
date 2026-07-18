#!/usr/bin/env python
from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from coloraide import Color

from index_service.gui.file_tree.query_filter import TrashAction
from index_service.services.pydantic_utils import model_from_json_data
from pydantic import BaseModel


@dataclass
class DirStats:
    path: str
    name: str
    total_files: int = 0
    deleted_files: int = 0
    children: dict[str, "DirStats"] = field(default_factory=dict)


def iter_files(node: Any):
    if getattr(node, "is_directory", False):
        for child in getattr(node, "nested", []):
            yield from iter_files(child)
    else:
        yield node


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


def normalize_under_root(root_dir: Path, raw_path: str) -> Path | None:
    p = Path(raw_path)
    candidate = (root_dir / p).resolve(
        strict=False) if not p.is_absolute() else p.resolve(strict=False)
    if candidate.is_relative_to(root_dir):
        return candidate
    return None


def collect_deleted_files(actions: list[Any], root_dir: Path) -> set[str]:
    deleted: set[str] = set()
    for action in actions:
        for file_node in iter_files(action.file):
            raw_path = getattr(file_node, "path", "")
            if not raw_path:
                continue
            normalized = normalize_under_root(root_dir, raw_path)
            if normalized is None:
                continue
            deleted.add(normalized.relative_to(root_dir).as_posix())
    return deleted


def collect_all_files(root_dir: Path) -> set[str]:
    return {
        p.relative_to(root_dir).as_posix() for p in root_dir.rglob("*") if p.is_file()
    }


def ensure_dir(stats: dict[str, DirStats], dir_key: str) -> None:
    if dir_key in stats:
        return
    name = Path(dir_key).name if dir_key else root_name_placeholder(stats)
    stats[dir_key] = DirStats(path=dir_key, name=name)


def root_name_placeholder(stats: dict[str, DirStats]) -> str:
    root = stats.get("")
    if root is not None:
        return root.name
    return "/"


def build_directory_stats(root_dir: Path, all_files: set[str],
                          deleted_files: set[str]) -> dict[str, DirStats]:
    stats: dict[str, DirStats] = {
        "": DirStats(path="", name=root_dir.name or str(root_dir))
    }

    def directory_chain(file_rel: str) -> list[str]:
        parts = Path(file_rel).parts
        dirs = [""]
        for i in range(len(parts) - 1):
            dirs.append("/".join(parts[:i + 1]))
        return dirs

    for rel_file in all_files:
        for dir_key in directory_chain(rel_file):
            ensure_dir(stats, dir_key)
            stats[dir_key].total_files += 1

    for rel_file in deleted_files:
        for dir_key in directory_chain(rel_file):
            ensure_dir(stats, dir_key)
            stats[dir_key].deleted_files += 1

    for key in list(stats.keys()):
        if key == "":
            continue
        parent = str(Path(key).parent).replace("\\", "/")
        if parent == ".":
            parent = ""
        ensure_dir(stats, parent)
        stats[parent].children[key] = stats[key]

    return stats


_YELLOW_TO_RED = Color.interpolate(["#eab308", "#ef4444"], space="srgb")


def deletion_color(total_files: int, deleted_files: int) -> str:
    if deleted_files == 0:
        return "#22c55e"

    if total_files <= 0:
        return "#ef4444"

    ratio = max(0.0, min(1.0, deleted_files / total_files))

    steps = 32
    step_index = round(ratio * (steps - 1))
    q_ratio = step_index / (steps - 1)

    return _YELLOW_TO_RED(q_ratio).convert("srgb").to_string(hex=True)


def to_plotly_data(root: DirStats, full_root: Path) -> dict[str, list[Any]]:
    data: dict[str, list[Any]] = {
        "ids": [],
        "labels": [],
        "parents": [],
        "values": [],
        "colors": [],
        "customdata": [],
    }

    def append_directory(node: DirStats, parent_id: str | None) -> None:
        node_id = f"dir:{node.path}"
        full_path = full_root if node.path == "" else full_root / node.path
        ratio = ((node.deleted_files / node.total_files) *
                 100.0 if node.total_files > 0 else 0.0)

        data["ids"].append(node_id)
        data["labels"].append(node.name)
        data["parents"].append(parent_id or "")
        data["values"].append(node.total_files)
        data["colors"].append(deletion_color(node.total_files, node.deleted_files))
        data["customdata"].append([
            str(full_path),
            node.deleted_files,
            node.total_files,
            round(ratio, 2),
        ])

        for child in sorted(node.children.values(), key=lambda item: item.name):
            append_directory(child, node_id)

    append_directory(root, None)
    return data


def build_plotly_html(root_dir: Path, tree_data: dict[str, list[Any]]) -> str:
    tree_data_json = json.dumps(tree_data, ensure_ascii=False)
    title_json = json.dumps(
        f"Root deletion view: {root_dir}",
        ensure_ascii=False,
    )

    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Trash actions root deletion</title>
  <script src="https://cdn.plot.ly/plotly-3.1.0.min.js"></script>
  <style>
    html, body {{
      margin: 0;
      width: 100%;
      height: 100%;
      background: #0b1020;
      overflow: hidden;
    }}

    #chart {{
      width: 100%;
      height: 100%;
    }}
  </style>
</head>
<body>
  <div id="chart"></div>
  <script>
    const treeData = {tree_data_json};

    const trace = {{
      type: 'treemap',
      ids: treeData.ids,
      labels: treeData.labels,
      parents: treeData.parents,
      values: treeData.values,
      branchvalues: 'total',
      sort: false,
      marker: {{
        colors: treeData.colors,
        line: {{
          color: '#0b1020',
          width: 1
        }}
      }},
      customdata: treeData.customdata,
      textinfo: 'label',
      textfont: {{
        color: '#e5e7eb'
      }},
      hovertemplate:
        'path: %{{customdata[0]}}<br>' +
        'deleted: %{{customdata[1]}} / %{{customdata[2]}}<br>' +
        'ratio: %{{customdata[3]}}%<extra></extra>',
      pathbar: {{
        visible: true,
        textfont: {{
          color: '#e5e7eb'
        }}
      }},
      tiling: {{
        packing: 'squarify'
      }}
    }};

    const layout = {{
      title: {{
        text: {title_json},
        font: {{
          color: '#e5e7eb'
        }}
      }},
      paper_bgcolor: '#0b1020',
      plot_bgcolor: '#0b1020',
      margin: {{
        top: 50,
        right: 0,
        bottom: 0,
        left: 0
      }},
      autosize: true
    }};

    Plotly.newPlot(
      'chart',
      [trace],
      layout,
      {{
        responsive: true,
        displaylogo: false
      }}
    );
  </script>
</body>
</html>
"""


class TrashActionVisualConfig(BaseModel, extra="forbid"):
    json_path: Path
    root_dir: Path
    out_path: Path


def visualize_trash_actions(conf: TrashActionVisualConfig) -> None:
    root_dir = conf.root_dir.resolve(strict=True)
    actions = load_actions(conf.json_path)

    all_files = collect_all_files(root_dir)
    deleted_files = collect_deleted_files(actions, root_dir)
    deleted_files &= all_files

    stats = build_directory_stats(root_dir, all_files, deleted_files)
    tree_data = to_plotly_data(stats[""], root_dir)

    html = build_plotly_html(root_dir, tree_data)
    conf.out_path.parent.mkdir(parents=True, exist_ok=True)
    conf.out_path.write_text(html, encoding="utf-8")

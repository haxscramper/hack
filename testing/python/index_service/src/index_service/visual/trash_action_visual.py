#!/usr/bin/env python
from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

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


def deletion_color(total_files: int, deleted_files: int) -> str:
    if total_files > 0 and deleted_files >= total_files:
        return "#ef4444"  # red
    if total_files > 0 and (deleted_files / total_files) >= 0.5:
        return "#f97316"  # orange
    if deleted_files > 0:
        return "#eab308"  # yellow
    return "#22c55e"  # green


def to_echarts_node(node: DirStats, full_root: Path) -> dict[str, Any]:
    ratio = 0.0
    if node.total_files > 0:
        ratio = (node.deleted_files / node.total_files) * 100.0

    full_path = str(full_root if node.path == "" else full_root / Path(node.path))
    children = [
        to_echarts_node(child, full_root)
        for child in sorted(node.children.values(), key=lambda x: x.name)
    ]

    result: dict[str, Any] = {
        "name": node.name,
        "value": max(node.total_files, 1),
        "children": children,
        "itemStyle": {
            "color": deletion_color(node.total_files, node.deleted_files)
        },
        "deletedFiles": node.deleted_files,
        "totalFiles": node.total_files,
        "ratio": round(ratio, 2),
        "fullPath": full_path,
    }
    return result


def build_echarts_html(root_dir: Path, tree_data: dict[str, Any]) -> str:
    option = {
        "title": {
            "text": f"Root deletion view: {root_dir}",
            "left": "center"
        },
        "tooltip": {
            "formatter": (
                "function(info) {"
                "  var d = info.data || {};"
                "  return 'path: ' + (d.fullPath || '') + '<br/>' +"
                "         'deleted: ' + (d.deletedFiles ?? 0) + ' / ' + (d.totalFiles ?? 0) + '<br/>' +"
                "         'ratio: ' + (d.ratio ?? 0) + '%';"
                "}")
        },
        "series": [{
            "type": "treemap",
            "data": [tree_data],
            "leafDepth": 3,
            "roam": True,
            "nodeClick": "zoomToNode",
            "breadcrumb": {
                "show": True
            },
            "label": {
                "show": True,
                "formatter": "{b}"
            },
        }],
    }

    option_json = json.dumps(option, ensure_ascii=False)
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Trash actions root deletion</title>
  <script src="https://cdn.jsdelivr.net/npm/echarts@5/dist/echarts.min.js"></script>
  <style>
    html, body {{
      margin: 0;
      padding: 0;
      width: 100%;
      height: 100%;
      background: #0b1020;
      color: #e5e7eb;
      font-family: sans-serif;
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
    const chart = echarts.init(document.getElementById('chart'));
    const option = {option_json};
    chart.setOption(option);
    window.addEventListener('resize', () => chart.resize());
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
    root_node = to_echarts_node(stats[""], root_dir)

    html = build_echarts_html(root_dir, root_node)
    conf.out_path.parent.mkdir(parents=True, exist_ok=True)
    conf.out_path.write_text(html, encoding="utf-8")

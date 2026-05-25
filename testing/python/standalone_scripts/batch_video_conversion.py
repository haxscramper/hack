#!/usr/bin/env python

from __future__ import annotations
import json
import logging
import queue
import sys
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from pathlib import Path
from typing import Any, Literal
import subprocess
import time

import click
from beartype import beartype
from plumbum import local
from plumbum.commands.processes import ProcessExecutionError
from pydantic import BaseModel, Field
from PySide6.QtCore import QAbstractItemModel, QModelIndex, QObject, QSortFilterProxyModel, Qt, Signal, Slot
from PySide6.QtGui import QMouseEvent
from PySide6.QtWidgets import (
    QApplication,
    QComboBox,
    QHeaderView,
    QHBoxLayout,
    QPushButton,
    QSpinBox,
    QStyledItemDelegate,
    QStyle,
    QStyleOptionViewItem,
    QTreeView,
    QVBoxLayout,
    QWidget,
)

LOGGER = logging.getLogger("video_manager")
VIDEO_EXTENSIONS = {
    ".mp4",
    ".mkv",
    ".mov",
    ".avi",
    ".webm",
    ".m4v",
    ".flv",
    ".wmv",
    ".mpg",
    ".mpeg",
    ".ts",
    ".mts",
    ".m2ts",
    ".3gp",
}
RESOLUTION_HEIGHT: dict[str, int] = {"SD": 480, "HD": 720, "FULL_HD": 1080}


@beartype
class ProbeInfoModel(BaseModel):
    duration_seconds: float | None = None
    bitrate_bps: int | None = None
    width: int | None = None
    height: int | None = None
    resolution: str | None = None
    codec_name: str | None = None
    format_name: str | None = None


@beartype
class ActionModel(BaseModel):
    target_resolution: Literal["", "SD", "HD", "FULL_HD"] = ""
    crf: int = 0


@beartype
class OutputVideoModel(BaseModel):
    output_path: str
    file_size_bytes: int
    created_ts: float
    probe: ProbeInfoModel | None = None
    action: ActionModel = Field(default_factory=ActionModel)
    preset: str = "medium"


@beartype
class TreeEntryModel(BaseModel):
    name: str
    absolute_path: str
    relative_path: str
    is_dir: bool
    row_in_parent: int
    entires: list["TreeEntryModel"] = Field(default_factory=list)
    file_size_bytes: int = 0
    created_ts: float = 0.0
    share: float = 0.0
    probe: ProbeInfoModel | None = None
    action: ActionModel = Field(default_factory=ActionModel)
    output: OutputVideoModel | None = None
    created_text: str = ""
    size_text: str = ""
    share_text: str = ""
    duration_text: str = ""
    bitrate_text: str = ""
    resolution_text: str = ""
    codec_text: str = ""


@beartype
class CacheModel(BaseModel):
    input_root: str
    generated_ts: float
    root_entry: TreeEntryModel


@beartype
class ProbeTaskResultModel(BaseModel):
    absolute_path: str
    probe: ProbeInfoModel | None = None


@beartype
class ConversionTaskModel(BaseModel):
    source_path: str
    relative_path: str
    output_path: str
    action: ActionModel
    preset: str


@beartype
class ConversionResultModel(BaseModel):
    source_path: str
    output: OutputVideoModel


TreeEntryModel.model_rebuild()


@beartype
def configure_logging() -> None:
    handler = logging.StreamHandler()
    handler.setFormatter(
        logging.Formatter(
            "%(levelname)s - %(name)s - %(filename)s:%(lineno)d: %(message)s"))
    LOGGER.setLevel(logging.DEBUG)
    LOGGER.handlers.clear()
    LOGGER.addHandler(handler)


@beartype
def format_size(size: int) -> str:
    if size == 0:
        return "0 B"
    units = ["B", "KiB", "MiB", "GiB", "TiB"]
    value = float(size)
    index = 0
    while 1024.0 <= value and index < len(units) - 1:
        value /= 1024.0
        index += 1
    if index == 0:
        return f"{int(value)} {units[index]}"
    return f"{value:.2f} {units[index]}"


@beartype
def format_datetime(ts: float) -> str:
    if ts <= 0:
        return ""
    return datetime.fromtimestamp(ts).strftime("%Y-%m-%d %H:%M:%S")


@beartype
def parse_ffmpeg_out_time(value: str) -> float | None:
    parts = value.split(":")
    if len(parts) != 3:
        return None
    hours = int(parts[0])
    minutes = int(parts[1])
    seconds = float(parts[2])
    return hours * 3600 + minutes * 60 + seconds


@beartype
def format_duration(seconds: float | None) -> str:
    if seconds is None:
        return ""
    total = int(seconds)
    hours = total // 3600
    minutes = (total % 3600) // 60
    sec = total % 60
    return f"{hours:02d}:{minutes:02d}:{sec:02d}"


@beartype
def format_bitrate(bitrate: int | None) -> str:
    if bitrate is None:
        return ""
    return f"{bitrate / 1_000_000:.2f} Mbps"


@beartype
def path_inside_root(path: Path, root: Path) -> bool:
    resolved = path.resolve()
    root_resolved = root.resolve()
    try:
        resolved.relative_to(root_resolved)
        return True
    except ValueError:
        return False


@beartype
def is_video_file(path: Path) -> bool:
    return path.suffix.lower() in VIDEO_EXTENSIONS


@beartype
def build_tree(input_root: Path, current: Path,
               row_in_parent: int) -> TreeEntryModel | None:
    stat = current.stat()
    rel = current.relative_to(
        input_root).as_posix() if current != input_root else "."
    node = TreeEntryModel(
        name=current.name if current != input_root else str(current),
        absolute_path=str(current.resolve()),
        relative_path=rel,
        is_dir=current.is_dir(),
        row_in_parent=row_in_parent,
        created_ts=stat.st_ctime,
    )

    if current.is_dir():
        entries = sorted(current.iterdir(),
                         key=lambda p: (not p.is_dir(), p.name.lower()))
        kept: list[TreeEntryModel] = []
        size_sum = 0
        created_latest = node.created_ts
        next_row = 0
        for entry_path in entries:
            if entry_path.is_symlink() and not path_inside_root(
                    entry_path, input_root):
                continue
            if entry_path.is_dir():
                built = build_tree(input_root, entry_path, next_row)
                if built is None:
                    continue
                kept.append(built)
                next_row += 1
                size_sum += built.file_size_bytes
                if created_latest < built.created_ts:
                    created_latest = built.created_ts
                continue
            if not entry_path.is_file():
                continue
            if not is_video_file(entry_path):
                continue
            if entry_path.is_symlink() and not path_inside_root(
                    entry_path, input_root):
                continue
            child_stat = entry_path.stat()
            child = TreeEntryModel(
                name=entry_path.name,
                absolute_path=str(entry_path.resolve()),
                relative_path=entry_path.relative_to(input_root).as_posix(),
                is_dir=False,
                row_in_parent=next_row,
                file_size_bytes=child_stat.st_size,
                created_ts=child_stat.st_ctime,
            )
            kept.append(child)
            next_row += 1
            size_sum += child.file_size_bytes
            if created_latest < child.created_ts:
                created_latest = child.created_ts
        if len(kept) == 0 and current != input_root:
            return None
        node.entires = kept
        node.file_size_bytes = size_sum
        node.created_ts = created_latest
    return node


@beartype
def collect_video_nodes(node: TreeEntryModel,
                        out: list[TreeEntryModel]) -> None:
    if node.is_dir:
        for entry in node.entires:
            collect_video_nodes(entry, out)
        return
    out.append(node)


@beartype
def assign_share_and_cached_text(node: TreeEntryModel,
                                 total_size: int) -> None:
    node.share = (node.file_size_bytes / total_size) if 0 < total_size else 0.0
    node.created_text = format_datetime(node.created_ts)
    node.size_text = format_size(node.file_size_bytes)
    node.share_text = f"{node.share * 100:.2f}%"
    if node.probe is None:
        node.duration_text = ""
        node.bitrate_text = ""
        node.resolution_text = ""
        node.codec_text = ""
    else:
        node.duration_text = format_duration(node.probe.duration_seconds)
        node.bitrate_text = format_bitrate(node.probe.bitrate_bps)
        node.resolution_text = node.probe.resolution or ""
        node.codec_text = node.probe.codec_name or ""
    for entry in node.entires:
        assign_share_and_cached_text(entry, total_size)


@beartype
def run_ffprobe(path: Path) -> ProbeInfoModel | None:
    ffprobe = local["ffprobe"]
    result = ffprobe.run([
        "-v",
        "error",
        "-print_format",
        "json",
        "-show_format",
        "-show_streams",
        str(path),
    ])
    payload = json.loads(result[1])
    streams_raw = payload.get("streams", [])
    format_raw = payload.get("format", {})
    video_stream: Any = None
    for stream in streams_raw:
        if stream.get("codec_type") == "video":
            video_stream = stream
            break

    def parse_int(value: Any) -> int | None:
        if value is None:
            return None
        if value == "N/A":
            return None
        return int(value)

    def parse_float(value: Any) -> float | None:
        if value is None:
            return None
        if value == "N/A":
            return None
        return float(value)

    width = parse_int(
        video_stream.get("width")) if video_stream is not None else None
    height = parse_int(
        video_stream.get("height")) if video_stream is not None else None
    resolution = f"{width}x{height}" if width is not None and height is not None else None
    return ProbeInfoModel(
        duration_seconds=parse_float(format_raw.get("duration")),
        bitrate_bps=parse_int(format_raw.get("bit_rate")),
        width=width,
        height=height,
        resolution=resolution,
        codec_name=video_stream.get("codec_name")
        if video_stream is not None else None,
        format_name=format_raw.get("format_name"),
    )


@beartype
def probe_one(index: int, total: int, path: Path) -> ProbeTaskResultModel:
    LOGGER.info(f"[{index}/{total}] ffprobe {path}")
    try:
        probe = run_ffprobe(path)
        return ProbeTaskResultModel(absolute_path=str(path.resolve()),
                                    probe=probe)
    except ProcessExecutionError as error:
        LOGGER.error(f"ffprobe failed for {path}: {error}")
        return ProbeTaskResultModel(absolute_path=str(path.resolve()),
                                    probe=None)


@beartype
def apply_probe_results(root: TreeEntryModel,
                        probe_results: list[ProbeTaskResultModel]) -> None:
    by_path = {item.absolute_path: item.probe for item in probe_results}
    videos: list[TreeEntryModel] = []
    collect_video_nodes(root, videos)
    for node in videos:
        node.probe = by_path.get(node.absolute_path)
    assign_share_and_cached_text(root, root.file_size_bytes)


@beartype
def cache_path(input_root: Path) -> Path:
    return input_root / ".video_model_cache.json"


@beartype
def save_cache(input_root: Path, root_entry: TreeEntryModel) -> None:
    payload = CacheModel(input_root=str(input_root.resolve()),
                         generated_ts=datetime.now().timestamp(),
                         root_entry=root_entry)
    cache_path(input_root).write_text(payload.model_dump_json(indent=2),
                                      encoding="utf-8")


@beartype
def load_or_build_model(input_root: Path) -> TreeEntryModel:
    cache_file = cache_path(input_root)
    if cache_file.exists():
        loaded = CacheModel.model_validate_json(
            cache_file.read_text(encoding="utf-8"))
        assign_share_and_cached_text(loaded.root_entry,
                                     loaded.root_entry.file_size_bytes)
        return loaded.root_entry

    root_entry = build_tree(input_root, input_root, 0)
    if root_entry is None:
        raise RuntimeError("Input tree is empty")
    videos: list[TreeEntryModel] = []
    collect_video_nodes(root_entry, videos)
    total = len(videos)
    results: list[ProbeTaskResultModel] = []
    if 0 < total:
        with ThreadPoolExecutor() as executor:
            futures = [
                executor.submit(probe_one, idx + 1, total,
                                Path(v.absolute_path))
                for idx, v in enumerate(videos)
            ]
            for future in as_completed(futures):
                results.append(future.result())
    apply_probe_results(root_entry, results)
    save_cache(input_root, root_entry)
    return root_entry


class ResolutionDelegate(QStyledItemDelegate):

    @beartype
    def createEditor(self, parent: QWidget, option: QStyleOptionViewItem,
                     index: QModelIndex) -> QWidget:
        editor = QComboBox(parent)
        editor.addItems(["", "SD", "HD", "FULL_HD"])
        return editor

    @beartype
    def setEditorData(self, editor: QWidget, index: QModelIndex) -> None:
        value = index.data(Qt.ItemDataRole.EditRole) or ""
        combo = editor
        if isinstance(combo, QComboBox):
            pos = combo.findText(str(value))
            combo.setCurrentIndex(pos if 0 <= pos else 0)

    @beartype
    def setModelData(self, editor: QWidget, model: QAbstractItemModel,
                     index: QModelIndex) -> None:
        combo = editor
        if isinstance(combo, QComboBox):
            model.setData(index, combo.currentText(), Qt.ItemDataRole.EditRole)


class CrfDelegate(QStyledItemDelegate):

    @beartype
    def createEditor(self, parent: QWidget, option: QStyleOptionViewItem,
                     index: QModelIndex) -> QWidget:
        editor = QSpinBox(parent)
        editor.setRange(0, 51)
        return editor

    @beartype
    def setEditorData(self, editor: QWidget, index: QModelIndex) -> None:
        value = int(index.data(Qt.ItemDataRole.EditRole) or 0)
        spin = editor
        if isinstance(spin, QSpinBox):
            spin.setValue(value)

    @beartype
    def setModelData(self, editor: QWidget, model: QAbstractItemModel,
                     index: QModelIndex) -> None:
        spin = editor
        if isinstance(spin, QSpinBox):
            model.setData(index, spin.value(), Qt.ItemDataRole.EditRole)


class ActionButtonDelegate(QStyledItemDelegate):

    def __init__(self,
                 action_name: str,
                 parent: QObject | None = None) -> None:
        super().__init__(parent)
        self._action_name = action_name

    @beartype
    def editorEvent(self, event: Any, model: QAbstractItemModel,
                    option: QStyleOptionViewItem, index: QModelIndex) -> bool:
        if isinstance(event, QMouseEvent):
            if event.type() == event.Type.MouseButtonRelease:
                target_model = model
                target_index = index
                if isinstance(model, QSortFilterProxyModel):
                    target_model = model.sourceModel()
                    target_index = model.mapToSource(index)
                if isinstance(target_model, VideoTreeModel):
                    target_model.handle_action(self._action_name, target_index)
                    return True
        return super().editorEvent(event, model, option, index)


class VideoTreeModel(QAbstractItemModel):
    HEADERS = [
        "Name",
        "Size",
        "Created",
        "Share",
        "Resolution",
        "Bitrate",
        "Duration",
        "Codec",
        "Play Input",
        "Set Resolution",
        "Set CRF",
        "Play Output",
        "Reduction Rate",
        "Convert",
    ]

    COL_NAME = 0
    COL_SIZE = 1
    COL_CREATED = 2
    COL_SHARE = 3
    COL_RESOLUTION = 4
    COL_BITRATE = 5
    COL_DURATION = 6
    COL_CODEC = 7
    COL_PLAY_INPUT = 8
    COL_SET_RESOLUTION = 9
    COL_SET_CRF = 10
    COL_PLAY_OUTPUT = 11
    COL_REDUCTION = 12
    COL_CONVERT = 13

    def __init__(self, root_entry: TreeEntryModel, input_root: Path,
                 output_root: Path, enqueue_callback: Any,
                 save_callback: Any) -> None:
        super().__init__()
        self._root = root_entry
        self._input_root = input_root
        self._output_root = output_root
        self._enqueue_callback = enqueue_callback
        self._save_callback = save_callback
        self._path_map: dict[str, TreeEntryModel] = {}
        self._expanded_dirs: set[str] = set()
        self._build_path_map(self._root)

    @beartype
    def _build_path_map(self, node: TreeEntryModel) -> None:
        self._path_map[node.absolute_path] = node
        for entry in node.entires:
            self._build_path_map(entry)

    @beartype
    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return len(self.HEADERS)

    @beartype
    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        node = self._node_from_index(parent)
        return len(node.entires)

    @beartype
    def index(self, row: int, column: int,
              parent: QModelIndex = QModelIndex()) -> QModelIndex:
        parent_node = self._node_from_index(parent)
        if row < 0 or len(parent_node.entires) <= row:
            return QModelIndex()
        node = parent_node.entires[row]
        return self.createIndex(row, column, node)

    @beartype
    def parent(self, index: QModelIndex) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()
        node: TreeEntryModel = index.internalPointer()
        if node.relative_path == ".":
            return QModelIndex()
        parent_path = Path(node.absolute_path).parent.resolve()
        parent_node = self._path_map.get(str(parent_path))
        if parent_node is None:
            return QModelIndex()
        if parent_node.absolute_path == self._root.absolute_path:
            return QModelIndex()
        return self.createIndex(parent_node.row_in_parent, 0, parent_node)

    @beartype
    def headerData(self,
                   section: int,
                   orientation: Qt.Orientation,
                   role: int = Qt.ItemDataRole.DisplayRole) -> Any:
        if orientation == Qt.Orientation.Horizontal and role == Qt.ItemDataRole.DisplayRole:
            return self.HEADERS[section]
        return None

    @beartype
    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags
        node: TreeEntryModel = index.internalPointer()
        base = Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled
        if node.is_dir:
            return base
        if index.column() in (self.COL_SET_RESOLUTION, self.COL_SET_CRF):
            return base | Qt.ItemFlag.ItemIsEditable
        return base

    @beartype
    def data(self,
             index: QModelIndex,
             role: int = Qt.ItemDataRole.DisplayRole) -> Any:
        if not index.isValid():
            return None
        node: TreeEntryModel = index.internalPointer()
        col = index.column()

        if role == Qt.ItemDataRole.DisplayRole:
            if col == self.COL_NAME:
                return node.name
            if col == self.COL_SIZE:
                return node.size_text
            if col == self.COL_CREATED:
                return node.created_text
            if col == self.COL_SHARE:
                return node.share_text
            if col == self.COL_RESOLUTION:
                return node.resolution_text
            if col == self.COL_BITRATE:
                return node.bitrate_text
            if col == self.COL_DURATION:
                return node.duration_text
            if col == self.COL_CODEC:
                return node.codec_text
            if col == self.COL_PLAY_INPUT and not node.is_dir:
                return ""
            if col == self.COL_SET_RESOLUTION and not node.is_dir:
                return node.action.target_resolution
            if col == self.COL_SET_CRF and not node.is_dir:
                return str(node.action.crf)
            if col == self.COL_PLAY_OUTPUT and not node.is_dir:
                if node.output is not None:
                    return ""
                return ""
            if col == self.COL_REDUCTION and not node.is_dir:
                if node.output is None or node.file_size_bytes == 0:
                    return ""
                ratio = (node.output.file_size_bytes /
                         node.file_size_bytes) * 100.0
                return f"{ratio:.2f}%"
            if col == self.COL_CONVERT and not node.is_dir:
                return ""

        if role == Qt.ItemDataRole.EditRole and not node.is_dir:
            if col == self.COL_SET_RESOLUTION:
                return node.action.target_resolution
            if col == self.COL_SET_CRF:
                return node.action.crf

        if role == Qt.ItemDataRole.TextAlignmentRole and col in (
                self.COL_SIZE, self.COL_SHARE, self.COL_SET_CRF,
                self.COL_REDUCTION):
            return int(Qt.AlignmentFlag.AlignRight
                       | Qt.AlignmentFlag.AlignVCenter)

        if role == Qt.ItemDataRole.UserRole:
            if col == self.COL_SIZE:
                return node.file_size_bytes
            if col == self.COL_SHARE:
                return node.share
            if col == self.COL_CREATED:
                return node.created_ts
            if col == self.COL_REDUCTION:
                if node.output is None or node.file_size_bytes == 0:
                    return 0.0
                return (node.output.file_size_bytes /
                        node.file_size_bytes) * 100.0
            return node.row_in_parent

        elif role == Qt.ItemDataRole.DecorationRole:
            style = QApplication.style()

            if col == self.COL_NAME:
                if node.is_dir:
                    if node.absolute_path in self._expanded_dirs:
                        return style.standardIcon(
                            QStyle.StandardPixmap.SP_DirOpenIcon)
                    return style.standardIcon(QStyle.StandardPixmap.SP_DirIcon)
                return style.standardIcon(QStyle.StandardPixmap.SP_FileIcon)

            if not node.is_dir and col == self.COL_PLAY_INPUT:
                return style.standardIcon(QStyle.StandardPixmap.SP_MediaPlay)

            if not node.is_dir and col == self.COL_PLAY_OUTPUT and node.output is not None:
                return style.standardIcon(QStyle.StandardPixmap.SP_MediaPlay)

            if not node.is_dir and col == self.COL_CONVERT:
                return style.standardIcon(
                    QStyle.StandardPixmap.SP_BrowserReload)

        return None

    @beartype
    def setData(self,
                index: QModelIndex,
                value: Any,
                role: int = Qt.ItemDataRole.EditRole) -> bool:
        if not index.isValid():
            return False
        if role != Qt.ItemDataRole.EditRole:
            return False
        node: TreeEntryModel = index.internalPointer()
        if node.is_dir:
            return False
        if index.column() == self.COL_SET_RESOLUTION:
            node.action.target_resolution = str(value)
            self.dataChanged.emit(
                index, index,
                [Qt.ItemDataRole.DisplayRole, Qt.ItemDataRole.EditRole])
            self._save_callback()
            return True
        if index.column() == self.COL_SET_CRF:
            node.action.crf = int(value)
            self.dataChanged.emit(
                index, index,
                [Qt.ItemDataRole.DisplayRole, Qt.ItemDataRole.EditRole])
            self._save_callback()
            return True
        return False

    @beartype
    def _node_from_index(self, index: QModelIndex) -> TreeEntryModel:
        if index.isValid():
            return index.internalPointer()
        return self._root

    @beartype
    def set_dir_expanded(self, absolute_path: str, expanded: bool) -> None:
        if expanded:
            self._expanded_dirs.add(absolute_path)
        else:
            self._expanded_dirs.discard(absolute_path)

        idx = self.index_for_path(absolute_path)
        if idx.isValid():
            name_idx = self.index(idx.row(), self.COL_NAME, idx.parent())
            self.dataChanged.emit(name_idx, name_idx,
                                  [Qt.ItemDataRole.DecorationRole])

    @beartype
    def handle_action(self, action_name: str, index: QModelIndex) -> None:
        if not index.isValid():
            return
        node: TreeEntryModel = index.internalPointer()
        if node.is_dir:
            return
        if action_name == "play_input":
            local["mpv"].popen([node.absolute_path])
            return
        if action_name == "play_output":
            if node.output is None:
                return
            local["mpv"].popen([node.output.output_path])
            return
        if action_name == "convert":
            if node.action.target_resolution == "" and node.action.crf == 0:
                return
            rel = Path(node.relative_path)
            output_path = self._output_root / rel.with_suffix(".mp4")
            task = ConversionTaskModel(
                source_path=node.absolute_path,
                relative_path=node.relative_path,
                output_path=str(output_path),
                action=node.action.model_copy(deep=True),
                preset="medium",
            )
            self._enqueue_callback(task)
            return

    @beartype
    def update_output(self, result: ConversionResultModel) -> None:
        node = self._path_map[result.source_path]
        node.output = result.output
        assign_share_and_cached_text(self._root, self._root.file_size_bytes)
        idx_row = self.index_for_path(node.absolute_path)
        if idx_row.isValid():
            left = self.index(idx_row.row(), self.COL_PLAY_OUTPUT,
                              idx_row.parent())
            right = self.index(idx_row.row(), self.COL_REDUCTION,
                               idx_row.parent())
            self.dataChanged.emit(
                left, right,
                [Qt.ItemDataRole.DisplayRole, Qt.ItemDataRole.UserRole])
        self._save_callback()

    @beartype
    def index_for_path(self, absolute_path: str) -> QModelIndex:
        node = self._path_map[absolute_path]
        if node.absolute_path == self._root.absolute_path:
            return QModelIndex()
        parent_path = str(Path(node.absolute_path).parent.resolve())
        parent = self._path_map[parent_path]
        return self.createIndex(node.row_in_parent, 0, node)

    @beartype
    def iter_video_nodes(self) -> list[TreeEntryModel]:
        out: list[TreeEntryModel] = []
        collect_video_nodes(self._root, out)
        return out


class DirectorySortProxy(QSortFilterProxyModel):

    @beartype
    def lessThan(self, left: QModelIndex, right: QModelIndex) -> bool:
        src = self.sourceModel()
        if not isinstance(src, VideoTreeModel):
            return super().lessThan(left, right)

        left_node: TreeEntryModel = left.internalPointer()
        right_node: TreeEntryModel = right.internalPointer()

        if left_node.is_dir != right_node.is_dir:
            return left_node.is_dir

        col = self.sortColumn()
        if col < 0:
            return left_node.row_in_parent < right_node.row_in_parent

        lval = src.data(left.siblingAtColumn(col), Qt.ItemDataRole.UserRole)
        rval = src.data(right.siblingAtColumn(col), Qt.ItemDataRole.UserRole)

        if isinstance(lval, (int, float)) and isinstance(rval, (int, float)):
            return lval < rval

        ltxt = str(
            src.data(left.siblingAtColumn(col), Qt.ItemDataRole.DisplayRole)
            or "")
        rtxt = str(
            src.data(right.siblingAtColumn(col), Qt.ItemDataRole.DisplayRole)
            or "")
        return ltxt.lower() < rtxt.lower()


class ConversionWorker(QObject):
    converted = Signal(object)
    failed = Signal(str)

    def __init__(self) -> None:
        super().__init__()
        self._queue: queue.Queue[ConversionTaskModel | None] = queue.Queue()
        self._running = True
        self._lock = threading.Lock()
        self._stop_requested = threading.Event()
        self._current_process: subprocess.Popen[str] | None = None

    @beartype
    def enqueue(self, task: ConversionTaskModel) -> None:
        self._queue.put(task)

    @Slot()
    def run(self) -> None:
        while self._running:
            task = self._queue.get()
            if task is None:
                break
            try:
                result = self._convert(task)
                self.converted.emit(result)
            except Exception as error:
                if not self._stop_requested.is_set():
                    self.failed.emit(f"{task.source_path}: {error}")
            finally:
                self._queue.task_done()

    @beartype
    def stop(self) -> None:
        self._stop_requested.set()
        with self._lock:
            self._running = False
            process = self._current_process

        if process is not None and process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                process.kill()

        self._queue.put(None)

    @beartype
    def _convert(self, task: ConversionTaskModel) -> ConversionResultModel:
        src = Path(task.source_path)
        dst = Path(task.output_path)
        dst.parent.mkdir(parents=True, exist_ok=True)

        source_probe = run_ffprobe(src)
        duration = source_probe.duration_seconds if source_probe is not None else None

        cmd = [
            "ffmpeg",
            "-y",
            "-loglevel",
            "error",
            "-nostats",
            "-progress",
            "pipe:1",
            "-hwaccel",
            "vaapi",
            "-hwaccel_device",
            "/dev/dri/renderD128",
            "-hwaccel_output_format",
            "vaapi",
            "-i",
            str(src),
            "-map",
            "0",
            "-vaapi_device",
            "/dev/dri/renderD128",
        ]

        filters: list[str] = []
        if task.action.target_resolution != "":
            h = RESOLUTION_HEIGHT[task.action.target_resolution]
            filters.append(
                f"scale_vaapi=w=-2:h={h}:force_original_aspect_ratio=decrease")

        if filters:
            cmd.extend(["-vf", ",".join(filters)])

        quality_value = task.action.crf if 0 < task.action.crf else 28
        cmd.extend([
            "-c:v",
            "hevc_vaapi",
            "-rc_mode",
            "CQP",
            "-qp",
            str(quality_value),
            "-preset",
            task.preset,
            "-c:a",
            "copy",
            "-c:s",
            "copy",
            str(dst),
        ])

        LOGGER.info(f"Converting {src} -> {dst}")
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            bufsize=1,
        )

        with self._lock:
            self._current_process = process

        start_ts = time.monotonic()
        last_log_ts = 0.0
        out_time_sec = 0.0

        try:
            if process.stdout is not None:
                for raw_line in process.stdout:
                    if self._stop_requested.is_set():
                        process.terminate()
                        break

                    line = raw_line.strip()
                    if "=" not in line:
                        continue

                    key, value = line.split("=", 1)

                    if key == "out_time":
                        parsed = parse_ffmpeg_out_time(value)
                        if parsed is not None:
                            out_time_sec = parsed

                    if key == "progress":
                        now = time.monotonic()
                        if (duration is not None and 0 < duration
                                and now - last_log_ts >= 1.0):
                            ratio = min(max(out_time_sec / duration, 0.0), 1.0)
                            pct = ratio * 100.0

                            eta_text = ""
                            if 0.0 < ratio < 1.0:
                                elapsed = now - start_ts
                                eta_seconds = elapsed * (1.0 - ratio) / ratio
                                eta_text = format_duration(eta_seconds)
                            elif ratio >= 1.0:
                                eta_text = "00:00:00"

                            LOGGER.info(
                                f"{src.name}: {pct:6.2f}% ETA {eta_text}")
                            last_log_ts = now

            return_code = process.wait()
        finally:
            with self._lock:
                self._current_process = None

        if self._stop_requested.is_set():
            raise RuntimeError("Conversion cancelled")

        if return_code != 0:
            raise RuntimeError(f"ffmpeg failed with exit code {return_code}")

        probe = run_ffprobe(dst)
        stat = dst.stat()
        output = OutputVideoModel(
            output_path=str(dst.resolve()),
            file_size_bytes=stat.st_size,
            created_ts=stat.st_ctime,
            probe=probe,
            action=task.action,
            preset=task.preset,
        )
        return ConversionResultModel(source_path=str(src.resolve()),
                                     output=output)


class MainWindow(QWidget):

    def __init__(self, input_root: Path, output_root: Path,
                 root_entry: TreeEntryModel) -> None:
        super().__init__()
        self._input_root = input_root
        self._output_root = output_root
        self._root_entry = root_entry
        self._preset = "medium"

        self._worker = ConversionWorker()
        self._thread = threading.Thread(target=self._worker.run, daemon=True)
        self._thread.start()

        self._model = VideoTreeModel(
            root_entry=self._root_entry,
            input_root=self._input_root,
            output_root=self._output_root,
            enqueue_callback=self.enqueue_task,
            save_callback=self.save_cache,
        )

        self._proxy = DirectorySortProxy(self)
        self._proxy.setSourceModel(self._model)
        self._proxy.setDynamicSortFilter(True)

        self._tree = QTreeView(self)
        self._tree.setModel(self._proxy)
        self._tree.setRootIsDecorated(True)
        self._tree.setAlternatingRowColors(True)
        self._tree.setSortingEnabled(True)
        self._tree.setStyleSheet("QTreeView::item { height: 26px; }")
        self._tree.setEditTriggers(QTreeView.EditTrigger.CurrentChanged
                                   | QTreeView.EditTrigger.SelectedClicked
                                   | QTreeView.EditTrigger.EditKeyPressed)
        self._tree.setItemDelegateForColumn(VideoTreeModel.COL_SET_RESOLUTION,
                                            ResolutionDelegate(self._tree))
        self._tree.setItemDelegateForColumn(VideoTreeModel.COL_SET_CRF,
                                            CrfDelegate(self._tree))
        self._tree.setItemDelegateForColumn(
            VideoTreeModel.COL_PLAY_INPUT,
            ActionButtonDelegate("play_input", self._tree))
        self._tree.setItemDelegateForColumn(
            VideoTreeModel.COL_PLAY_OUTPUT,
            ActionButtonDelegate("play_output", self._tree))
        self._tree.setItemDelegateForColumn(
            VideoTreeModel.COL_CONVERT,
            ActionButtonDelegate("convert", self._tree))

        self._tree.expanded.connect(self.on_tree_expanded)
        self._tree.collapsed.connect(self.on_tree_collapsed)

        header = self._tree.header()
        header.setSectionsClickable(True)
        for index in range(len(VideoTreeModel.HEADERS)):
            header.setSectionResizeMode(index,
                                        QHeaderView.ResizeMode.Interactive)
        header.sectionClicked.connect(self.handle_sort_click)

        self._sort_state_column = -1
        self._sort_state_mode = 0

        self._preset_combo = QComboBox(self)
        self._preset_combo.addItems(["fast", "medium", "slow"])
        self._preset_combo.currentTextChanged.connect(self.on_preset_changed)

        self._convert_all = QPushButton("Convert all", self)
        self._convert_all.clicked.connect(self.convert_all)

        top = QHBoxLayout()
        top.addWidget(self._preset_combo)
        top.addWidget(self._convert_all)

        root_layout = QVBoxLayout(self)
        root_layout.addLayout(top)
        root_layout.addWidget(self._tree)

        self._worker.converted.connect(self.on_converted)
        self._worker.failed.connect(self.on_failed)

        self.setWindowTitle(f"Video manager: {input_root}")
        self.resize(1600, 900)
        self._tree.collapseAll()

    @Slot(str)
    def on_preset_changed(self, value: str) -> None:
        self._preset = value

    @Slot()
    def convert_all(self) -> None:
        videos = self._model.iter_video_nodes()
        for node in videos:
            action_enabled = node.action.target_resolution != "" or 0 < node.action.crf
            if not action_enabled:
                continue
            if node.output is not None:
                if node.output.action.target_resolution == node.action.target_resolution and node.output.action.crf == node.action.crf and node.output.preset == self._preset:
                    continue
            rel = Path(node.relative_path)
            out = self._output_root / rel.with_suffix(".mp4")
            task = ConversionTaskModel(
                source_path=node.absolute_path,
                relative_path=node.relative_path,
                output_path=str(out),
                action=node.action.model_copy(deep=True),
                preset=self._preset,
            )
            self.enqueue_task(task)

    @Slot(QModelIndex)
    def on_tree_expanded(self, proxy_index: QModelIndex) -> None:
        src_index = self._proxy.mapToSource(proxy_index)
        if not src_index.isValid():
            return
        node: TreeEntryModel = src_index.internalPointer()
        if node.is_dir:
            self._model.set_dir_expanded(node.absolute_path, True)

    @Slot(QModelIndex)
    def on_tree_collapsed(self, proxy_index: QModelIndex) -> None:
        src_index = self._proxy.mapToSource(proxy_index)
        if not src_index.isValid():
            return
        node: TreeEntryModel = src_index.internalPointer()
        if node.is_dir:
            self._model.set_dir_expanded(node.absolute_path, False)

    @beartype
    def enqueue_task(self, task: ConversionTaskModel) -> None:
        task.preset = self._preset
        LOGGER.info(f"Queue conversion: {task.source_path}")
        self._worker.enqueue(task)

    @Slot(object)
    def on_converted(self, result_obj: object) -> None:
        result = result_obj
        if isinstance(result, ConversionResultModel):
            LOGGER.info(f"Conversion done: {result.source_path}")
            self._model.update_output(result)

    @Slot(str)
    def on_failed(self, message: str) -> None:
        LOGGER.error(f"Conversion failed: {message}")

    @Slot(int)
    def handle_sort_click(self, section: int) -> None:
        if section != self._sort_state_column:
            self._sort_state_column = section
            self._sort_state_mode = 1
        else:
            self._sort_state_mode += 1
            if 3 <= self._sort_state_mode:
                self._sort_state_mode = 0

        if self._sort_state_mode == 0:
            self._proxy.sort(-1, Qt.SortOrder.AscendingOrder)
            self._tree.header().setSortIndicator(-1,
                                                 Qt.SortOrder.AscendingOrder)
            return
        if self._sort_state_mode == 1:
            self._proxy.sort(section, Qt.SortOrder.AscendingOrder)
            return
        self._proxy.sort(section, Qt.SortOrder.DescendingOrder)

    @beartype
    def save_cache(self) -> None:
        save_cache(self._input_root, self._root_entry)

    def closeEvent(self, event: Any) -> None:
        self._worker.stop()
        self._thread.join(timeout=3)
        super().closeEvent(event)


@click.command()
@click.option("--input-root",
              type=click.Path(path_type=Path, exists=True, file_okay=False),
              required=True)
@click.option("--output-root",
              type=click.Path(path_type=Path, file_okay=False),
              required=True)
def main(input_root: Path, output_root: Path) -> None:
    configure_logging()
    resolved_input = input_root.resolve()
    resolved_output = output_root.resolve()
    resolved_output.mkdir(parents=True, exist_ok=True)
    root_entry = load_or_build_model(resolved_input)
    app = QApplication(sys.argv)
    win = MainWindow(resolved_input, resolved_output, root_entry)
    win.show()
    raise SystemExit(app.exec())


if __name__ == "__main__":
    main()

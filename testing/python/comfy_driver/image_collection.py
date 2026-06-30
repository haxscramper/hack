#!/usr/bin/env python

import argparse
import json
import random
import urllib.request
from dataclasses import dataclass
from difflib import SequenceMatcher
from pathlib import Path

from arango import ArangoClient
from pydantic import BaseModel, Field
from PySide6.QtCore import (
    QAbstractTableModel,
    QEvent,
    QModelIndex,
    QObject,
    QPersistentModelIndex,
    QRunnable,
    QSize,
    QSortFilterProxyModel,
    Qt,
    QThreadPool,
    Signal,
)
from PySide6.QtGui import QColor, QImage, QPainter, QPixmap, QTextOption
from PySide6.QtWidgets import (
    QAbstractItemDelegate,
    QApplication,
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QDoubleSpinBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QMessageBox,
    QPlainTextEdit,
    QPushButton,
    QScrollArea,
    QSpinBox,
    QStyle,
    QStyledItemDelegate,
    QStyleOptionButton,
    QTableView,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

RUN_BTN_W = 64
RUN_BTN_H = 26
MODEL_EXTS = {".safetensors", ".ckpt", ".pt", ".pth", ".bin"}
IMAGE_EXTS = (".png", ".jpg", ".jpeg", ".webp", ".bmp")
THUMB = 228

(
    COL_IMAGE,
    COL_POSITIVE,
    COL_NEGATIVE,
    COL_WIDTH,
    COL_HEIGHT,
    COL_STEPS,
    COL_CFG,
    COL_CHECKPOINT,
    COL_LORAS,
    COL_MD5,
    COL_RUN,
) = range(11)

HEADERS = [
    "Image",
    "Positive",
    "Negative",
    "Width",
    "Height",
    "Steps",
    "CFG",
    "Checkpoint",
    "LoRAs",
    "MD5",
    "",
]

EDITABLE_COLS = {
    COL_POSITIVE,
    COL_NEGATIVE,
    COL_WIDTH,
    COL_HEIGHT,
    COL_STEPS,
    COL_CFG,
    COL_CHECKPOINT,
}

COLOR_WARN = QColor(255, 165, 0)
COLOR_BAD = QColor(220, 80, 80)
COLOR_OK = QColor(70, 150, 90)
COLOR_REPLACED = QColor(150, 80, 220)

LORA_ROW_H = 22
LORA_ON_W = 34
LORA_WEIGHT_W = 84

EDITOR_H = 28
MIN_W_CHECKPOINT = 260
MIN_W_LORAS = 420
MIN_W_MD5 = 220

AQL = """
FOR gp IN generation_params
  FILTER gp.result.positive != "" AND gp.result.negative != ""
  LET f = DOCUMENT(@@files, gp.md5)
  FILTER f != null
  FOR p IN f.paths
    LET root = DOCUMENT(@@roots, p.root.name)
    RETURN {
      path: CONCAT(root.path, "/", p.relative),
      md5: gp.md5,
      indexer_id: gp.indexer_id,
      result: gp.result
    }
"""

SAFETENSOR_AQL = """
FOR doc IN @@safetensor
  LET f = DOCUMENT(@@files, doc.md5)
  FILTER f != null
  FOR p IN f.paths
    LET root = DOCUMENT(@@roots, p.root.name)
    RETURN {
      arch: doc.result.metadata.`modelspec.architecture`,
      md5: doc.md5,
      root: root.path,
      relative: p.relative
    }
"""

KIND_SUBDIRS = {
    "checkpoint": ["checkpoints", "diffusion_models"],
    "lora": ["loras"],
    "vae": ["vae"],
    "clip": ["text_encoders", "clip"],
}

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------


class Config(BaseModel):
    comfy_dir: Path
    workflow_path: Path
    comfy_url: str = "http://127.0.0.1:8188"
    arango_url: str = "http://127.0.0.1:8529"
    db_name: str = "test_index"
    username: str = "root"
    password: str = "test"
    roots_collection: str = "roots"
    files_collection: str = "files"
    safetensor_collection: str = "safetensor"
    model_replacements: dict[str, str] = Field(default_factory=dict)
    arch_type_map: dict[str, str] = Field(default_factory=dict)
    model_arch_map: dict[str, str] = Field(default_factory=dict)
    default_vae: dict[str, str] = Field(default_factory=dict)
    default_clip: dict[str, str] = Field(default_factory=dict)
    replacement_overrides: dict[str, dict] = Field(default_factory=dict)


# ---------------------------------------------------------------------------
# Model registry / scanning
# ---------------------------------------------------------------------------


def strip_ext(name: str) -> str:
    if not name:
        return name
    p = Path(name)
    if p.suffix.lower() in MODEL_EXTS:
        return p.stem
    return name


@dataclass(frozen=True)
class ModelEntry:
    display: str
    payload: str
    arch: str | None
    type: str | None


def scan_models(comfy_dir: Path, subdir: str) -> list[tuple[str, str, str]]:
    base = comfy_dir / "models" / subdir
    out: list[tuple[str, str, str]] = []
    if base.exists():
        for p in base.rglob("*"):
            if not p.is_file() or p.suffix.lower() not in MODEL_EXTS:
                continue
            rel = p.relative_to(base).with_suffix("").as_posix()
            payload = p.stem
            arch_key = f"{subdir}/{rel}"
            out.append((rel, payload, arch_key))
    return out


class ModelRegistry:

    def __init__(
        self,
        comfy_dir: Path,
        arch_info: dict[str, str | None],
        config: Config,
    ):
        self.model_replacements = {
            self._norm_key(k): (self.normalize_payload(v) or v.strip())
            for k, v in config.model_replacements.items()
        }
        self.arch_type_map = dict(config.arch_type_map)
        self.model_arch_map = {
            self._norm_key(k): v
            for k, v in config.model_arch_map.items()
        }

        self.replacement_overrides = {
            self._norm_key(k): v
            for k, v in config.replacement_overrides.items()
        }

        self._entries: dict[str, list[ModelEntry]] = {}
        self._payload_sets: dict[str, set[str]] = {}
        self._display_to_payload: dict[str, dict[str, str]] = {}
        self._payload_to_entry: dict[str, dict[str, ModelEntry]] = {}

        for kind, subdirs in KIND_SUBDIRS.items():
            entries: list[ModelEntry] = []
            for subdir in subdirs:
                for rel, payload, arch_key in scan_models(comfy_dir, subdir):
                    arch = arch_info.get(arch_key) or self.model_arch_map.get(
                        payload)
                    type_ = self.arch_type_map.get(arch) if arch else None
                    entries.append(
                        ModelEntry(display=rel,
                                   payload=payload,
                                   arch=arch,
                                   type=type_))
            entries.sort(key=lambda e: e.display.casefold())
            self._entries[kind] = entries
            self._payload_sets[kind] = {e.payload for e in entries}
            self._display_to_payload[kind] = {
                e.display: e.payload
                for e in entries
            }
            self._payload_to_entry[kind] = {e.payload: e for e in entries}

        self.default_vae = {
            k: self.resolve_payload(v, "vae")
            for k, v in config.default_vae.items()
        }
        self.default_clip = {
            k: self.resolve_payload(v, "clip")
            for k, v in config.default_clip.items()
        }

    def _norm_key(self, k: str) -> str:
        if not k:
            return ""
        n = self.normalize_payload(k)
        return n or k.strip()

    def options(self, kind: str) -> list[tuple[str, str]]:
        return [(e.display, e.payload) for e in self._entries[kind]]

    def normalize_payload(self, value: str) -> str:
        if not value:
            return ""
        s = strip_ext(value.strip())
        return Path(s).name

    def _resolve_display_to_payload(self, value: str, kind: str) -> str:
        value = (value or "").strip()
        if not value:
            return ""
        m = self._display_to_payload[kind]
        if value in m:
            return m[value]
        return self.normalize_payload(value)

    def resolve_with_replacement(self, value: str,
                                 kind: str) -> tuple[str, bool]:
        raw = (value or "").strip()
        payload = self._resolve_display_to_payload(raw, kind)
        if payload and payload in self._payload_sets[kind]:
            return payload, False
        repl = self.model_replacements.get(raw) or self.model_replacements.get(
            payload)
        if repl:
            repl_payload = self._resolve_display_to_payload(
                repl, kind) or self.normalize_payload(repl)
            return repl_payload, True
        return payload, False

    def resolve_payload(self, value: str, kind: str) -> str:
        payload, _ = self.resolve_with_replacement(value, kind)
        return payload

    def display_for_payload(self, payload: str, kind: str) -> str:
        e = self._payload_to_entry[kind].get(payload)
        return e.display if e else payload

    def type_for_payload(self, payload: str, kind: str) -> str | None:
        e = self._payload_to_entry[kind].get(payload)
        return e.type if e else None

    def is_known_payload(self, payload: str, kind: str) -> bool:
        return payload in self._payload_sets[kind]

    def color_for(self, value: str, kind: str):
        payload, replaced = self.resolve_with_replacement(value, kind)
        if not payload:
            return None
        if replaced:
            return COLOR_REPLACED
        if payload in self._payload_sets[kind]:
            return None
        best = max(
            (SequenceMatcher(None, payload, c).ratio()
             for c in self._payload_sets[kind]),
            default=0.0,
        )
        return COLOR_WARN if best >= 0.7 else COLOR_BAD

    def has(self, value: str, kind: str) -> bool:
        payload, _ = self.resolve_with_replacement(value, kind)
        return payload in self._payload_sets[kind]


# ---------------------------------------------------------------------------
# Image loading
# ---------------------------------------------------------------------------


def resolve_image_path(path: str) -> str:
    p = Path(path)
    if p.suffix.lower() in IMAGE_EXTS and p.exists():
        return str(p)
    for ext in IMAGE_EXTS:
        cand = p.with_suffix(ext)
        if cand.exists():
            return str(cand)
    return path


class ImageLoaderSignals(QObject):
    done = Signal(str)


class ImageLoader(QRunnable):

    def __init__(self, path: str, cache: dict, signals: ImageLoaderSignals):
        super().__init__()
        self.path = path
        self.cache = cache
        self.signals = signals

    def run(self) -> None:
        real = resolve_image_path(self.path)
        img = QImage(real)
        if not img.isNull():
            img = img.scaled(
                THUMB,
                THUMB,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation,
            )
        self.cache[self.path] = img
        self.signals.done.emit(self.path)


class ImageDelegate(QStyledItemDelegate):

    def __init__(self, view: QTableView):
        super().__init__(view)
        self.view = view
        self.cache: dict[str, QImage] = {}
        self.pending: set[str] = set()
        self.pool = QThreadPool.globalInstance()
        self.signals = ImageLoaderSignals()
        self.signals.done.connect(self._on_done)

    def _on_done(self, path: str) -> None:
        self.pending.discard(path)
        self.view.viewport().update()

    def sizeHint(self, option, index) -> QSize:
        return QSize(THUMB + 8, THUMB + 8)

    def paint(self, painter, option, index) -> None:
        path = index.data(Qt.ItemDataRole.DisplayRole)
        img = self.cache.get(path)
        if img is None:
            if path not in self.pending:
                self.pending.add(path)
                self.pool.start(ImageLoader(path, self.cache, self.signals))
            painter.drawText(option.rect, Qt.AlignmentFlag.AlignCenter,
                             "loading…")
            return
        if img.isNull():
            painter.drawText(option.rect, Qt.AlignmentFlag.AlignCenter,
                             "no image")
            return
        x = option.rect.x() + (option.rect.width() - img.width()) // 2
        y = option.rect.y() + (option.rect.height() - img.height()) // 2
        painter.drawImage(x, y, img)


class ImagePreviewDialog(QDialog):

    def __init__(self, path: str, parent=None):
        super().__init__(parent)
        self._path = path
        self.setWindowTitle(Path(path).name)
        self.resize(1024, 768)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(4)

        from PySide6.QtWidgets import QScrollArea

        scroll = QScrollArea(self)
        scroll.setWidgetResizable(True)
        scroll.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(scroll)

        self.label = QLabel(scroll)
        self.label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        scroll.setWidget(self.label)

        real = resolve_image_path(path)
        pix = QPixmap(real)
        if pix.isNull():
            self.label.setText(f"Could not load image:\n{real}")
        else:
            self.label.setPixmap(pix)

        btn_row = QHBoxLayout()
        btn_row.addStretch()
        copy_btn = QPushButton("Copy", self)
        copy_btn.setFixedHeight(EDITOR_H)
        copy_btn.clicked.connect(self._copy_path)
        btn_row.addWidget(copy_btn)
        layout.addLayout(btn_row)

    def _copy_path(self) -> None:
        QApplication.clipboard().setText(self._path)


# ---------------------------------------------------------------------------
# Delegates
# ---------------------------------------------------------------------------


class ModelComboDelegate(QStyledItemDelegate):

    def __init__(
        self,
        options: list[tuple[str, str]],
        allow_empty: bool,
        registry: ModelRegistry,
        kind: str,
        filter_by_type: bool = False,
        parent=None,
    ):
        super().__init__(parent)
        self._options = options
        self._allow_empty = allow_empty
        self._registry = registry
        self._kind = kind
        self._filter_by_type = filter_by_type

    def _sorted_options(
            self, default_text: str,
            options: list[tuple[str, str]]) -> list[tuple[str, str]]:
        needle = (default_text or "").strip().casefold()

        if not needle:
            return sorted(options, key=lambda item: item[0].casefold())

        def similarity(item: tuple[str, str]) -> float:
            display, payload = item
            d = (display or "").casefold()
            p = (payload or "").casefold()
            return max(
                SequenceMatcher(None, needle, d).ratio(),
                SequenceMatcher(None, needle, p).ratio(),
            )

        return sorted(
            options,
            key=lambda item: (-similarity(item), item[0].casefold()),
        )

    def _filtered_options(self, default_text: str,
                          index: QModelIndex) -> list[tuple[str, str]]:
        opts = self._options
        if self._filter_by_type:
            current = (index.data(Qt.ItemDataRole.EditRole) or "").strip()
            current_payload = self._registry.resolve_payload(
                current, self._kind)
            current_type = self._registry.type_for_payload(
                current_payload, self._kind)
            if current_type is not None:
                opts = [
                    o for o in opts if self._registry.type_for_payload(
                        o[1], self._kind) == current_type
                ]
        return self._sorted_options(default_text, opts)

    def createEditor(self, parent, option, index):
        cb = QComboBox(parent)
        cb.setEditable(True)
        cb.setMaxVisibleItems(14)
        cb.setMaximumHeight(EDITOR_H)

        default_text = (index.data(Qt.ItemDataRole.DisplayRole)
                        or index.data(Qt.ItemDataRole.EditRole) or "").strip()

        if self._allow_empty:
            cb.addItem("", "")

        for display, payload in self._filtered_options(default_text, index):
            cb.addItem(display, payload)

        return cb

    def updateEditorGeometry(self, editor, option, index):
        margin_x = 1
        available_w = max(0, option.rect.width() - (2 * margin_x))
        editor_h = min(36, max(0, option.rect.height() - 4))
        y = option.rect.y() + (option.rect.height() - editor_h) // 2

        editor.setGeometry(option.rect.x() + margin_x, y, available_w,
                           editor_h)

    def setEditorData(self, editor, index):
        payload = self._registry.resolve_payload(
            index.data(Qt.ItemDataRole.EditRole) or "", self._kind)
        for i in range(editor.count()):
            if (editor.itemData(i) or "") == payload:
                editor.setCurrentIndex(i)
                return
        editor.setCurrentText(index.data(Qt.ItemDataRole.EditRole) or "")

    def setModelData(self, editor, model, index):
        text = editor.currentText().strip()
        payload = editor.currentData()
        if payload:
            model.setData(index, payload, Qt.ItemDataRole.EditRole)
            return
        model.setData(
            index,
            self._registry.resolve_payload(text, self._kind),
            Qt.ItemDataRole.EditRole,
        )

    def paint(self, painter, option, index):
        value = (index.data(Qt.ItemDataRole.DisplayRole) or "").strip()
        payload, replaced = self._registry.resolve_with_replacement(
            value, self._kind)

        if not value:
            text = "—"
        elif replaced:
            text = self._registry.display_for_payload(payload,
                                                      self._kind) or payload
        else:
            text = value

        status = self._registry.color_for(value, self._kind)
        if not value:
            bg = QColor(110, 110, 110, 45)
        elif status == COLOR_BAD:
            bg = QColor(220, 80, 80, 60)
        elif status == COLOR_WARN:
            bg = QColor(255, 165, 0, 60)
        elif status == COLOR_REPLACED:
            bg = QColor(150, 80, 220, 60)
        else:
            bg = QColor(70, 150, 90, 50)
        fg = option.palette.text().color()

        r = option.rect.adjusted(6, (option.rect.height() - 22) // 2, -6,
                                 -(option.rect.height() - 22) // 2)

        painter.save()
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(bg)
        painter.drawRoundedRect(r, 6, 6)
        painter.setPen(fg)
        shown = option.fontMetrics.elidedText(text,
                                              Qt.TextElideMode.ElideMiddle,
                                              r.width() - 10)
        painter.drawText(
            r.adjusted(5, 0, -5, 0),
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            shown,
        )
        painter.restore()


class LoraEditorWidget(QWidget):

    def __init__(self,
                 options: list[tuple[str, str]],
                 registry: ModelRegistry,
                 parent=None):
        super().__init__(parent)
        self._options = options
        self._registry = registry

        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(2)

        self.table = QTableWidget(0, 3, self)
        self.table.setHorizontalHeaderLabels(["On", "LoRA", "Weight"])
        self.table.horizontalHeader().setVisible(False)
        self.table.horizontalHeader().setSectionResizeMode(
            0, QHeaderView.ResizeMode.Fixed)
        self.table.horizontalHeader().setSectionResizeMode(
            1, QHeaderView.ResizeMode.Stretch)
        self.table.horizontalHeader().setSectionResizeMode(
            2, QHeaderView.ResizeMode.Fixed)
        self.table.setColumnWidth(0, LORA_ON_W)
        self.table.setColumnWidth(2, LORA_WEIGHT_W)

        self.table.verticalHeader().setVisible(False)
        self.table.verticalHeader().setDefaultSectionSize(LORA_ROW_H)
        self.table.verticalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.Fixed)

        self.table.setShowGrid(True)
        self.table.setWordWrap(False)
        layout.addWidget(self.table)

        btn_row = QHBoxLayout()
        btn_row.setContentsMargins(0, 0, 0, 0)
        btn_row.setSpacing(2)
        add = QPushButton("+", self)
        rem = QPushButton("-", self)
        add.setFixedWidth(24)
        rem.setFixedWidth(24)
        add.clicked.connect(self._add_row)
        rem.clicked.connect(self._remove_row)
        btn_row.addStretch()
        btn_row.addWidget(add)
        btn_row.addWidget(rem)
        layout.addLayout(btn_row)

    def _resort_combo_by_similarity(self, cb: QComboBox, text: str) -> None:
        needle = self._registry.normalize_payload(text)
        if needle:
            ranked = sorted(
                self._options,
                key=lambda item: SequenceMatcher(None, needle, item[1]).ratio(
                ),
                reverse=True,
            )
        else:
            ranked = self._options

        current_text = cb.currentText()
        cb.blockSignals(True)
        cb.clear()
        for display, payload in ranked:
            cb.addItem(display, payload)
        cb.setCurrentText(current_text)
        cb.blockSignals(False)

    def _mk_combo(self, payload: str = "") -> QComboBox:
        cb = QComboBox(self.table)
        cb.setEditable(True)
        cb.setMaxVisibleItems(14)

        for display, p in self._options:
            cb.addItem(display, p)

        if cb.lineEdit() is not None:
            cb.lineEdit().textEdited.connect(
                lambda t, combo=cb: self._resort_combo_by_similarity(combo, t))

        if payload:
            for i in range(cb.count()):
                if cb.itemData(i) == payload:
                    cb.setCurrentIndex(i)
                    return cb
            cb.setCurrentText(payload)
        return cb

    def _add_row(self, lora: dict | None = None):
        lora = lora or {"enabled": True, "model": "", "weight": 1.0}
        r = self.table.rowCount()
        self.table.insertRow(r)
        self.table.setRowHeight(r, LORA_ROW_H)

        on_item = QTableWidgetItem()
        model_value = lora.get("model", "")
        color = self._registry.color_for(model_value, "lora")
        is_missing = color in (COLOR_BAD, COLOR_WARN)

        if is_missing:
            on_item.setFlags(Qt.ItemFlag.ItemIsSelectable)
            on_item.setCheckState(Qt.CheckState.Unchecked)
        else:
            on_item.setFlags(Qt.ItemFlag.ItemIsEnabled
                             | Qt.ItemFlag.ItemIsUserCheckable
                             | Qt.ItemFlag.ItemIsSelectable)
            on_item.setCheckState(Qt.CheckState.Checked if lora.get(
                "enabled", True) else Qt.CheckState.Unchecked)
        self.table.setItem(r, 0, on_item)

        payload = self._registry.resolve_payload(lora.get("model", ""), "lora")
        combo = self._mk_combo(payload)
        self.table.setCellWidget(r, 1, combo)

        sb = QDoubleSpinBox(self.table)
        sb.setRange(-10.0, 10.0)
        sb.setSingleStep(0.05)
        sb.setValue(float(lora.get("weight", 1.0)))
        self.table.setCellWidget(r, 2, sb)

    def _remove_row(self):
        r = self.table.currentRow()
        if r >= 0:
            self.table.removeRow(r)

    def set_loras(self, loras: list[dict]):
        self.table.setRowCount(0)
        for l in loras:
            self._add_row({
                "enabled": l.get("enabled", True),
                "model": l.get("model", ""),
                "weight": l.get("weight", 1.0),
            })
        if self.table.rowCount() == 0:
            self._add_row()

    def loras(self) -> list[dict]:
        out = []
        for r in range(self.table.rowCount()):
            on_item = self.table.item(r, 0)
            enabled = on_item.checkState(
            ) == Qt.CheckState.Checked if on_item else True

            cb = self.table.cellWidget(r, 1)
            model_text = cb.currentText().strip() if cb else ""
            model_payload = self._registry.resolve_payload(model_text, "lora")
            if not model_payload:
                continue

            sb = self.table.cellWidget(r, 2)
            weight = sb.value() if sb else 1.0

            out.append({
                "enabled": enabled,
                "model": model_payload,
                "weight": float(weight),
            })
        return out


class LoraDelegate(QStyledItemDelegate):

    def __init__(self, registry: ModelRegistry, parent=None):
        super().__init__(parent)
        self._registry = registry

    def createEditor(self, parent, option, index):
        ckpt_idx = index.sibling(index.row(), COL_CHECKPOINT)
        ckpt = (ckpt_idx.data(Qt.ItemDataRole.EditRole) or "").strip()
        ckpt_payload = self._registry.resolve_payload(ckpt, "checkpoint")
        ckpt_type = self._registry.type_for_payload(ckpt_payload, "checkpoint")
        if ckpt_type is not None:
            opts = [
                o for o in self._registry.options("lora")
                if self._registry.type_for_payload(o[1], "lora") == ckpt_type
            ]
        else:
            opts = self._registry.options("lora")
        return LoraEditorWidget(opts, self._registry, parent)

    def setEditorData(self, editor, index):
        editor.set_loras(index.data(Qt.ItemDataRole.UserRole) or [])

    def setModelData(self, editor, model, index):
        model.setData(index, editor.loras(), Qt.ItemDataRole.EditRole)

    def paint(self, painter, option, index):
        loras = index.data(Qt.ItemDataRole.UserRole) or []
        if not loras:
            painter.drawText(option.rect, Qt.AlignmentFlag.AlignCenter, "—")
            return

        painter.save()
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)

        r = option.rect.adjusted(4, 4, -4, -4)
        row_h = 22

        on_w = 34
        weight_w = 84
        model_w = max(0, r.width() - on_w - weight_w)

        x_on = r.x()
        x_model = x_on + on_w
        x_weight = x_model + model_w

        painter.setPen(option.palette.mid().color())
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawRect(r)

        for i, lora in enumerate(loras):
            y = r.y() + i * row_h
            if y >= r.bottom():
                break

            row_rect = r.__class__(r.x(), y, r.width(),
                                   min(row_h,
                                       r.bottom() - y + 1))

            enabled = bool(lora.get("enabled", True))
            raw_model = str(lora.get("model", "") or "")
            model_payload, model_replaced = self._registry.resolve_with_replacement(
                raw_model, "lora")
            if model_replaced:
                model = (self._registry.display_for_payload(
                    model_payload, "lora") or model_payload)
            else:
                model = raw_model
            weight = float(lora.get("weight", 1.0))

            if not enabled:
                bg = QColor(120, 120, 120, 35)
            else:
                status = self._registry.color_for(model, "lora")
                if status == COLOR_BAD:
                    bg = QColor(220, 80, 80, 55)
                elif status == COLOR_WARN:
                    bg = QColor(255, 165, 0, 55)
                elif status == COLOR_REPLACED:
                    bg = QColor(150, 80, 220, 55)
                else:
                    bg = QColor(70, 150, 90, 45)

            painter.fillRect(row_rect, bg)

            painter.setPen(option.palette.mid().color())
            painter.drawLine(r.x(), y, r.right(), y)

            painter.drawLine(x_model, y, x_model, y + row_rect.height() - 1)
            painter.drawLine(x_weight, y, x_weight, y + row_rect.height() - 1)

            painter.setPen(option.palette.text().color())
            on_text = "☑" if enabled else "☐"
            painter.drawText(
                r.__class__(x_on + 4, y, on_w - 8, row_rect.height()),
                Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignHCenter,
                on_text,
            )

            painter.drawText(
                r.__class__(x_model + 6, y, model_w - 12, row_rect.height()),
                Qt.AlignmentFlag.AlignVCenter
                | Qt.AlignmentFlag.AlignLeft
                | Qt.TextFlag.TextSingleLine,
                model,
            )

            painter.drawText(
                r.__class__(x_weight + 6, y, weight_w - 10, row_rect.height()),
                Qt.AlignmentFlag.AlignVCenter
                | Qt.AlignmentFlag.AlignRight
                | Qt.TextFlag.TextSingleLine,
                f"{weight:g}",
            )

        painter.restore()


class PromptEdit(QPlainTextEdit):

    def __init__(self, text: str, parent=None):
        super().__init__(parent)
        self.setPlainText(text)
        self.setWordWrapMode(QTextOption.WrapMode.WordWrap)
        self.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff)


class PromptDelegate(QStyledItemDelegate):

    def createEditor(self, parent, option, index):
        return PromptEdit(index.data(Qt.ItemDataRole.EditRole) or "", parent)

    def setEditorData(self, editor, index):
        editor.setPlainText(index.data(Qt.ItemDataRole.EditRole) or "")

    def setModelData(self, editor, model, index):
        model.setData(index, editor.toPlainText(), Qt.ItemDataRole.EditRole)

    def updateEditorGeometry(self, editor, option, index):
        editor.setGeometry(option.rect.adjusted(2, 2, -2, -2))

    def sizeHint(self, option, index):
        h = max(option.rect.height(), EDITOR_H * 3)
        return QSize(option.rect.width(), h)


class NumberDelegate(QStyledItemDelegate):

    def __init__(self, decimals: bool, parent=None):
        super().__init__(parent)
        self._decimals = decimals

    def createEditor(self, parent, option, index):
        if self._decimals:
            sb = QDoubleSpinBox(parent)
            sb.setDecimals(3)
            sb.setSingleStep(0.05)
        else:
            sb = QSpinBox(parent)
            sb.setSingleStep(1)
            sb.setMinimum(0)
            sb.setMaximum(8192)

        sb.setMaximumHeight(EDITOR_H)
        sb.setFixedHeight(EDITOR_H)
        return sb

    def updateEditorGeometry(self, editor, option, index):
        y = option.rect.y() + (option.rect.height() - EDITOR_H) // 2
        editor.setGeometry(option.rect.x(), y, max(0, option.rect.width()),
                           EDITOR_H)

    def setEditorData(self, editor, index):
        editor.setValue(index.data(Qt.ItemDataRole.EditRole))

    def setModelData(self, editor, model, index):
        model.setData(index, editor.value(), Qt.ItemDataRole.EditRole)


class ButtonDelegate(QStyledItemDelegate):
    clicked = Signal(QModelIndex)

    def __init__(self, view: QTableView, parent=None):
        super().__init__(parent)
        self._view = view
        self._pressed = QPersistentModelIndex()

    def _button_rect(self, rect):
        x = rect.x() + (rect.width() - RUN_BTN_W) // 2
        y = rect.y() + (rect.height() - RUN_BTN_H) // 2
        return rect.__class__(x, y, RUN_BTN_W, RUN_BTN_H)

    def paint(self, painter, option, index):
        opt = QStyleOptionButton()
        opt.rect = self._button_rect(option.rect)
        opt.text = "Run"
        opt.state = QStyle.StateFlag.State_Enabled
        if (self._pressed.isValid() and self._pressed.row() == index.row()
                and self._pressed.column() == index.column()):
            opt.state |= QStyle.StateFlag.State_Sunken
        else:
            opt.state |= QStyle.StateFlag.State_Raised
        QApplication.style().drawControl(QStyle.ControlElement.CE_PushButton,
                                         opt, painter)

    def editorEvent(self, event, model, option, index):
        br = self._button_rect(option.rect)

        if event.type() == QEvent.Type.MouseButtonPress and br.contains(
                event.pos()):
            self._pressed = QPersistentModelIndex(index)
            self._view.viewport().update(option.rect)
            return True

        if event.type() == QEvent.Type.MouseButtonRelease:
            hit = br.contains(event.pos())
            was_pressed = (self._pressed.isValid()
                           and self._pressed.row() == index.row()
                           and self._pressed.column() == index.column())
            self._pressed = QPersistentModelIndex()
            self._view.viewport().update(option.rect)
            if was_pressed and hit:
                self.clicked.emit(index)
                return True

        return False


# ---------------------------------------------------------------------------
# Table model
# ---------------------------------------------------------------------------


class GenerationModel(QAbstractTableModel):

    def __init__(self, rows: list[dict], registry: ModelRegistry):
        super().__init__()
        self.rows = rows
        self.registry = registry
        for row in self.rows:
            loras = row.get("result", {}).get("loras", [])
            for l in loras:
                if "enabled" not in l:
                    l["enabled"] = True
                color = self.registry.color_for(l.get("model", ""), "lora")
                if color in (COLOR_BAD, COLOR_WARN):
                    l["enabled"] = False

    def rowCount(self, parent=QModelIndex()) -> int:
        return 0 if parent.isValid() else len(self.rows)

    def columnCount(self, parent=QModelIndex()) -> int:
        return len(HEADERS)

    def headerData(self,
                   section,
                   orientation,
                   role=Qt.ItemDataRole.DisplayRole):
        if (role == Qt.ItemDataRole.DisplayRole
                and orientation == Qt.Orientation.Horizontal):
            return HEADERS[section]
        return None

    def flags(self, index):
        base = Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable
        if index.column() in EDITABLE_COLS | {COL_LORAS}:
            return base | Qt.ItemFlag.ItemIsEditable
        return base

    def data(self, index, role=Qt.ItemDataRole.DisplayRole):
        row = self.rows[index.row()]
        res = row["result"]
        col = index.column()

        if role == Qt.ItemDataRole.UserRole and col == COL_LORAS:
            return res.get("loras", [])

        if role in (Qt.ItemDataRole.DisplayRole, Qt.ItemDataRole.EditRole):
            if col == COL_IMAGE:
                return row["path"]
            if col == COL_POSITIVE:
                return res.get("positive", "")
            if col == COL_NEGATIVE:
                return res.get("negative", "")
            if col == COL_WIDTH:
                return res.get("width", 1024)
            if col == COL_HEIGHT:
                return res.get("height", 1360)
            if col == COL_STEPS:
                return res.get("steps", 30)
            if col == COL_CFG:
                return res.get("cfg", 4.5)
            if col == COL_CHECKPOINT:
                return res.get("checkpoint", "")
            if col == COL_MD5:
                return row.get("md5", "")
            return None

        if role == Qt.ItemDataRole.BackgroundRole:
            if col == COL_CHECKPOINT:
                return self.registry.color_for(res.get("checkpoint", ""),
                                               "checkpoint")
            if col == COL_LORAS:
                colors = []
                for l in res.get("loras", []):
                    if not l.get("enabled", True):
                        continue
                    c = self.registry.color_for(l.get("model", ""), "lora")
                    if c is not None:
                        colors.append(c)
                if any(c == COLOR_BAD for c in colors):
                    return COLOR_BAD
                if any(c == COLOR_WARN for c in colors):
                    return COLOR_WARN
                if any(c == COLOR_REPLACED for c in colors):
                    return COLOR_REPLACED

        if role == Qt.ItemDataRole.ToolTipRole and col in (COL_POSITIVE,
                                                           COL_NEGATIVE):
            return res.get("positive" if col == COL_POSITIVE else "negative",
                           "")

        if role == Qt.ItemDataRole.ToolTipRole and col == COL_MD5:
            return row.get("md5", "")

        return None

    def setData(self, index, value, role=Qt.ItemDataRole.EditRole):
        if role != Qt.ItemDataRole.EditRole:
            return False
        res = self.rows[index.row()]["result"]
        col = index.column()
        try:
            if col == COL_POSITIVE:
                res["positive"] = value
            elif col == COL_NEGATIVE:
                res["negative"] = value
            elif col == COL_WIDTH:
                res["width"] = int(value)
            elif col == COL_HEIGHT:
                res["height"] = int(value)
            elif col == COL_STEPS:
                res["steps"] = int(value)
            elif col == COL_CFG:
                res["cfg"] = float(value)
            elif col == COL_CHECKPOINT:
                res["checkpoint"] = value
            elif col == COL_LORAS:
                res["loras"] = value
            else:
                return False
        except (ValueError, TypeError):
            return False
        self.dataChanged.emit(index, index)
        return True


# ---------------------------------------------------------------------------
# Main window
# ---------------------------------------------------------------------------


class RunFriendlyTableView(QTableView):

    def _active_delegate_editor(self):
        vp = self.viewport()
        w = QApplication.focusWidget()
        while w is not None:
            if w.parentWidget() is vp:
                return w
            w = w.parentWidget()
        return None

    def commit_active_editor(self) -> None:
        editor = self._active_delegate_editor()
        if editor is None:
            return
        self.commitData(editor)
        self.closeEditor(editor, QAbstractItemDelegate.EndEditHint.NoHint)

    def mousePressEvent(self, event):
        pos = event.position().toPoint()
        idx = self.indexAt(pos)
        if idx.isValid() and idx.column() == COL_RUN:
            self.commit_active_editor()
        super().mousePressEvent(event)


class MainWindow(QWidget):

    def __init__(
        self,
        rows: list[dict],
        registry: ModelRegistry,
        workflow_path: Path,
        comfy_url: str,
    ):
        super().__init__()
        self.setWindowTitle("Generation Params Explorer")
        self.registry = registry
        self.workflow_path = workflow_path
        self.comfy_url = comfy_url

        self.model = GenerationModel(rows, registry)
        self.proxy = QSortFilterProxyModel(self)
        self.proxy.setSourceModel(self.model)
        self.proxy.setFilterKeyColumn(COL_POSITIVE)
        self.proxy.setFilterCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)

        layout = QVBoxLayout(self)

        search_row = QHBoxLayout()
        self.search = QLineEdit()
        self.search.setPlaceholderText("Filter by positive prompt…")
        self.search.returnPressed.connect(self._apply_filter)

        apply_filter_btn = QPushButton("Apply filter")
        apply_filter_btn.setFixedHeight(EDITOR_H)
        apply_filter_btn.clicked.connect(self._apply_filter)

        search_row.addWidget(self.search)
        search_row.addWidget(apply_filter_btn)
        layout.addLayout(search_row)

        self.view = RunFriendlyTableView()
        self.view.setModel(self.proxy)
        self.view.setSortingEnabled(True)
        self.view.setEditTriggers(QTableView.DoubleClicked
                                  | QTableView.EditKeyPressed)
        self.view.setWordWrap(True)
        self.view.verticalHeader().setDefaultSectionSize(THUMB + 8)
        self.view.doubleClicked.connect(self._on_double_clicked)
        layout.addWidget(self.view)

        self._setup_delegates()

        header = self.view.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.ResizeMode.Interactive)
        header.setStretchLastSection(False)

        self.view.setColumnWidth(COL_IMAGE, THUMB + 12)
        self.view.setColumnWidth(COL_RUN, RUN_BTN_W + 16)

        for c in [
                COL_WIDTH,
                COL_HEIGHT,
                COL_STEPS,
                COL_CFG,
                COL_CHECKPOINT,
                COL_LORAS,
        ]:
            self.view.resizeColumnToContents(c)

        self.view.setColumnWidth(COL_POSITIVE,
                                 max(self.view.columnWidth(COL_POSITIVE), 320))
        self.view.setColumnWidth(COL_NEGATIVE,
                                 max(self.view.columnWidth(COL_NEGATIVE), 320))

        self.view.setColumnWidth(
            COL_CHECKPOINT,
            max(self.view.columnWidth(COL_CHECKPOINT), MIN_W_CHECKPOINT))
        self.view.setColumnWidth(
            COL_LORAS, max(self.view.columnWidth(COL_LORAS), MIN_W_LORAS))
        self.view.setColumnWidth(COL_MD5, MIN_W_MD5)

        self.resize(1400, 800)

    def _apply_filter(self) -> None:
        self.proxy.setFilterFixedString(self.search.text())

    def _setup_delegates(self) -> None:
        self.image_delegate = ImageDelegate(self.view)
        self.view.setItemDelegateForColumn(COL_IMAGE, self.image_delegate)

        self.view.setItemDelegateForColumn(
            COL_CHECKPOINT,
            ModelComboDelegate(
                self.registry.options("checkpoint"),
                allow_empty=False,
                registry=self.registry,
                kind="checkpoint",
                filter_by_type=True,
                parent=self,
            ),
        )
        self.view.setItemDelegateForColumn(COL_LORAS,
                                           LoraDelegate(self.registry, self))

        self.run_delegate = ButtonDelegate(self.view, self)
        self.run_delegate.clicked.connect(self._on_run)
        self.view.setItemDelegateForColumn(COL_RUN, self.run_delegate)

        self.view.setItemDelegateForColumn(COL_WIDTH,
                                           NumberDelegate(False, self))
        self.view.setItemDelegateForColumn(COL_HEIGHT,
                                           NumberDelegate(False, self))
        self.view.setItemDelegateForColumn(COL_STEPS,
                                           NumberDelegate(False, self))
        self.view.setItemDelegateForColumn(COL_CFG, NumberDelegate(True, self))
        self.view.setItemDelegateForColumn(COL_POSITIVE, PromptDelegate(self))
        self.view.setItemDelegateForColumn(COL_NEGATIVE, PromptDelegate(self))

    def _on_double_clicked(self, proxy_index: QModelIndex) -> None:
        if proxy_index.column() != COL_IMAGE:
            return
        src = self.proxy.mapToSource(proxy_index)
        path = self.model.index(src.row(),
                                COL_IMAGE).data(Qt.ItemDataRole.DisplayRole)
        if not path:
            return
        dlg = ImagePreviewDialog(path, self)
        dlg.exec()

    def _on_run(self, proxy_index: QModelIndex) -> None:
        self.view.commit_active_editor()
        src = self.proxy.mapToSource(proxy_index)
        row = self.model.rows[src.row()]

        ckpt_payload, replaced = self.registry.resolve_with_replacement(
            row["result"].get("checkpoint", ""), "checkpoint")
        if replaced and ckpt_payload in self.registry.replacement_overrides:
            ov = self.registry.replacement_overrides[ckpt_payload]
            if "steps" in ov:
                steps_idx = self.model.index(src.row(), COL_STEPS)
                self.model.setData(steps_idx, int(ov["steps"]),
                                   Qt.ItemDataRole.EditRole)
            if "cfg" in ov:
                cfg_idx = self.model.index(src.row(), COL_CFG)
                self.model.setData(cfg_idx, float(ov["cfg"]),
                                   Qt.ItemDataRole.EditRole)

        self._send_to_comfy(row)

    def _send_to_comfy(self, row: dict) -> None:
        res = dict(row["result"])

        ckpt_payload, _ = self.registry.resolve_with_replacement(
            res.get("checkpoint", ""), "checkpoint")
        ckpt_type = self.registry.type_for_payload(ckpt_payload, "checkpoint")

        vae_value = res.get("vae") or ""
        vae_payload, _ = self.registry.resolve_with_replacement(
            vae_value, "vae")
        if not vae_payload and ckpt_type and ckpt_type in self.registry.default_vae:
            vae_payload = self.registry.default_vae[ckpt_type]

        clip_value = res.get("clip") or ""
        clip_payload = None
        if clip_value:
            clip_payload, _ = self.registry.resolve_with_replacement(
                clip_value, "clip")
        if not clip_payload and ckpt_type and ckpt_type in self.registry.default_clip:
            clip_payload = self.registry.default_clip[ckpt_type]

        active_loras = [
            l for l in res.get("loras", []) if l.get("enabled", True)
        ]

        missing = []
        if ckpt_payload and not self.registry.has(res.get("checkpoint", ""),
                                                  "checkpoint"):
            missing.append(f"checkpoint: {res.get('checkpoint')}")

        normalized_loras = []
        for l in active_loras:
            payload_name, _ = self.registry.resolve_with_replacement(
                l.get("model", ""), "lora")
            if not payload_name:
                continue
            if not self.registry.has(l.get("model", ""), "lora"):
                missing.append(f"lora: {l.get('model')}")
            normalized_loras.append({
                "model": payload_name,
                "weight": float(l.get("weight", 1.0)),
            })

        if vae_payload and not self.registry.is_known_payload(
                vae_payload, "vae"):
            missing.append(f"vae: {vae_value or vae_payload}")

        if clip_payload and not self.registry.is_known_payload(
                clip_payload, "clip"):
            missing.append(f"clip: {clip_value or clip_payload}")

        if missing:
            QMessageBox.critical(
                self,
                "Missing models",
                "These models are not present in the ComfyUI directory:\n\n" +
                "\n".join(missing),
            )
            return

        res["checkpoint"] = ckpt_payload
        res["vae"] = vae_payload or None
        res["clip"] = clip_payload or None
        res["loras"] = normalized_loras
        res["seed"] = random.randint(0, 2**32 - 1)

        payload = {
            "md5": row["md5"],
            "indexer_id": row["indexer_id"],
            "result": res,
        }

        try:
            with open(self.workflow_path, "r", encoding="utf-8") as f:
                workflow = json.load(f)

            target_node = None
            for node in workflow.values():
                if node.get("_meta",
                            {}).get("title") == "input_json_parameter":
                    target_node = node
                    break
            if target_node is None:
                raise RuntimeError(
                    "Could not find node titled input_json_parameter")

            target_node["inputs"]["value"] = json.dumps(payload, indent=2)

            req = urllib.request.Request(
                f"{self.comfy_url}/prompt",
                data=json.dumps({
                    "prompt": workflow
                }).encode("utf-8"),
                headers={"Content-Type": "application/json"},
                method="POST",
            )
            with urllib.request.urlopen(req):
                pass
        except Exception as exc:  # noqa: BLE001
            QMessageBox.critical(self, "Send failed", str(exc))


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def main() -> None:
    parser = argparse.ArgumentParser(description="Generation Params Explorer")
    parser.add_argument(
        "config",
        type=Path,
        help="Path to the JSON configuration file (matches the Config schema).",
    )
    args = parser.parse_args()

    config = Config.model_validate_json(
        args.config.read_text(encoding="utf-8"))

    client = ArangoClient(hosts=config.arango_url)
    db = client.db(config.db_name,
                   username=config.username,
                   password=config.password)

    cursor = db.aql.execute(
        AQL,
        bind_vars={
            "@files": config.files_collection,
            "@roots": config.roots_collection,
        },
    )
    rows = list(cursor)

    arch_cursor = db.aql.execute(
        SAFETENSOR_AQL,
        bind_vars={
            "@safetensor": config.safetensor_collection,
            "@files": config.files_collection,
            "@roots": config.roots_collection,
        },
    )
    arch_info: dict[str, str | None] = {}
    for doc in arch_cursor:
        rel = doc.get("relative")
        if not rel:
            continue
        key = Path(rel).with_suffix("").as_posix()
        arch_info[key] = doc.get("arch")

    registry = ModelRegistry(config.comfy_dir, arch_info, config)

    app = QApplication([])
    window = MainWindow(rows, registry, config.workflow_path, config.comfy_url)
    window.show()
    app.exec()


if __name__ == "__main__":
    main()

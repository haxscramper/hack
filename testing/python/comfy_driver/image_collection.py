#!/usr/bin/env python

import json
import random
import urllib.request
from dataclasses import dataclass
from difflib import SequenceMatcher
from pathlib import Path

import click
from arango import ArangoClient
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
from PySide6.QtGui import QColor, QImage, QPainter, QTextOption
from PySide6.QtWidgets import (
    QAbstractItemDelegate,
    QApplication,
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QDoubleSpinBox,
    QHBoxLayout,
    QHeaderView,
    QLineEdit,
    QMessageBox,
    QPlainTextEdit,
    QPushButton,
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
    COL_VAE,
    COL_LORAS,
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
    "VAE",
    "LoRAs",
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
    COL_VAE,
}

COLOR_WARN = QColor(255, 165, 0)
COLOR_BAD = QColor(220, 80, 80)
COLOR_OK = QColor(70, 150, 90)

LORA_ROW_H = 22
LORA_ON_W = 34
LORA_WEIGHT_W = 84

EDITOR_H = 28
MIN_W_CHECKPOINT = 260
MIN_W_VAE = 200
MIN_W_LORAS = 420

AQL = """
FOR gp IN generation_params
  FILTER gp.result.positive != "" AND gp.result.negative != ""
  LET f = DOCUMENT("files", gp.md5)
  FILTER f != null
  FOR p IN f.paths
    LET root = DOCUMENT(@roots, p.root.name)
    RETURN {
      path: CONCAT(root.path, "/", p.relative),
      md5: gp.md5,
      indexer_id: gp.indexer_id,
      result: gp.result
    }
"""

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
    display: str  # relative path without suffix
    payload: str  # basename without suffix (what is sent to comfy)


def scan_models(comfy_dir: Path, subdir: str) -> list[ModelEntry]:
    base = comfy_dir / "models" / subdir
    out: dict[str, str] = {}
    if base.exists():
        for p in base.rglob("*"):
            if not p.is_file() or p.suffix.lower() not in MODEL_EXTS:
                continue
            rel = p.relative_to(base).with_suffix("").as_posix()
            payload = p.stem
            out[rel] = payload
    return [ModelEntry(display=k, payload=v) for k, v in sorted(out.items())]


class ModelRegistry:

    def __init__(self, comfy_dir: Path):
        self.checkpoints = scan_models(comfy_dir, "checkpoints")
        self.loras = scan_models(comfy_dir, "loras")
        self.vae = scan_models(comfy_dir, "vae")

        self._payload_sets = {
            "checkpoint": {e.payload
                           for e in self.checkpoints},
            "lora": {e.payload
                     for e in self.loras},
            "vae": {e.payload
                    for e in self.vae},
        }
        self._display_to_payload = {
            "checkpoint": {
                e.display: e.payload
                for e in self.checkpoints
            },
            "lora": {
                e.display: e.payload
                for e in self.loras
            },
            "vae": {
                e.display: e.payload
                for e in self.vae
            },
        }

    def options(self, kind: str) -> list[tuple[str, str]]:
        entries = {
            "checkpoint": self.checkpoints,
            "lora": self.loras,
            "vae": self.vae,
        }[kind]
        return [(e.display, e.payload) for e in entries]

    def normalize_payload(self, value: str) -> str:
        if not value:
            return ""
        s = strip_ext(value.strip())
        return Path(s).name

    def resolve_payload(self, value: str, kind: str) -> str:
        if not value:
            return ""
        value = value.strip()
        m = self._display_to_payload[kind]
        if value in m:
            return m[value]
        return self.normalize_payload(value)

    def color_for(self, value: str, kind: str):
        payload = self.resolve_payload(value, kind)
        if not payload:
            return None
        candidates = self._payload_sets[kind]
        if payload in candidates:
            return None
        best = max(
            (SequenceMatcher(None, payload, c).ratio() for c in candidates),
            default=0.0,
        )
        return COLOR_WARN if best >= 0.7 else COLOR_BAD

    def has(self, value: str, kind: str) -> bool:
        payload = self.resolve_payload(value, kind)
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
            img = img.scaled(THUMB, THUMB, Qt.KeepAspectRatio,
                             Qt.SmoothTransformation)
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
        path = index.data(Qt.DisplayRole)
        img = self.cache.get(path)
        if img is None:
            if path not in self.pending:
                self.pending.add(path)
                self.pool.start(ImageLoader(path, self.cache, self.signals))
            painter.drawText(option.rect, Qt.AlignCenter, "loading…")
            return
        if img.isNull():
            painter.drawText(option.rect, Qt.AlignCenter, "no image")
            return
        x = option.rect.x() + (option.rect.width() - img.width()) // 2
        y = option.rect.y() + (option.rect.height() - img.height()) // 2
        painter.drawImage(x, y, img)


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
        parent=None,
    ):
        super().__init__(parent)
        self._options = options
        self._allow_empty = allow_empty
        self._registry = registry
        self._kind = kind

    def _sorted_options(self, default_text: str) -> list[tuple[str, str]]:
        needle = (default_text or "").strip().casefold()

        if not needle:
            return sorted(self._options, key=lambda item: item[0].casefold())

        def similarity(item: tuple[str, str]) -> float:
            display, payload = item
            d = (display or "").casefold()
            p = (payload or "").casefold()
            return max(
                SequenceMatcher(None, needle, d).ratio(),
                SequenceMatcher(None, needle, p).ratio(),
            )

        return sorted(
            self._options,
            key=lambda item: (-similarity(item), item[0].casefold()),
        )

    def createEditor(self, parent, option, index):
        cb = QComboBox(parent)
        cb.setEditable(True)
        cb.setMaxVisibleItems(14)
        cb.setMaximumHeight(EDITOR_H)

        default_text = (index.data(Qt.DisplayRole) or index.data(Qt.EditRole)
                        or "").strip()

        if self._allow_empty:
            cb.addItem("", "")

        for display, payload in self._sorted_options(default_text):
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
            index.data(Qt.EditRole) or "", self._kind)
        for i in range(editor.count()):
            if (editor.itemData(i) or "") == payload:
                editor.setCurrentIndex(i)
                return
        editor.setCurrentText(index.data(Qt.EditRole) or "")

    def setModelData(self, editor, model, index):
        text = editor.currentText().strip()
        payload = editor.currentData()
        if payload:
            model.setData(index, payload, Qt.EditRole)
            return
        model.setData(index, self._registry.resolve_payload(text, self._kind),
                      Qt.EditRole)

    def paint(self, painter, option, index):
        value = (index.data(Qt.DisplayRole) or "").strip()
        text = value if value else "—"

        status = self._registry.color_for(value, self._kind)
        if not value:
            bg = QColor(110, 110, 110, 45)
            fg = option.palette.text().color()
        elif status == COLOR_BAD:
            bg = QColor(220, 80, 80, 60)
            fg = option.palette.text().color()
        elif status == COLOR_WARN:
            bg = QColor(255, 165, 0, 60)
            fg = option.palette.text().color()
        else:
            bg = QColor(70, 150, 90, 50)
            fg = option.palette.text().color()

        r = option.rect.adjusted(6, (option.rect.height() - 22) // 2, -6,
                                 -(option.rect.height() - 22) // 2)

        painter.save()
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)
        painter.setPen(Qt.NoPen)
        painter.setBrush(bg)
        painter.drawRoundedRect(r, 6, 6)
        painter.setPen(fg)
        shown = option.fontMetrics.elidedText(text, Qt.ElideMiddle,
                                              r.width() - 10)
        painter.drawText(r.adjusted(5, 0, -5, 0),
                         Qt.AlignVCenter | Qt.AlignLeft, shown)
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
            0, QHeaderView.Fixed)
        self.table.horizontalHeader().setSectionResizeMode(
            1, QHeaderView.Stretch)
        self.table.horizontalHeader().setSectionResizeMode(
            2, QHeaderView.Fixed)
        self.table.setColumnWidth(0, LORA_ON_W)
        self.table.setColumnWidth(2, LORA_WEIGHT_W)

        self.table.verticalHeader().setVisible(False)
        self.table.verticalHeader().setDefaultSectionSize(LORA_ROW_H)
        self.table.verticalHeader().setSectionResizeMode(QHeaderView.Fixed)

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
        on_item.setFlags(Qt.ItemIsEnabled | Qt.ItemIsUserCheckable
                         | Qt.ItemIsSelectable)
        on_item.setCheckState(
            Qt.Checked if lora.get("enabled", True) else Qt.Unchecked)
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
            enabled = on_item.checkState() == Qt.Checked if on_item else True

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
        return LoraEditorWidget(self._registry.options("lora"), self._registry,
                                parent)

    def setEditorData(self, editor, index):
        editor.set_loras(index.data(Qt.UserRole) or [])

    def setModelData(self, editor, model, index):
        model.setData(index, editor.loras(), Qt.EditRole)

    def paint(self, painter, option, index):
        loras = index.data(Qt.UserRole) or []
        if not loras:
            painter.drawText(option.rect, Qt.AlignCenter, "—")
            return

        painter.save()
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)

        r = option.rect.adjusted(4, 4, -4, -4)
        row_h = 22

        # Fixed "table-like" columns, no wrapping/jiggling.
        on_w = 34
        weight_w = 84
        model_w = max(0, r.width() - on_w - weight_w)

        x_on = r.x()
        x_model = x_on + on_w
        x_weight = x_model + model_w

        # Draw outer border once.
        painter.setPen(option.palette.mid().color())
        painter.setBrush(Qt.NoBrush)
        painter.drawRect(r)

        for i, lora in enumerate(loras):
            y = r.y() + i * row_h
            if y >= r.bottom():
                break  # deterministic clipping; no reflow/reorder/no ellipsis markers

            row_rect = r.__class__(r.x(), y, r.width(),
                                   min(row_h,
                                       r.bottom() - y + 1))

            enabled = bool(lora.get("enabled", True))
            model = str(lora.get("model", "") or "")
            weight = float(lora.get("weight", 1.0))

            if not enabled:
                bg = QColor(120, 120, 120, 35)
            else:
                status = self._registry.color_for(model, "lora")
                if status == COLOR_BAD:
                    bg = QColor(220, 80, 80, 55)
                elif status == COLOR_WARN:
                    bg = QColor(255, 165, 0, 55)
                else:
                    bg = QColor(70, 150, 90, 45)

            painter.fillRect(row_rect, bg)

            # Horizontal row separator
            painter.setPen(option.palette.mid().color())
            painter.drawLine(r.x(), y, r.right(), y)

            # Vertical column separators
            painter.drawLine(x_model, y, x_model, y + row_rect.height() - 1)
            painter.drawLine(x_weight, y, x_weight, y + row_rect.height() - 1)

            # On
            painter.setPen(option.palette.text().color())
            on_text = "☑" if enabled else "☐"
            painter.drawText(
                r.__class__(x_on + 4, y, on_w - 8, row_rect.height()),
                Qt.AlignVCenter | Qt.AlignHCenter,
                on_text,
            )

            # LoRA model: draw full text in fixed column, no eliding
            painter.drawText(
                r.__class__(x_model + 6, y, model_w - 12, row_rect.height()),
                Qt.AlignVCenter | Qt.AlignLeft | Qt.TextSingleLine,
                model,
            )

            # Weight
            painter.drawText(
                r.__class__(x_weight + 6, y, weight_w - 10, row_rect.height()),
                Qt.AlignVCenter | Qt.AlignRight | Qt.TextSingleLine,
                f"{weight:g}",
            )

        painter.restore()


class PromptEdit(QPlainTextEdit):

    def __init__(self, text: str, parent=None):
        super().__init__(parent)
        self.setPlainText(text)
        self.setWordWrapMode(QTextOption.WrapMode.WordWrap)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)


class PromptDelegate(QStyledItemDelegate):

    def createEditor(self, parent, option, index):
        return PromptEdit(index.data(Qt.EditRole) or "", parent)

    def setEditorData(self, editor, index):
        editor.setPlainText(index.data(Qt.EditRole) or "")

    def setModelData(self, editor, model, index):
        model.setData(index, editor.toPlainText(), Qt.EditRole)

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
        sb.setMaximumHeight(EDITOR_H)
        sb.setFixedHeight(EDITOR_H)
        return sb

    def updateEditorGeometry(self, editor, option, index):
        y = option.rect.y() + (option.rect.height() - EDITOR_H) // 2
        editor.setGeometry(option.rect.x(), y, max(0, option.rect.width()),
                           EDITOR_H)

    def setEditorData(self, editor, index):
        editor.setValue(index.data(Qt.EditRole))

    def setModelData(self, editor, model, index):
        model.setData(index, editor.value(), Qt.EditRole)


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
        opt.state = QStyle.State_Enabled
        if (self._pressed.isValid() and self._pressed.row() == index.row()
                and self._pressed.column() == index.column()):
            opt.state |= QStyle.State_Sunken
        else:
            opt.state |= QStyle.State_Raised
        QApplication.style().drawControl(QStyle.CE_PushButton, opt, painter)

    def editorEvent(self, event, model, option, index):
        br = self._button_rect(option.rect)

        if event.type() == QEvent.MouseButtonPress and br.contains(
                event.pos()):
            self._pressed = QPersistentModelIndex(index)
            self._view.viewport().update(option.rect)
            return True

        if event.type() == QEvent.MouseButtonRelease:
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

    def rowCount(self, parent=QModelIndex()) -> int:
        return 0 if parent.isValid() else len(self.rows)

    def columnCount(self, parent=QModelIndex()) -> int:
        return len(HEADERS)

    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.DisplayRole and orientation == Qt.Horizontal:
            return HEADERS[section]
        return None

    def flags(self, index):
        base = Qt.ItemIsEnabled | Qt.ItemIsSelectable
        if index.column() in EDITABLE_COLS | {COL_LORAS}:
            return base | Qt.ItemIsEditable
        return base

    def data(self, index, role=Qt.DisplayRole):
        row = self.rows[index.row()]
        res = row["result"]
        col = index.column()

        if role == Qt.UserRole and col == COL_LORAS:
            return res.get("loras", [])

        if role in (Qt.DisplayRole, Qt.EditRole):
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
            if col == COL_VAE:
                return res.get("vae") or ""
            return None

        if role == Qt.BackgroundRole:
            if col == COL_CHECKPOINT:
                return self.registry.color_for(res.get("checkpoint", ""),
                                               "checkpoint")
            if col == COL_VAE:
                return self.registry.color_for(res.get("vae") or "", "vae")
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

        if role == Qt.ToolTipRole and col in (COL_POSITIVE, COL_NEGATIVE):
            return res.get("positive" if col == COL_POSITIVE else "negative",
                           "")

        return None

    def setData(self, index, value, role=Qt.EditRole):
        if role != Qt.EditRole:
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
            elif col == COL_VAE:
                res["vae"] = value or None
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
        self.closeEditor(editor, QAbstractItemDelegate.NoHint)

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
        self.proxy.setFilterCaseSensitivity(Qt.CaseInsensitive)

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
        layout.addWidget(self.view)

        self._setup_delegates()

        header = self.view.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.Interactive)
        header.setStretchLastSection(False)

        self.view.setColumnWidth(COL_IMAGE, THUMB + 12)
        self.view.setColumnWidth(COL_RUN, RUN_BTN_W + 16)

        for c in [
                COL_WIDTH,
                COL_HEIGHT,
                COL_STEPS,
                COL_CFG,
                COL_CHECKPOINT,
                COL_VAE,
                COL_LORAS,
        ]:
            self.view.resizeColumnToContents(c)

        # keep prompt columns user-resizable too
        self.view.setColumnWidth(COL_POSITIVE,
                                 max(self.view.columnWidth(COL_POSITIVE), 320))
        self.view.setColumnWidth(COL_NEGATIVE,
                                 max(self.view.columnWidth(COL_NEGATIVE), 320))

        self.view.setColumnWidth(
            COL_CHECKPOINT,
            max(self.view.columnWidth(COL_CHECKPOINT), MIN_W_CHECKPOINT))
        self.view.setColumnWidth(
            COL_VAE, max(self.view.columnWidth(COL_VAE), MIN_W_VAE))
        self.view.setColumnWidth(
            COL_LORAS, max(self.view.columnWidth(COL_LORAS), MIN_W_LORAS))

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
                parent=self,
            ),
        )
        self.view.setItemDelegateForColumn(
            COL_VAE,
            ModelComboDelegate(
                self.registry.options("vae"),
                allow_empty=True,
                registry=self.registry,
                kind="vae",
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

    def _on_run(self, proxy_index: QModelIndex) -> None:
        self.view.commit_active_editor()
        src = self.proxy.mapToSource(proxy_index)
        self._send_to_comfy(self.model.rows[src.row()])

    def _send_to_comfy(self, row: dict) -> None:
        res = dict(row["result"])

        checkpoint_payload = self.registry.resolve_payload(
            res.get("checkpoint", ""), "checkpoint")
        vae_payload = self.registry.resolve_payload(
            res.get("vae", "") or "", "vae")
        active_loras = [
            l for l in res.get("loras", []) if l.get("enabled", True)
        ]

        missing = []
        if checkpoint_payload and not self.registry.has(
                checkpoint_payload, "checkpoint"):
            missing.append(f"checkpoint: {res.get('checkpoint')}")

        normalized_loras = []
        for l in active_loras:
            payload_name = self.registry.resolve_payload(
                l.get("model", ""), "lora")
            if not payload_name:
                continue
            if not self.registry.has(payload_name, "lora"):
                missing.append(f"lora: {l.get('model')}")
            normalized_loras.append({
                "model": payload_name,
                "weight": float(l.get("weight", 1.0)),
            })

        if vae_payload and not self.registry.has(vae_payload, "vae"):
            missing.append(f"vae: {res.get('vae')}")

        if missing:
            QMessageBox.critical(
                self,
                "Missing models",
                "These models are not present in the ComfyUI directory:\n\n" +
                "\n".join(missing),
            )
            return

        res["checkpoint"] = checkpoint_payload
        res["vae"] = vae_payload or None
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
# CLI
# ---------------------------------------------------------------------------


@click.command()
@click.option(
    "--comfy-dir",
    required=True,
    type=click.Path(exists=True, file_okay=False, path_type=Path),
    help="Path to the local ComfyUI installation.",
)
@click.option(
    "--workflow",
    "workflow_path",
    required=True,
    type=click.Path(exists=True, dir_okay=False, path_type=Path),
    help="Path to the saved API-format workflow JSON.",
)
@click.option("--comfy-url", default="http://127.0.0.1:8188")
@click.option("--arango-url", default="http://127.0.0.1:8529")
@click.option("--db", "db_name", default="test_index")
@click.option("--username", default="root")
@click.option("--password", default="test")
@click.option("--roots-collection", default="roots")
def main(
    comfy_dir: Path,
    workflow_path: Path,
    comfy_url: str,
    arango_url: str,
    db_name: str,
    username: str,
    password: str,
    roots_collection: str,
) -> None:
    client = ArangoClient(hosts=arango_url)
    db = client.db(db_name, username=username, password=password)
    cursor = db.aql.execute(AQL, bind_vars={"roots": roots_collection})
    rows = list(cursor)

    registry = ModelRegistry(comfy_dir)

    app = QApplication([])
    window = MainWindow(rows, registry, workflow_path, comfy_url)
    window.show()
    app.exec()


if __name__ == "__main__":
    main()

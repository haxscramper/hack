#!/usr/bin/env python

import json
import random
import urllib.request
from difflib import SequenceMatcher
from pathlib import Path

import click
from arango import ArangoClient
from PySide6.QtCore import (
    QAbstractTableModel,
    QEvent,
    QModelIndex,
    QObject,
    QRunnable,
    QSize,
    QSortFilterProxyModel,
    Qt,
    QThreadPool,
    Signal,
)
from PySide6.QtGui import QColor, QImage
from PySide6.QtWidgets import (
    QApplication,
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QDoubleSpinBox,
    QHBoxLayout,
    QHeaderView,
    QLineEdit,
    QMessageBox,
    QPushButton,
    QStyle,
    QStyledItemDelegate,
    QStyleOptionButton,
    QTableView,
    QTableWidget,
    QVBoxLayout,
    QWidget,
)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

MODEL_EXTS = {".safetensors", ".ckpt", ".pt", ".pth", ".bin"}
IMAGE_EXTS = (".png", ".jpg", ".jpeg", ".webp", ".bmp")
THUMB = 128

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


def scan_models(comfy_dir: Path, subdir: str) -> list[str]:
    base = comfy_dir / "models" / subdir
    out: set[str] = set()
    if base.exists():
        for p in base.rglob("*"):
            if p.is_file() and p.suffix.lower() in MODEL_EXTS:
                out.add(p.stem)
    return sorted(out)


class ModelRegistry:

    def __init__(self, comfy_dir: Path):
        self.checkpoints = scan_models(comfy_dir, "checkpoints")
        self.loras = scan_models(comfy_dir, "loras")
        self.vae = scan_models(comfy_dir, "vae")
        self._ckpt_set = set(self.checkpoints)
        self._lora_set = set(self.loras)
        self._vae_set = set(self.vae)

    def color_for(self, value: str, kind: str):
        if not value:
            return None
        name = strip_ext(value)
        candidates = {
            "checkpoint": self.checkpoints,
            "lora": self.loras,
            "vae": self.vae,
        }[kind]
        if name in candidates:
            return None
        best = max(
            (SequenceMatcher(None, name, c).ratio() for c in candidates),
            default=0.0,
        )
        return COLOR_WARN if best >= 0.7 else COLOR_BAD

    def has(self, value: str, kind: str) -> bool:
        name = strip_ext(value)
        return (name in {
            "checkpoint": self._ckpt_set,
            "lora": self._lora_set,
            "vae": self._vae_set,
        }[kind])


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


class ComboDelegate(QStyledItemDelegate):

    def __init__(self, items: list[str], allow_empty: bool, parent=None):
        super().__init__(parent)
        self.items = ([""] if allow_empty else []) + items

    def createEditor(self, parent, option, index):
        cb = QComboBox(parent)
        cb.setEditable(True)
        cb.addItems(self.items)
        return cb

    def setEditorData(self, editor, index):
        editor.setCurrentText(index.data(Qt.EditRole) or "")

    def setModelData(self, editor, model, index):
        model.setData(index, editor.currentText(), Qt.EditRole)


class LoraDelegate(QStyledItemDelegate):

    def paint(self, painter, option, index):
        loras = index.data(Qt.UserRole) or []
        if not loras:
            painter.drawText(option.rect, Qt.AlignCenter, "—")
            return
        line_h = max(14, option.rect.height() // max(len(loras), 1))
        y = option.rect.y()
        for lora in loras:
            text = f"{lora.get('model', '')}: {lora.get('weight', 1.0):g}"
            painter.drawText(
                option.rect.x() + 4,
                y,
                option.rect.width() - 8,
                line_h,
                Qt.AlignVCenter | Qt.AlignLeft,
                text,
            )
            y += line_h


class ButtonDelegate(QStyledItemDelegate):
    clicked = Signal(QModelIndex)

    def paint(self, painter, option, index):
        opt = QStyleOptionButton()
        opt.rect = option.rect.adjusted(4, 4, -4, -4)
        opt.text = "Run"
        opt.state = QStyle.State_Enabled | QStyle.State_Raised
        QApplication.style().drawControl(QStyle.CE_PushButton, opt, painter)

    def editorEvent(self, event, model, option, index):
        if event.type() == QEvent.MouseButtonRelease and option.rect.contains(
                event.pos()):
            self.clicked.emit(index)
            return True
        return False


# ---------------------------------------------------------------------------
# Lora edit dialog
# ---------------------------------------------------------------------------


class LoraDialog(QDialog):

    def __init__(self, loras: list[dict], choices: list[str], parent=None):
        super().__init__(parent)
        self.setWindowTitle("Edit LoRAs")
        self.choices = choices
        layout = QVBoxLayout(self)

        self.table = QTableWidget(0, 2)
        self.table.setHorizontalHeaderLabels(["Model", "Weight"])
        self.table.horizontalHeader().setSectionResizeMode(
            0, QHeaderView.Stretch)
        layout.addWidget(self.table)

        row = QHBoxLayout()
        add = QPushButton("Add")
        rem = QPushButton("Remove")
        add.clicked.connect(lambda: self._add_row())
        rem.clicked.connect(self._remove_row)
        row.addWidget(add)
        row.addWidget(rem)
        row.addStretch()
        layout.addLayout(row)

        buttons = QDialogButtonBox(QDialogButtonBox.Ok
                                   | QDialogButtonBox.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

        for lora in loras:
            self._add_row(lora.get("model", ""), lora.get("weight", 1.0))

        self.resize(420, 280)

    def _add_row(self, model: str = "", weight: float = 1.0) -> None:
        r = self.table.rowCount()
        self.table.insertRow(r)
        cb = QComboBox()
        cb.setEditable(True)
        cb.addItems(self.choices)
        cb.setCurrentText(model)
        self.table.setCellWidget(r, 0, cb)
        sb = QDoubleSpinBox()
        sb.setRange(-10.0, 10.0)
        sb.setSingleStep(0.05)
        sb.setValue(weight)
        self.table.setCellWidget(r, 1, sb)

    def _remove_row(self) -> None:
        r = self.table.currentRow()
        if r >= 0:
            self.table.removeRow(r)

    def result_loras(self) -> list[dict]:
        out = []
        for r in range(self.table.rowCount()):
            model = self.table.cellWidget(r, 0).currentText().strip()
            if model:
                out.append({
                    "model": model,
                    "weight": self.table.cellWidget(r, 1).value()
                })
        return out


# ---------------------------------------------------------------------------
# Table model
# ---------------------------------------------------------------------------


class GenerationModel(QAbstractTableModel):

    def __init__(self, rows: list[dict], registry: ModelRegistry):
        super().__init__()
        self.rows = rows
        self.registry = registry

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
        if index.column() in EDITABLE_COLS:
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
                return res.get("width", 0)
            if col == COL_HEIGHT:
                return res.get("height", 0)
            if col == COL_STEPS:
                return res.get("steps", 0)
            if col == COL_CFG:
                return res.get("cfg", 0.0)
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

        self.search = QLineEdit()
        self.search.setPlaceholderText("Filter by positive prompt…")
        self.search.textChanged.connect(self.proxy.setFilterFixedString)
        layout.addWidget(self.search)

        self.view = QTableView()
        self.view.setModel(self.proxy)
        self.view.setSortingEnabled(True)
        self.view.setEditTriggers(QTableView.DoubleClicked
                                  | QTableView.EditKeyPressed)
        self.view.setWordWrap(True)
        self.view.verticalHeader().setDefaultSectionSize(THUMB + 8)
        layout.addWidget(self.view)

        self._setup_delegates()
        self.view.doubleClicked.connect(self._on_double_click)

        header = self.view.horizontalHeader()
        header.setSectionResizeMode(COL_POSITIVE, QHeaderView.Stretch)
        header.setSectionResizeMode(COL_NEGATIVE, QHeaderView.Stretch)
        self.view.setColumnWidth(COL_IMAGE, THUMB + 12)
        self.view.setColumnWidth(COL_LORAS, 180)

        self.resize(1400, 800)

    def _setup_delegates(self) -> None:
        self.image_delegate = ImageDelegate(self.view)
        self.view.setItemDelegateForColumn(COL_IMAGE, self.image_delegate)

        self.view.setItemDelegateForColumn(
            COL_CHECKPOINT,
            ComboDelegate(self.registry.checkpoints,
                          allow_empty=False,
                          parent=self),
        )
        self.view.setItemDelegateForColumn(
            COL_VAE,
            ComboDelegate(self.registry.vae, allow_empty=True, parent=self),
        )
        self.view.setItemDelegateForColumn(COL_LORAS, LoraDelegate(self))

        self.run_delegate = ButtonDelegate(self)
        self.run_delegate.clicked.connect(self._on_run)
        self.view.setItemDelegateForColumn(COL_RUN, self.run_delegate)

    def _on_double_click(self, proxy_index: QModelIndex) -> None:
        if proxy_index.column() != COL_LORAS:
            return
        src = self.proxy.mapToSource(proxy_index)
        res = self.model.rows[src.row()]["result"]
        dlg = LoraDialog(res.get("loras", []), self.registry.loras, self)
        if dlg.exec() == QDialog.Accepted:
            self.model.setData(src, dlg.result_loras(), Qt.EditRole)

    def _on_run(self, proxy_index: QModelIndex) -> None:
        src = self.proxy.mapToSource(proxy_index)
        self._send_to_comfy(self.model.rows[src.row()])

    def _send_to_comfy(self, row: dict) -> None:
        res = dict(row["result"])

        missing = []
        if not self.registry.has(res.get("checkpoint", ""), "checkpoint"):
            missing.append(f"checkpoint: {res.get('checkpoint')}")
        for lora in res.get("loras", []):
            if not self.registry.has(lora.get("model", ""), "lora"):
                missing.append(f"lora: {lora.get('model')}")
        if res.get("vae") and not self.registry.has(res["vae"], "vae"):
            missing.append(f"vae: {res['vae']}")

        if missing:
            QMessageBox.critical(
                self,
                "Missing models",
                "These models are not present in the ComfyUI directory:\n\n" +
                "\n".join(missing),
            )
            return

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
            with urllib.request.urlopen(req) as resp:
                body = resp.read().decode("utf-8")
            QMessageBox.information(self, "Submitted", body)
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

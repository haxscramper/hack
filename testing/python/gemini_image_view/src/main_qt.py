import os
import sys
import uuid
import yaml
import mimetypes
import traceback
import logging
from datetime import datetime
from pathlib import Path
from typing import List, Optional

from pydantic import BaseModel, Field

from PySide6.QtCore import (
    Qt,
    QSize,
    Signal,
    QObject,
    QRunnable,
    QThreadPool,
    QPoint,
    QRect,
    QModelIndex,
)
from PySide6.QtGui import (
    QAction,
    QPixmap,
    QImage,
    QPainter,
    QColor,
    QPen,
    QFont,
    QStandardItemModel,
    QStandardItem,
)
from PySide6.QtWidgets import (
    QApplication,
    QMainWindow,
    QWidget,
    QFileDialog,
    QLabel,
    QPushButton,
    QTextEdit,
    QComboBox,
    QSpinBox,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QHBoxLayout,
    QVBoxLayout,
    QGridLayout,
    QScrollArea,
    QMessageBox,
    QFrame,
    QSplitter,
    QDialog,
    QSizePolicy,
    QFormLayout,
    QStyledItemDelegate,
    QListView,
    QLayout,
    QStyle,
)

from google import genai
from google.genai import types


APP_NAME = "gemini-image-qt"
APP_AUTHOR = "local"


IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".webp", ".bmp", ".gif"}


def xdg_config_dir() -> Path:
    base = os.environ.get("XDG_CONFIG_HOME")
    if base:
        return Path(base) / APP_NAME
    return Path.home() / ".config" / APP_NAME


def xdg_data_dir() -> Path:
    base = os.environ.get("XDG_DATA_HOME")
    if base:
        return Path(base) / APP_NAME
    return Path.home() / ".local" / "share" / APP_NAME


CONFIG_DIR = xdg_config_dir()
DATA_DIR = xdg_data_dir()
CONFIG_DIR.mkdir(parents=True, exist_ok=True)
DATA_DIR.mkdir(parents=True, exist_ok=True)

STATE_FILE = CONFIG_DIR / "state.yaml"


class RequestResult(BaseModel):
    input_images: List[str] = Field(default_factory=list)
    output_image: Optional[str] = None
    response_text: str = ""

class GenerationResult(BaseModel):
    requests: List[RequestResult] = Field(default_factory=list)
    run_index: Optional[int] = None
    batch_id: Optional[str] = None

class HistoryEntry(BaseModel):
    timestamp: str
    prompt: str
    prompt_mode: str
    aspect_ratio: str
    image_size: str
    results: List[GenerationResult] = Field(default_factory=list)

class AppState(BaseModel):
    input_directories: List[str] = Field(default_factory=list)
    output_directory: str = str(Path.home() / "Pictures" / "gemini-output")
    prompt: str = ""
    prompt_mode: str = "all images"
    repetitions: int = 1
    aspect_ratio: str = "1:1"
    image_size: str = "1K"
    history: List[HistoryEntry] = Field(default_factory=list)


def load_state() -> AppState:
    if STATE_FILE.exists():
        try:
            with open(STATE_FILE, "r", encoding="utf-8") as f:
                data = yaml.safe_load(f) or {}
            
            if "history" in data:
                new_history = []
                for entry in data["history"]:
                    if isinstance(entry, dict) and "results" not in entry:
                        req_results = []
                        out_images = entry.get("output_images", [])
                        in_images = entry.get("input_images", [])
                        
                        # Best effort mapping for old data
                        if len(out_images) >= len(in_images) and len(in_images) > 0:
                            reps = len(out_images) // len(in_images)
                            for i, in_img in enumerate(in_images):
                                for r in range(reps):
                                    out_idx = i * reps + r
                                    req_results.append(RequestResult(
                                        input_images=[in_img],
                                        output_image=out_images[out_idx] if out_idx < len(out_images) else None,
                                        response_text=entry.get("response_text", "")
                                    ))
                        else:
                            for out_img in out_images:
                                req_results.append(RequestResult(
                                    input_images=in_images,
                                    output_image=out_img,
                                    response_text=entry.get("response_text", "")
                                ))
                        
                        res = GenerationResult(requests=req_results)
                        new_entry = HistoryEntry(
                            timestamp=entry.get("timestamp", ""),
                            prompt=entry.get("prompt", ""),
                            prompt_mode=entry.get("prompt_mode", ""),
                            aspect_ratio=entry.get("aspect_ratio", ""),
                            image_size=entry.get("image_size", ""),
                            results=[res]
                        )
                        new_history.append(new_entry.model_dump())
                    else:
                        new_history.append(entry)
                data["history"] = new_history

            logging.info("State loaded successfully from %s", STATE_FILE)
            return AppState(**data)
        except Exception:
            logging.error("Failed to load state from %s", STATE_FILE, exc_info=True)
            traceback.print_exc()
    else:
        logging.info("State file %s does not exist, using default state", STATE_FILE)
    return AppState()


def save_state(state: AppState):
    try:
        with open(STATE_FILE, "w", encoding="utf-8") as f:
            yaml.safe_dump(state.model_dump(), f, sort_keys=False, allow_unicode=True)
        logging.info("State saved successfully to %s", STATE_FILE)
    except Exception:
        logging.error("Failed to save state to %s", STATE_FILE, exc_info=True)
        traceback.print_exc()


def is_image_file(path: Path) -> bool:
    return path.is_file() and path.suffix.lower() in IMAGE_EXTENSIONS


def scan_images(directories: List[str]) -> List[Path]:
    result = []
    seen = set()
    logging.info("Scanning directories for images: %s", directories)
    for d in directories:
        p = Path(d)
        if not p.exists() or not p.is_dir():
            logging.warning("Directory does not exist or is not a directory: %s", d)
            continue
        for file in sorted(p.rglob("*")):
            if is_image_file(file):
                rp = str(file.resolve())
                if rp not in seen:
                    seen.add(rp)
                    result.append(file)
    logging.info("Found %d images", len(result))
    return result


class FlowLayout(QLayout):
    def __init__(self, parent=None, margin=-1, hSpacing=-1, vSpacing=-1):
        super().__init__(parent)
        self._item_list = []
        if margin >= 0:
            self.setContentsMargins(margin, margin, margin, margin)
        self._h_space = hSpacing
        self._v_space = vSpacing

    def __del__(self):
        item = self.takeAt(0)
        while item:
            item = self.takeAt(0)

    def addItem(self, item):
        self._item_list.append(item)

    def horizontalSpacing(self):
        if self._h_space >= 0:
            return self._h_space
        return self.smartSpacing(QStyle.PixelMetric.PM_LayoutHorizontalSpacing)

    def verticalSpacing(self):
        if self._v_space >= 0:
            return self._v_space
        return self.smartSpacing(QStyle.PixelMetric.PM_LayoutVerticalSpacing)

    def count(self):
        return len(self._item_list)

    def itemAt(self, index):
        if 0 <= index < len(self._item_list):
            return self._item_list[index]
        return None

    def takeAt(self, index):
        if 0 <= index < len(self._item_list):
            return self._item_list.pop(index)
        return None

    def expandingDirections(self):
        return Qt.Orientation(0)

    def hasHeightForWidth(self):
        return True

    def heightForWidth(self, width):
        return self.doLayout(QRect(0, 0, width, 0), True)

    def setGeometry(self, rect):
        super().setGeometry(rect)
        self.doLayout(rect, False)

    def sizeHint(self):
        return self.minimumSize()

    def minimumSize(self):
        size = QSize()
        for item in self._item_list:
            size = size.expandedTo(item.minimumSize())
        margins = self.contentsMargins()
        size += QSize(margins.left() + margins.right(), margins.top() + margins.bottom())
        return size

    def smartSpacing(self, pm):
        parent = self.parent()
        if not parent:
            return -1
        elif parent.isWidgetType():
            return parent.style().pixelMetric(pm, None, parent)
        else:
            return parent.spacing()

    def doLayout(self, rect, testOnly):
        x = rect.x()
        y = rect.y()
        lineHeight = 0

        for item in self._item_list:
            wid = item.widget()
            spaceX = self.horizontalSpacing()
            if spaceX == -1:
                spaceX = wid.style().layoutSpacing(QSizePolicy.ControlType.PushButton, QSizePolicy.ControlType.PushButton, Qt.Orientation.Horizontal) if wid else 10
            spaceY = self.verticalSpacing()
            if spaceY == -1:
                spaceY = wid.style().layoutSpacing(QSizePolicy.ControlType.PushButton, QSizePolicy.ControlType.PushButton, Qt.Orientation.Vertical) if wid else 10

            nextX = x + item.sizeHint().width() + spaceX
            if nextX - spaceX > rect.right() and lineHeight > 0:
                x = rect.x()
                y = y + lineHeight + spaceY
                nextX = x + item.sizeHint().width() + spaceX
                lineHeight = 0

            if not testOnly:
                item.setGeometry(QRect(QPoint(x, y), item.sizeHint()))

            x = nextX
            lineHeight = max(lineHeight, item.sizeHint().height())

        return y + lineHeight - rect.y() + self.contentsMargins().bottom()


class ThumbnailDelegate(QStyledItemDelegate):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.thumb_size = QSize(160, 160)
        self.cache = {}

    def get_thumbnail(self, path: str) -> QPixmap:
        if path in self.cache:
            return self.cache[path]

        pix = QPixmap(path)
        if pix.isNull():
            return QPixmap()

        scaled = pix.scaled(
            self.thumb_size - QSize(6, 6),
            Qt.KeepAspectRatio,
            Qt.SmoothTransformation,
        )
        canvas = QPixmap(self.thumb_size)
        canvas.fill(QColor("#202020"))
        painter = QPainter(canvas)
        x = (self.thumb_size.width() - scaled.width()) // 2
        y = (self.thumb_size.height() - scaled.height()) // 2
        painter.drawPixmap(x, y, scaled)
        painter.end()

        self.cache[path] = canvas
        return canvas

    def paint(self, painter, option, index):
        painter.save()
        path = index.data(Qt.ItemDataRole.UserRole)
        order = index.data(Qt.ItemDataRole.UserRole + 1)

        # Background
        painter.fillRect(option.rect, QColor("#1e1e1e"))

        # Draw Thumbnail
        thumbnail = self.get_thumbnail(path)
        if not thumbnail.isNull():
            thumb_rect = QRect(
                option.rect.x() + (option.rect.width() - self.thumb_size.width()) // 2,
                option.rect.y() + 5,
                self.thumb_size.width(),
                self.thumb_size.height(),
            )
            painter.drawPixmap(thumb_rect.topLeft(), thumbnail)

            # Selection Border
            if order is not None:
                pen = QPen(QColor(20, 120, 255))
                pen.setWidth(3)
                painter.setPen(pen)
                painter.drawRect(thumb_rect)

        # Draw File Name
        name_rect = QRect(
            option.rect.x(),
            option.rect.y() + self.thumb_size.height() + 5,
            option.rect.width(),
            option.rect.height() - self.thumb_size.height() - 5,
        )
        painter.setPen(Qt.white)
        font = painter.font()
        font.setPointSize(9)
        painter.setFont(font)
        painter.drawText(
            name_rect,
            int(Qt.AlignmentFlag.AlignCenter) | int(Qt.TextFlag.TextWordWrap),
            Path(path).name,
        )

        # Selection Badge
        if order is not None:
            painter.setRenderHint(QPainter.RenderHint.Antialiasing)
            badge_rect = QRect(option.rect.x() + 10, option.rect.y() + 10, 28, 28)
            painter.setBrush(QColor(20, 120, 255, 220))
            painter.setPen(Qt.PenStyle.NoPen)
            painter.drawRoundedRect(badge_rect, 6, 6)

            painter.setPen(QColor(Qt.GlobalColor.white))
            font = QFont()
            font.setBold(True)
            painter.setFont(font)
            painter.drawText(badge_rect, int(Qt.AlignmentFlag.AlignCenter), str(order))

        painter.restore()

    def sizeHint(self, option, index):
        return QSize(170, 200)


class ScaledImageLabel(QLabel):
    def __init__(self, pixmap, parent=None):
        super().__init__(parent)
        self.original_pixmap = pixmap
        self.setAlignment(Qt.AlignCenter)
        self.setMinimumSize(1, 1)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        if not self.original_pixmap.isNull():
            scaled = self.original_pixmap.scaled(
                event.size(), Qt.KeepAspectRatio, Qt.SmoothTransformation
            )
            super().setPixmap(scaled)

class PreviewDialog(QDialog):
    def __init__(self, image_path: str, parent=None):
        super().__init__(parent)
        self.setWindowTitle(Path(image_path).name)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        pix = QPixmap(image_path)
        if pix.isNull():
            label = QLabel("Could not load image.")
            label.setAlignment(Qt.AlignCenter)
            layout.addWidget(label)
            self.resize(400, 300)
            return

        label = ScaledImageLabel(pix)
        layout.addWidget(label)

        screen = QApplication.primaryScreen().availableGeometry()
        w = pix.width()
        h = pix.height()

        max_w = int(screen.width() * 0.9)
        max_h = int(screen.height() * 0.9)

        if w > max_w or h > max_h:
            scaled = pix.scaled(max_w, max_h, Qt.KeepAspectRatio, Qt.SmoothTransformation)
            self.resize(scaled.width(), scaled.height())
        else:
            self.resize(w, h)


class ClickableImageLabel(QLabel):
    openRequested = Signal(str)
    selectRequested = Signal(str)

    def __init__(
        self, image_path: str, thumb_size: QSize = QSize(160, 160), parent=None
    ):
        super().__init__(parent)
        self.image_path = image_path
        self.thumb_size = thumb_size
        self.selection_order: Optional[int] = None
        self.setFixedSize(thumb_size)
        self.setAlignment(Qt.AlignCenter)
        self.setFrameShape(QFrame.Box)
        self.setLineWidth(1)
        self.setStyleSheet("background: #222; color: white;")
        self.refresh_pixmap()

    def refresh_pixmap(self):
        pix = QPixmap(self.image_path)
        if pix.isNull():
            self.setText("Invalid image")
            return
        scaled = pix.scaled(
            self.thumb_size - QSize(6, 6),
            Qt.KeepAspectRatio,
            Qt.SmoothTransformation,
        )
        canvas = QPixmap(self.thumb_size)
        canvas.fill(QColor("#202020"))
        painter = QPainter(canvas)
        x = (self.thumb_size.width() - scaled.width()) // 2
        y = (self.thumb_size.height() - scaled.height()) // 2
        painter.drawPixmap(x, y, scaled)

        if self.selection_order is not None:
            painter.setRenderHint(QPainter.Antialiasing)
            rect_w = 28
            rect_h = 28
            painter.setBrush(QColor(20, 120, 255, 220))
            painter.setPen(Qt.NoPen)
            painter.drawRoundedRect(4, 4, rect_w, rect_h, 6, 6)

            painter.setPen(QPen(Qt.white))
            font = QFont()
            font.setBold(True)
            painter.setFont(font)
            painter.drawText(
                4, 4, rect_w, rect_h, Qt.AlignCenter, str(self.selection_order)
            )

            pen = QPen(QColor(20, 120, 255))
            pen.setWidth(3)
            painter.setPen(pen)
            painter.setBrush(Qt.NoBrush)
            painter.drawRect(
                1, 1, self.thumb_size.width() - 2, self.thumb_size.height() - 2
            )

        painter.end()
        self.setPixmap(canvas)

    def set_selection_order(self, order: Optional[int]):
        self.selection_order = order
        self.refresh_pixmap()

    def mousePressEvent(self, event):
        if event.button() == Qt.LeftButton:
            self.selectRequested.emit(self.image_path)
        super().mousePressEvent(event)

    def mouseDoubleClickEvent(self, event):
        if event.button() == Qt.LeftButton:
            self.openRequested.emit(self.image_path)
        super().mouseDoubleClickEvent(event)


class CopyableTextBox(QFrame):
    def __init__(self, text: str, parent=None):
        super().__init__(parent)
        self.text = text
        self.setFrameShape(QFrame.StyledPanel)

        layout = QVBoxLayout(self)
        top = QHBoxLayout()

        title = QLabel("Prompt")
        top.addWidget(title)
        top.addStretch()

        copy_btn = QPushButton("Copy")
        copy_btn.setFixedWidth(60)
        copy_btn.clicked.connect(self.copy_text)
        top.addWidget(copy_btn)

        layout.addLayout(top)

        text_label = QLabel(text)
        text_label.setWordWrap(True)
        text_label.setTextInteractionFlags(Qt.TextSelectableByMouse)
        layout.addWidget(text_label)

    def copy_text(self):
        QApplication.clipboard().setText(self.text)


class ResultImageWidget(QFrame):
    def __init__(self, image_path: str, parent=None):
        super().__init__(parent)
        self.image_path = image_path
        self.setFrameShape(QFrame.StyledPanel)

        # Set a fixed size to prevent the widget from taking up excess horizontal space
        self.setFixedSize(QSize(130, 160))

        layout = QVBoxLayout(self)
        layout.setAlignment(Qt.AlignCenter)
        layout.setContentsMargins(5, 5, 5, 5)

        self.label = ClickableImageLabel(image_path, QSize(110, 110))
        self.label.openRequested.connect(self.show_preview)
        layout.addWidget(self.label, alignment=Qt.AlignCenter)

        copy_btn = QPushButton("Copy")
        copy_btn.setFixedWidth(60)
        copy_btn.clicked.connect(self.copy_path)
        layout.addWidget(copy_btn, alignment=Qt.AlignCenter)

    def copy_path(self):
        QApplication.clipboard().setText(self.image_path)

    def show_preview(self, _):
        dlg = PreviewDialog(self.image_path, self)
        dlg.exec()


class ResultRunWidget(QFrame):
    def __init__(self, run_data: HistoryEntry, parent=None):
        super().__init__(parent)
        self.run_data = run_data
        self.setFrameShape(QFrame.Box)
        self.setLineWidth(1)

        layout = QVBoxLayout(self)

        meta = QLabel(
            f"Time: {run_data.timestamp} | "
            f"Mode: {run_data.prompt_mode} | "
            f"Aspect: {run_data.aspect_ratio} | "
            f"Size: {run_data.image_size}"
        )
        meta.setWordWrap(True)
        layout.addWidget(meta)

        layout.addWidget(CopyableTextBox(run_data.prompt))

        # Gather all requests from all results to see how many input images we have in total
        all_requests = []
        for result in run_data.results:
            all_requests.extend(result.requests)

        input_groups = {}
        for req in all_requests:
            key = tuple(req.input_images)
            if key not in input_groups:
                input_groups[key] = []
            input_groups[key].append(req)

        if run_data.prompt_mode == "batch mode" and len(input_groups) > 1:
            out_label = QLabel("Generated images (Batch):")
            layout.addWidget(out_label)

            # Table-like form: first row = original image, second row = results
            for in_imgs, reqs in input_groups.items():
                if in_imgs:
                    # Row 1: Original image
                    inp_flow = FlowLayout()
                    for img in in_imgs:
                        inp_lbl = ClickableImageLabel(img, QSize(160, 160))
                        inp_flow.addWidget(inp_lbl)
                    layout.addLayout(inp_flow)

                # Row 2: Results
                out_flow = FlowLayout()
                for req in reqs:
                    if req.output_image:
                        w = ResultImageWidget(req.output_image)
                        out_flow.addWidget(w)
                layout.addLayout(out_flow)

        else:
            # All Images Mode OR Batch with 1 Image
            for in_imgs, reqs in input_groups.items():
                if in_imgs:
                    inp_label = QLabel("Input images:")
                    layout.addWidget(inp_label)
                    for p in in_imgs:
                        lbl = QLabel(p)
                        lbl.setWordWrap(True)
                        lbl.setTextInteractionFlags(Qt.TextSelectableByMouse)
                        layout.addWidget(lbl)

                out_images = [req.output_image for req in reqs if req.output_image]
                if out_images:
                    out_label = QLabel("Generated images:")
                    layout.addWidget(out_label)

                    flow = FlowLayout()
                    for path in out_images:
                        w = ResultImageWidget(path)
                        flow.addWidget(w)
                    
                    layout.addLayout(flow)

class WorkerSignals(QObject):
    finished = Signal(dict)
    error = Signal(str)


class GenerationWorker(QRunnable):
    def __init__(
        self,
        api_key: str,
        prompt: str,
        input_images: List[str],
        output_directory: str,
        aspect_ratio: str,
        image_size: str,
        prompt_mode: str,
        run_index: int,
        batch_id: str = "",
        total_in_batch: int = 1,
    ):
        super().__init__()
        self.signals = WorkerSignals()
        self.api_key = api_key
        self.prompt = prompt
        self.input_images = input_images
        self.output_directory = output_directory
        self.aspect_ratio = aspect_ratio
        self.image_size = image_size
        self.prompt_mode = prompt_mode
        self.run_index = run_index
        self.batch_id = batch_id
        self.total_in_batch = total_in_batch

    def run(self):
        try:
            logging.info("Starting generation worker for run index %s, batch ID %s", self.run_index, self.batch_id)
            client = genai.Client(api_key=self.api_key)
            model = "gemini-3.1-flash-image-preview"

            parts = [types.Part.from_text(text=self.prompt)]

            for image_path in self.input_images:
                mime_type, _ = mimetypes.guess_type(image_path)
                if not mime_type:
                    mime_type = "image/png"
                with open(image_path, "rb") as f:
                    data = f.read()
                parts.append(
                    types.Part.from_bytes(
                        data=data,
                        mime_type=mime_type,
                    )
                )

            contents = [
                types.Content(
                    role="user",
                    parts=parts,
                )
            ]

            config = types.GenerateContentConfig(
                thinking_config=types.ThinkingConfig(
                    thinking_level="MINIMAL",
                ),
                image_config=types.ImageConfig(
                    aspect_ratio=None if self.aspect_ratio == "auto" else self.aspect_ratio,
                    image_size=self.image_size,
                ),
                response_modalities=["IMAGE", "TEXT"],
            )

            out_dir = Path(self.output_directory)
            out_dir.mkdir(parents=True, exist_ok=True)

            output_paths = []
            response_text_parts = []
            file_index = 0

            logging.debug("Generating content stream...")
            for chunk in client.models.generate_content_stream(
                model=model,
                contents=contents,
                config=config,
            ):
                if getattr(chunk, "candidates", None):
                    for cand in chunk.candidates:
                        fr = getattr(cand, "finish_reason", None)
                        if fr and str(fr) not in ["STOP", "FINISH_REASON_UNSPECIFIED", "None"]:
                            response_text_parts.append(f"Finish Reason: {fr}")
                        
                        safety_ratings = getattr(cand, "safety_ratings", None)
                        if safety_ratings:
                            for sr in safety_ratings:
                                if getattr(sr, "blocked", False) or str(getattr(sr, "probability", "")) in ["HIGH", "MEDIUM"]:
                                    response_text_parts.append(f"Safety Rating - {getattr(sr, 'category', 'Unknown')}: {getattr(sr, 'probability', 'Unknown')}")

                if getattr(chunk, "prompt_feedback", None):
                    pf = chunk.prompt_feedback
                    block_reason = getattr(pf, "block_reason", None)
                    if block_reason:
                        response_text_parts.append(f"Prompt Blocked: {block_reason}")

                if not getattr(chunk, "parts", None):
                    continue

                for part in chunk.parts:
                    if (
                        getattr(part, "inline_data", None)
                        and part.inline_data
                        and part.inline_data.data
                    ):
                        inline_data = part.inline_data
                        ext = mimetypes.guess_extension(inline_data.mime_type) or ".png"
                        file_name = (
                            f"{datetime.now().strftime('%Y%m%d-%H%M%S')}_"
                            f"{self.run_index}_{file_index}_{uuid.uuid4().hex[:8]}{ext}"
                        )
                        file_path = out_dir / file_name
                        with open(file_path, "wb") as f:
                            f.write(inline_data.data)
                        output_paths.append(str(file_path))
                        file_index += 1
                    else:
                        txt = getattr(chunk, "text", None)
                        if txt:
                            response_text_parts.append(txt)

            logging.info("Generation completed successfully. Output files: %s", output_paths)
            result = {
                "timestamp": datetime.now().isoformat(timespec="seconds"),
                "prompt": self.prompt,
                "prompt_mode": self.prompt_mode,
                "aspect_ratio": self.aspect_ratio,
                "image_size": self.image_size,
                "input_images": list(self.input_images),
                "output_images": output_paths,
                "response_text": "\n".join(response_text_parts).strip(),
                "run_index": self.run_index,
                "batch_id": self.batch_id,
            }
            self.signals.finished.emit(result)

        except Exception as e:
            logging.error("Generation worker error: %s", e, exc_info=True)
            self.signals.error.emit(f"{e}\n\n{traceback.format_exc()}")


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Gemini Image Generator")
        self.resize(1600, 950)

        self.state = load_state()
        self.thread_pool = QThreadPool.globalInstance()
        self.image_paths: List[Path] = []
        self.selection_queue: List[str] = []
        self.thumbnail_widgets = {}

        self.build_ui()
        self.load_state_to_ui()
        self.refresh_input_images()
        self.refresh_history_ui()

    def build_ui(self):
        central = QWidget()
        self.setCentralWidget(central)

        root = QHBoxLayout(central)

        splitter = QSplitter(Qt.Horizontal)
        root.addWidget(splitter)

        # Left pane: results/history
        self.results_container = QWidget()
        self.results_layout = QVBoxLayout(self.results_container)
        self.results_layout.setAlignment(Qt.AlignTop)

        self.results_scroll = QScrollArea()
        self.results_scroll.setWidgetResizable(True)
        self.results_scroll.setMinimumWidth(100)
        self.results_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.results_scroll.setWidget(self.results_container)
        splitter.addWidget(self.results_scroll)

        # Center pane: controls
        center_widget = QWidget()
        center_layout = QVBoxLayout(center_widget)

        form = QFormLayout()

        self.api_key_edit = QLineEdit()
        self.api_key_edit.setEchoMode(QLineEdit.Password)
        self.api_key_edit.setPlaceholderText(
            "Uses GEMINI_API_KEY env var if left blank"
        )
        form.addRow("API Key:", self.api_key_edit)

        self.prompt_edit = QTextEdit()
        self.prompt_edit.setPlaceholderText("Enter prompt here...")
        self.prompt_edit.setMinimumHeight(180)
        form.addRow("Prompt:", self.prompt_edit)

        self.prompt_mode_combo = QComboBox()
        self.prompt_mode_combo.addItems(["all images", "batch mode"])
        form.addRow("Prompt mode:", self.prompt_mode_combo)

        self.repetitions_spin = QSpinBox()
        self.repetitions_spin.setRange(1, 999)
        self.repetitions_spin.setValue(1)
        form.addRow("Repetitions:", self.repetitions_spin)

        self.aspect_ratio_combo = QComboBox()
        self.aspect_ratio_combo.addItems(
            [
                "auto",
                "1:1",
                "3:4",
                "4:3",
                "9:16",
                "16:9",
                "3:2",
                "2:3",
                "4:5",
                "5:4",
                "21:9",
                "4:1",
                "1:4",
                "8:1",
                "1:8",
            ]
        )
        form.addRow("Aspect ratio:", self.aspect_ratio_combo)

        self.image_size_combo = QComboBox()
        self.image_size_combo.addItems(["512", "1K", "2K", "4K"])
        form.addRow("Image size:", self.image_size_combo)

        out_row = QHBoxLayout()
        self.output_dir_edit = QLineEdit()
        self.output_dir_btn = QPushButton("Browse")
        self.output_dir_btn.clicked.connect(self.choose_output_directory)
        out_row.addWidget(self.output_dir_edit)
        out_row.addWidget(self.output_dir_btn)
        form.addRow("Output directory:", out_row)

        center_layout.addLayout(form)

        dir_label = QLabel("Input directories")
        center_layout.addWidget(dir_label)

        self.dir_list = QListWidget()
        center_layout.addWidget(self.dir_list)

        dir_btn_row = QHBoxLayout()
        self.add_dir_btn = QPushButton("Add directory")
        self.remove_dir_btn = QPushButton("Remove selected")
        self.rescan_btn = QPushButton("Rescan")
        dir_btn_row.addWidget(self.add_dir_btn)
        dir_btn_row.addWidget(self.remove_dir_btn)
        dir_btn_row.addWidget(self.rescan_btn)
        center_layout.addLayout(dir_btn_row)

        self.add_dir_btn.clicked.connect(self.add_input_directory)
        self.remove_dir_btn.clicked.connect(self.remove_selected_directories)
        self.rescan_btn.clicked.connect(self.refresh_input_images)

        self.selection_info = QLabel("Selected images: 0")
        center_layout.addWidget(self.selection_info)

        self.generate_btn = QPushButton("Generate")
        self.generate_btn.clicked.connect(self.start_generation)
        
        gen_layout = QHBoxLayout()
        gen_layout.addWidget(self.generate_btn)
        
        from PySide6.QtCore import QTimer
        self.spinner_timer = QTimer(self)
        self.spinner_timer.timeout.connect(self.update_spinner)
        self.spinner_chars = ["|", "/", "-", "\\"]
        self.spinner_idx = 0
        
        self.spinner_label = QLabel("")
        self.spinner_label.hide()
        gen_layout.addWidget(self.spinner_label)
        gen_layout.addStretch()
        
        center_layout.addLayout(gen_layout)

        center_layout.addStretch()
        splitter.addWidget(center_widget)

        # Right pane: input images
        self.input_list = QListView()
        self.input_list.setViewMode(QListView.ViewMode.IconMode)
        self.input_list.setResizeMode(QListView.ResizeMode.Adjust)
        self.input_list.setSpacing(10)
        self.input_list.setUniformItemSizes(True)
        self.input_list.setSelectionMode(QListView.SelectionMode.NoSelection)
        self.input_delegate = ThumbnailDelegate(self.input_list)
        self.input_list.setItemDelegate(self.input_delegate)
        self.input_model = QStandardItemModel()
        self.input_list.setModel(self.input_model)

        self.input_list.clicked.connect(self.on_input_list_clicked)
        self.input_list.doubleClicked.connect(self.on_input_list_double_clicked)

        splitter.addWidget(self.input_list)

        splitter.setSizes([400, 420, 900])

        save_action = QAction("Save State", self)
        save_action.triggered.connect(self.save_ui_state)
        self.menuBar().addAction(save_action)

    def on_input_list_clicked(self, index: QModelIndex):
        path = index.data(Qt.ItemDataRole.UserRole)
        if path:
            self.toggle_selection(path)

    def on_input_list_double_clicked(self, index: QModelIndex):
        path = index.data(Qt.ItemDataRole.UserRole)
        if path:
            self.show_preview(path)

    def load_state_to_ui(self):
        self.prompt_edit.setPlainText(self.state.prompt)
        self.prompt_mode_combo.setCurrentText(self.state.prompt_mode)
        self.repetitions_spin.setValue(self.state.repetitions)
        self.aspect_ratio_combo.setCurrentText(self.state.aspect_ratio)
        self.image_size_combo.setCurrentText(self.state.image_size)
        self.output_dir_edit.setText(self.state.output_directory)

        self.dir_list.clear()
        for d in self.state.input_directories:
            self.dir_list.addItem(d)

    def save_ui_state(self):
        self.state.prompt = self.prompt_edit.toPlainText()
        self.state.prompt_mode = self.prompt_mode_combo.currentText()
        self.state.repetitions = self.repetitions_spin.value()
        self.state.aspect_ratio = self.aspect_ratio_combo.currentText()
        self.state.image_size = self.image_size_combo.currentText()
        self.state.output_directory = self.output_dir_edit.text().strip()
        self.state.input_directories = [
            self.dir_list.item(i).text() for i in range(self.dir_list.count())
        ]
        save_state(self.state)

    def closeEvent(self, event):
        self.save_ui_state()
        super().closeEvent(event)

    def add_input_directory(self):
        d = QFileDialog.getExistingDirectory(self, "Select input directory")
        if not d:
            return
        existing = {self.dir_list.item(i).text() for i in range(self.dir_list.count())}
        if d not in existing:
            self.dir_list.addItem(d)
            self.save_ui_state()
            self.refresh_input_images()

    def remove_selected_directories(self):
        for item in self.dir_list.selectedItems():
            self.dir_list.takeItem(self.dir_list.row(item))
        self.save_ui_state()
        self.refresh_input_images()

    def choose_output_directory(self):
        d = QFileDialog.getExistingDirectory(self, "Select output directory")
        if d:
            self.output_dir_edit.setText(d)
            self.save_ui_state()

    def clear_layout(self, layout):
        while layout.count():
            item = layout.takeAt(0)
            widget = item.widget()
            child_layout = item.layout()
            if widget is not None:
                widget.deleteLater()
            elif child_layout is not None:
                self.clear_layout(child_layout)

    def refresh_input_images(self):
        dirs = [self.dir_list.item(i).text() for i in range(self.dir_list.count())]
        self.image_paths = scan_images(dirs)
        self.selection_queue = []
        self.selection_info.setText("Selected images: 0")

        self.input_model.clear()

        for path in self.image_paths:
            item = QStandardItem()
            item.setData(str(path), Qt.ItemDataRole.UserRole)
            item.setData(None, Qt.ItemDataRole.UserRole + 1)
            item.setEditable(False)
            self.input_model.appendRow(item)

        self.save_ui_state()

    def toggle_selection(self, image_path: str):
        if image_path in self.selection_queue:
            self.selection_queue.remove(image_path)
        else:
            self.selection_queue.append(image_path)
        self.refresh_selection_badges()

    def refresh_selection_badges(self):
        order_map = {path: idx + 1 for idx, path in enumerate(self.selection_queue)}

        for row in range(self.input_model.rowCount()):
            item = self.input_model.item(row)
            path = item.data(Qt.ItemDataRole.UserRole)
            item.setData(order_map.get(path), Qt.ItemDataRole.UserRole + 1)

        self.selection_info.setText(f"Selected images: {len(self.selection_queue)}")

    def show_preview(self, image_path: str):
        dlg = PreviewDialog(image_path, self)
        dlg.exec()

    def refresh_history_ui(self):
        self.clear_layout(self.results_layout)
        if not self.state.history:
            self.results_layout.addWidget(QLabel("No history yet."))
            return

        for run_data in reversed(self.state.history):
            widget = ResultRunWidget(run_data)
            self.results_layout.addWidget(widget)

        self.results_layout.addStretch()

    def update_spinner(self):
        self.spinner_idx = (self.spinner_idx + 1) % len(self.spinner_chars)
        self.spinner_label.setText(f" Generating... [{self.completed_jobs}/{self.pending_jobs}] {self.spinner_chars[self.spinner_idx]}")

    def start_generation(self):
        prompt = self.prompt_edit.toPlainText().strip()
        if not prompt:
            QMessageBox.warning(self, "Missing prompt", "Please enter a prompt.")
            return

        if not self.selection_queue:
            QMessageBox.warning(
                self,
                "No images selected",
                "Select one or more images with single click.",
            )
            return

        api_key = (
            self.api_key_edit.text().strip()
            or os.environ.get("GEMINI_API_KEY", "").strip()
        )
        if not api_key:
            QMessageBox.warning(
                self, "Missing API key", "Provide an API key or set GEMINI_API_KEY."
            )
            return

        output_dir = self.output_dir_edit.text().strip()
        if not output_dir:
            QMessageBox.warning(
                self, "Missing output directory", "Please select an output directory."
            )
            return

        self.save_ui_state()
        self.generate_btn.setEnabled(False)

        prompt_mode = self.prompt_mode_combo.currentText()
        repetitions = self.repetitions_spin.value()
        aspect_ratio = self.aspect_ratio_combo.currentText()
        image_size = self.image_size_combo.currentText()

        jobs = []
        selected = list(self.selection_queue)

        # We need a shared ID to group jobs together when they finish
        batch_id = str(uuid.uuid4())

        if prompt_mode == "all images":
            for i in range(repetitions):
                jobs.append(
                    {
                        "prompt": prompt,
                        "input_images": selected,
                        "run_index": i,
                        "batch_id": batch_id,
                        "total_in_batch": repetitions,
                    }
                )
        else:  # batch mode
            run_index = 0
            for image_path in selected:
                for _ in range(repetitions):
                    jobs.append(
                        {
                            "prompt": prompt,
                            "input_images": [image_path],
                            "run_index": run_index,
                            "batch_id": batch_id,
                            "total_in_batch": len(selected) * repetitions,
                        }
                    )
                    run_index += 1

        self.pending_jobs = len(jobs)
        self.completed_jobs = 0
        self.current_batch_results = []  # Store results until all finish

        self.spinner_label.setText(f" Generating... [{self.completed_jobs}/{self.pending_jobs}] |")
        self.spinner_label.show()
        self.spinner_timer.start(100)

        for job in jobs:
            worker = GenerationWorker(
                api_key=api_key,
                prompt=job["prompt"],
                input_images=job["input_images"],
                output_directory=output_dir,
                aspect_ratio=aspect_ratio,
                image_size=image_size,
                prompt_mode=prompt_mode,
                run_index=job["run_index"],
                batch_id=job["batch_id"],
                total_in_batch=job["total_in_batch"],
            )
            worker.signals.finished.connect(self.on_generation_finished)
            worker.signals.error.connect(self.on_generation_error)
            self.thread_pool.start(worker)

    def on_generation_finished(self, run_data: dict):
        self.current_batch_results.append(run_data)
        self.completed_jobs += 1

        if self.completed_jobs >= self.pending_jobs:
            # All jobs in the current batch have finished. Aggregate them.
            if not self.current_batch_results:
                self.generate_btn.setEnabled(True)
                self.spinner_timer.stop()
                self.spinner_label.hide()
                return

            first_run = self.current_batch_results[0]

            # Sort results by run_index to maintain order
            self.current_batch_results.sort(key=lambda x: x.get("run_index", 0))

            results = []
            for res in self.current_batch_results:
                reqs = []
                out_images = res.get("output_images", [])
                for out_img in out_images:
                    reqs.append(RequestResult(
                        input_images=res.get("input_images", []),
                        output_image=out_img,
                        response_text=res.get("response_text", "")
                    ))
                # Handle cases where generation failed but we want to record the attempt
                if not out_images:
                    reqs.append(RequestResult(
                        input_images=res.get("input_images", []),
                        output_image=None,
                        response_text=res.get("response_text", "")
                    ))

                results.append(
                    GenerationResult(
                        requests=reqs,
                        run_index=res.get("run_index"),
                        batch_id=res.get("batch_id")
                    )
                )

            # Create a single consolidated history entry
            consolidated_run = HistoryEntry(
                timestamp=first_run.get("timestamp"),
                prompt=first_run.get("prompt"),
                prompt_mode=first_run.get("prompt_mode"),
                aspect_ratio=first_run.get("aspect_ratio"),
                image_size=first_run.get("image_size"),
                results=results
            )

            self.state.history.append(consolidated_run)
            save_state(self.state)
            self.refresh_history_ui()

            self.current_batch_results = []  # Clear for next generation
            self.generate_btn.setEnabled(True)
            self.spinner_timer.stop()
            self.spinner_label.hide()

    def on_generation_error(self, error_text: str):
        self.completed_jobs += 1
        if self.completed_jobs >= self.pending_jobs:
            self.generate_btn.setEnabled(True)
            self.spinner_timer.stop()
            self.spinner_label.hide()


def main():
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        handlers=[logging.StreamHandler(sys.stdout)],
    )
    logging.info("Starting %s", APP_NAME)
    app = QApplication(sys.argv)
    win = MainWindow()
    win.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()

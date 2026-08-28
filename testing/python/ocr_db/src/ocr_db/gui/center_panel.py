import logging
from collections import deque
from pathlib import Path
from typing import Dict, List, Optional, Set

import numpy as np
import pymupdf
from PyQt6.QtCore import QObject, Qt, QRectF, QThread, QTimer, pyqtSignal, pyqtSlot
from PyQt6.QtGui import QBrush, QColor, QImage, QPen, QPixmap
from PyQt6.QtWidgets import (
    QApplication,
    QCheckBox,
    QGraphicsItem,
    QGraphicsPixmapItem,
    QGraphicsRectItem,
    QGraphicsScene,
    QGraphicsView,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPushButton,
    QVBoxLayout,
    QWidget,
)
from sqlalchemy import select
from sqlalchemy.orm import sessionmaker

from ocr_db.gui.range_slider import RangeSlider
from ocr_db.ocr_db import DocumentRecord, ElementRecord, InputFileRecord, PageRecord

# Render scale applied to PDF pages (2.0 == 144 DPI). Element bboxes are
# stored as coordinates relative to the page dimensions in the [0, 1] range.
PDF_RENDER_SCALE: float = 2.0

TAG_COLORS = {
    "footnote": QColor(200, 150, 50, 150),
    "formula": QColor(100, 200, 50, 150),
    "list": QColor(50, 200, 150, 150),
    "list_item": QColor(50, 150, 200, 150),
    "page_footer": QColor(150, 50, 200, 150),
    "picture": QColor(200, 50, 150, 150),
    "root": QColor(255, 255, 255, 100),
    "section_header": QColor(255, 100, 100, 150),
    "table": QColor(100, 100, 255, 150),
    "text": QColor(100, 255, 100, 150),
    "unspecified": QColor(150, 150, 150, 150),
}

USER_REMOVED_COLOR = QColor(128, 128, 128, 150)


def _make_overlay_image(image: QImage) -> QImage:
    """Convert a rendered page into a black-ink alpha mask."""
    rgba = image.convertToFormat(QImage.Format.Format_RGBA8888)
    width = rgba.width()
    height = rgba.height()
    ptr = rgba.bits()
    ptr.setsize(rgba.sizeInBytes())
    array = np.frombuffer(ptr, dtype=np.uint8).reshape((height, width, 4))

    rgb = array[:, :, :3].astype(np.float32)
    gray = (0.299 * rgb[:, :, 0] + 0.587 * rgb[:, :, 1] + 0.114 * rgb[:, :, 2])

    contrast = 2.5
    contrasted = np.clip(
        (gray - 128.0) * contrast + 128.0,
        0,
        255,
    ).astype(np.uint8)
    alpha = 255 - contrasted

    mask = np.zeros((height, width, 4), dtype=np.uint8)
    mask[:, :, 3] = alpha
    return QImage(
        mask.data,
        width,
        height,
        width * 4,
        QImage.Format.Format_RGBA8888,
    ).copy()


class PdfPageRenderer(QObject):
    """Render PDF pages in a dedicated thread."""

    documentOpened = pyqtSignal(int)
    pageReady = pyqtSignal(int, QImage, QImage)

    def __init__(self, render_scale: float) -> None:
        super().__init__()
        self._render_scale = render_scale
        self._doc: Optional[pymupdf.Document] = None
        self._queue: deque[int] = deque()
        self._queued: Set[int] = set()

    @pyqtSlot(str)
    def openDocument(self, path: str) -> None:
        self.closeDocument()
        self._doc = pymupdf.open(path)
        self.documentOpened.emit(self._doc.page_count)

    @pyqtSlot()
    def closeDocument(self) -> None:
        self._queue.clear()
        self._queued.clear()
        if self._doc is not None:
            self._doc.close()
            self._doc = None

    @pyqtSlot(int)
    def requestPage(self, page_number: int) -> None:
        if self._doc is None or page_number in self._queued:
            return

        self._queue.append(page_number)
        self._queued.add(page_number)
        QTimer.singleShot(0, self._process_next)

    def _process_next(self) -> None:
        if self._doc is None or not self._queue:
            return

        page_number = self._queue.popleft()
        self._queued.discard(page_number)

        page = self._doc.load_page(page_number - 1)
        matrix = pymupdf.Matrix(self._render_scale, self._render_scale)
        pix = page.get_pixmap(matrix=matrix, alpha=False)

        base = QImage(
            pix.samples,
            pix.width,
            pix.height,
            pix.stride,
            QImage.Format.Format_RGB888,
        ).copy()
        overlay = _make_overlay_image(base)

        self.pageReady.emit(page_number, base, overlay)

        if self._queue:
            QTimer.singleShot(0, self._process_next)


class CustomGraphicsView(QGraphicsView):

    def mousePressEvent(self, event) -> None:
        if event.button() == Qt.MouseButton.RightButton:
            event.accept()
            return

        super().mousePressEvent(event)


class CenterPanel(QWidget):

    _openDocumentRequested = pyqtSignal(str)
    _pageRenderRequested = pyqtSignal(int)

    def __init__(
        self,
        session_factory: sessionmaker,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.session_factory = session_factory
        layout = QVBoxLayout(self)

        self.scene = QGraphicsScene(self)
        self.view = CustomGraphicsView(self.scene, self)
        self.view.setDragMode(QGraphicsView.DragMode.RubberBandDrag)
        self.view.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.view.customContextMenuRequested.connect(self.on_context_menu)
        layout.addWidget(self.view)

        top_layout = QHBoxLayout()
        self.cb_overlay = QCheckBox("Overlay Mode", self)
        self.cb_overlay.stateChanged.connect(self.on_mode_changed)
        top_layout.addWidget(self.cb_overlay)
        top_layout.addStretch()
        layout.addLayout(top_layout)

        self.nav_widget = QWidget(self)
        nav_layout = QHBoxLayout(self.nav_widget)
        nav_layout.setContentsMargins(0, 0, 0, 0)

        self.btn_prev = QPushButton("< Prev", self)

        self.page_input = QLineEdit(self)
        self.page_input.setFixedWidth(50)
        self.page_input.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.page_input.setText("1")

        self.btn_next = QPushButton("Next >", self)

        nav_layout.addStretch()
        nav_layout.addWidget(self.btn_prev)
        nav_layout.addWidget(self.page_input)
        nav_layout.addWidget(self.btn_next)
        nav_layout.addStretch()

        layout.addWidget(self.nav_widget)

        self.overlay_widget = QWidget(self)
        overlay_layout = QHBoxLayout(self.overlay_widget)
        overlay_layout.setContentsMargins(0, 0, 0, 0)

        self.slider = RangeSlider(self)
        self.slider.rangeChanged.connect(self.on_overlay_changed)

        self.lbl_overlay_range = QLabel("Range: 1 - 1", self)

        overlay_layout.addWidget(QLabel("Pages to overlay:", self))
        overlay_layout.addWidget(self.slider)
        overlay_layout.addWidget(self.lbl_overlay_range)
        layout.addWidget(self.overlay_widget)
        self.overlay_widget.hide()

        self.current_page_idx: int = 1
        self.overlay_mode: bool = False
        self.overlay_items: Dict[int, dict] = {}
        self.document_id: Optional[int] = None
        self.total_pages: int = 0
        self.current_elements: List[ElementRecord] = []
        self.current_pdf_path: Optional[str] = None

        self._base_images: Dict[int, QImage] = {}
        self._overlay_images: Dict[int, QImage] = {}

        self.btn_prev.clicked.connect(self.load_prev)
        self.btn_next.clicked.connect(self.load_next)
        self.page_input.returnPressed.connect(self.on_page_input_changed)

        self._render_thread = QThread(self)
        self._renderer = PdfPageRenderer(PDF_RENDER_SCALE)
        self._renderer.moveToThread(self._render_thread)

        self._render_thread.finished.connect(self._renderer.closeDocument)
        self._render_thread.finished.connect(self._renderer.deleteLater)
        self._openDocumentRequested.connect(self._renderer.openDocument)
        self._pageRenderRequested.connect(self._renderer.requestPage)
        self._renderer.documentOpened.connect(self._on_document_opened)
        self._renderer.pageReady.connect(self._on_page_ready)
        self._render_thread.start()

        app = QApplication.instance()
        if app is not None:
            app.aboutToQuit.connect(self._stop_render_thread)

    def _stop_render_thread(self) -> None:
        if self._render_thread.isRunning():
            self._render_thread.quit()
            self._render_thread.wait()

    def _load_document(self, file_path: str) -> None:
        with self.session_factory() as session:
            document_id = session.scalar(
                select(DocumentRecord.id).join(
                    InputFileRecord,
                    InputFileRecord.file_sha256 == DocumentRecord.file_sha256,
                ).where(InputFileRecord.absolute_path == file_path))

            if document_id is None:
                logging.warning(f"CenterPanel: No DB record for {file_path}")

            self.document_id = document_id

    def _load_page_elements(
        self,
        page_number: int,
    ) -> List[ElementRecord]:
        if self.document_id is None:
            return []

        with self.session_factory() as session:
            elements = list(
                session.scalars(
                    select(ElementRecord).join(
                        PageRecord,
                        ElementRecord.page_id == PageRecord.id,
                    ).where(
                        PageRecord.document_id == self.document_id,
                        PageRecord.page_number == page_number,
                    ).order_by(ElementRecord.element_index)))

            for element in elements:
                session.expunge(element)

            return elements

    def _load_all_elements(self) -> Dict[int, List[ElementRecord]]:
        result: Dict[int, List[ElementRecord]] = {}

        if self.document_id is None:
            return result

        with self.session_factory() as session:
            pairs = session.execute(
                select(PageRecord.page_number, ElementRecord).join(
                    ElementRecord,
                    ElementRecord.page_id == PageRecord.id,
                ).where(PageRecord.document_id == self.document_id).order_by(
                    PageRecord.page_number,
                    ElementRecord.element_index,
                )).all()

            for page_number, element in pairs:
                session.expunge(element)
                result.setdefault(page_number, []).append(element)

        return result

    def _save_elements(self, elements: List[ElementRecord]) -> None:
        if not elements:
            return

        ids = [element.id for element in elements]

        with self.session_factory() as session:
            db_elements = {
                element.id: element
                for element in session.scalars(
                    select(ElementRecord).where(ElementRecord.id.in_(ids)))
            }

            for element in elements:
                db_element = db_elements.get(element.id)
                if db_element is None:
                    continue

                db_element.enabled = element.enabled
                db_element.text = element.text
                db_element.label = element.label

            session.commit()

        logging.info(f"CenterPanel: Saved {len(elements)} elements to DB.")

    def _on_document_opened(self, page_count: int) -> None:
        self.total_pages = page_count

        if page_count > 0:
            self.slider.setRange(1, page_count)
            self.slider.setLowerValue(min(5, page_count))
            self.slider.setUpperValue(min(20, page_count))

        self.load_page_data()

    def _on_page_ready(
        self,
        page_number: int,
        base: QImage,
        overlay: QImage,
    ) -> None:
        self._base_images[page_number] = base
        self._overlay_images[page_number] = overlay

        if self.overlay_mode:
            entry = self.overlay_items.get(page_number)
            if entry is None:
                return

            if entry["pixmap_item"] is None:
                pixmap_item = QGraphicsPixmapItem(QPixmap.fromImage(overlay))
                pixmap_item.setZValue(-1)
                self.scene.addItem(pixmap_item)
                entry["pixmap_item"] = pixmap_item

            if not entry["elements_drawn"]:
                entry["tag_items"] = self.draw_elements(
                    entry["elements"],
                    overlay.width(),
                    overlay.height(),
                )
                entry["elements_drawn"] = True

            self.on_overlay_changed()
        elif page_number == self.current_page_idx:
            self._show_single_page()

    def on_mode_changed(self, state: int) -> None:
        self.overlay_mode = (state == Qt.CheckState.Checked.value
                             or state == 2)
        self.nav_widget.setVisible(not self.overlay_mode)
        self.overlay_widget.setVisible(self.overlay_mode)
        self.overlay_items = {}
        self.load_page_data()

    def on_overlay_changed(
        self,
        lower: int = -1,
        upper: int = -1,
    ) -> None:
        if lower == -1 or upper == -1:
            start = self.slider.lowerValue()
            end = self.slider.upperValue()
        else:
            start = lower
            end = upper

        self.lbl_overlay_range.setText(f"Range: {start} - {end}")

        if not self.overlay_mode:
            return

        first_visible = None

        for page_number in sorted(self.overlay_items):
            entry = self.overlay_items[page_number]
            is_visible = start <= page_number <= end

            if entry["pixmap_item"] is not None:
                entry["pixmap_item"].setVisible(is_visible)

                if is_visible and first_visible is None:
                    first_visible = entry["pixmap_item"]

            for tag_item in entry["tag_items"]:
                tag_item.setVisible(is_visible)

        if first_visible is not None:
            rect = first_visible.boundingRect()
            self.scene.setSceneRect(rect)
            self.view.fitInView(
                self.scene.sceneRect(),
                Qt.AspectRatioMode.KeepAspectRatio,
            )

    def on_page_input_changed(self) -> None:
        try:
            page = int(self.page_input.text())

            if 0 < page <= max(self.total_pages, 1):
                self.current_page_idx = page
                self.load_page_data()
        except ValueError:
            self.page_input.setText(str(self.current_page_idx))

    def on_context_menu(self, pos) -> None:
        from PyQt6.QtWidgets import QInputDialog, QMenu

        item = self.view.itemAt(pos)
        selected_items = self.scene.selectedItems()

        if not selected_items and item:
            item.setSelected(True)
            selected_items = [item]

        if not selected_items:
            return

        menu = QMenu(self)
        mark_removed_action = menu.addAction("Mark as Removed")
        unmark_removed_action = menu.addAction("Unmark as Removed")
        edit_text_action = menu.addAction("Edit Text")
        change_tag_action = menu.addAction("Change Tag")

        action = menu.exec(self.view.viewport().mapToGlobal(pos))

        if action == mark_removed_action:
            self.set_removed_status(selected_items, True)
        elif action == unmark_removed_action:
            self.set_removed_status(selected_items, False)
        elif action == edit_text_action:
            first_item = next(
                (selected_item
                 for selected_item in selected_items if isinstance(
                     selected_item,
                     QGraphicsRectItem,
                 )),
                None,
            )

            if first_item:
                element = first_item.data(Qt.ItemDataRole.UserRole)

                if element:
                    new_text, ok = QInputDialog.getText(
                        self,
                        "Edit Text",
                        "Text:",
                        QLineEdit.EchoMode.Normal,
                        element.text or "",
                    )

                    if ok:
                        self.edit_text_selected_items(
                            selected_items,
                            new_text,
                        )
        elif action == change_tag_action:
            tags = [
                "h1",
                "h2",
                "h3",
                "p",
                "table",
                "list",
                "picture",
                "footnote",
                "formula",
            ]

            first_item = next(
                (selected_item
                 for selected_item in selected_items if isinstance(
                     selected_item,
                     QGraphicsRectItem,
                 )),
                None,
            )

            current_tag = ""

            if first_item:
                element = first_item.data(Qt.ItemDataRole.UserRole)

                if element:
                    current_tag = element.label

            current_index = (tags.index(current_tag)
                             if current_tag in tags else 0)

            new_tag, ok = QInputDialog.getItem(
                self,
                "Change Tag",
                "Tag:",
                tags,
                current_index,
                True,
            )

            if ok and new_tag:
                self.change_tag_selected_items(
                    selected_items,
                    new_tag,
                )

    def _selected_elements(
        self,
        items: List[QGraphicsItem],
    ) -> List[ElementRecord]:
        elements = []

        for item in items:
            if not isinstance(item, QGraphicsRectItem):
                continue

            element = item.data(Qt.ItemDataRole.UserRole)
            if element:
                elements.append(element)

        return elements

    def edit_text_selected_items(
        self,
        items: List[QGraphicsItem],
        new_text: str,
    ) -> None:
        elements = self._selected_elements(items)

        for element in elements:
            element.text = new_text

        self._save_elements(elements)

    def change_tag_selected_items(
        self,
        items: List[QGraphicsItem],
        new_tag: str,
    ) -> None:
        elements = self._selected_elements(items)

        for element in elements:
            element.label = new_tag

        self._save_elements(elements)
        self.load_page_data()

    def set_removed_status(
        self,
        items: List[QGraphicsItem],
        status: bool,
    ) -> None:
        elements = []

        for item in items:
            if not isinstance(item, QGraphicsRectItem):
                continue

            element = item.data(Qt.ItemDataRole.UserRole)
            if element:
                element.enabled = not status
                elements.append(element)
                self._apply_element_style(item, element)

        self._save_elements(elements)

    def _apply_element_style(
        self,
        rect_item: QGraphicsRectItem,
        element: ElementRecord,
    ) -> None:
        if not element.enabled:
            brush = QBrush(USER_REMOVED_COLOR)
            brush.setStyle(Qt.BrushStyle.BDiagPattern)
            rect_item.setBrush(brush)
            pen = QPen(USER_REMOVED_COLOR)
        else:
            rect_item.setBrush(QBrush(Qt.BrushStyle.NoBrush))
            pen_color = TAG_COLORS.get(
                element.label,
                QColor(255, 0, 0, 150),
            )
            pen = QPen(pen_color)

        pen.setWidth(2)
        rect_item.setPen(pen)

    def load_prev(self) -> None:
        if self.current_page_idx > 1:
            self.current_page_idx -= 1
            self.load_page_data()

    def load_next(self) -> None:
        if self.current_page_idx < self.total_pages:
            self.current_page_idx += 1
            self.load_page_data()

    def load_pdf(self, pdf_path) -> None:
        self.current_pdf_path = str(Path(pdf_path).absolute())
        self.current_page_idx = 1
        self.overlay_items = {}
        self._base_images.clear()
        self._overlay_images.clear()
        self.total_pages = 0

        logging.info(f"CenterPanel: Loading PDF: {self.current_pdf_path}")
        self._load_document(self.current_pdf_path)

        self.scene.clear()
        self.scene.addText("Loading PDF...")
        self._openDocumentRequested.emit(self.current_pdf_path)

    def load_page_data(self) -> None:
        if self.document_id is None:
            self.scene.clear()
            self.scene.addText("No document loaded from DB.")
            return

        if self.overlay_mode:
            self._build_overlay_scene()
        else:
            self._show_single_page()

    def _show_single_page(self) -> None:
        self.scene.clear()
        self.overlay_items = {}
        self.page_input.setText(str(self.current_page_idx))

        base = self._base_images.get(self.current_page_idx)

        if base is None:
            self.scene.addText(f"Rendering page {self.current_page_idx}...")
            self._pageRenderRequested.emit(self.current_page_idx)
            return

        self.current_elements = self._load_page_elements(self.current_page_idx)

        pixmap_item = QGraphicsPixmapItem(QPixmap.fromImage(base))
        self.scene.addItem(pixmap_item)

        self.draw_elements(
            self.current_elements,
            base.width(),
            base.height(),
        )

        self.scene.setSceneRect(pixmap_item.boundingRect())
        self.view.fitInView(
            self.scene.sceneRect(),
            Qt.AspectRatioMode.KeepAspectRatio,
        )

    def _build_overlay_scene(self) -> None:
        self.scene.clear()
        self.overlay_items = {}

        if self.total_pages <= 0:
            return

        all_elements = self._load_all_elements()

        for page_number in range(1, self.total_pages + 1):
            elements = all_elements.get(page_number, [])
            entry = {
                "pixmap_item": None,
                "tag_items": [],
                "elements": elements,
                "elements_drawn": False,
            }

            overlay_image = self._overlay_images.get(page_number)

            if overlay_image is not None:
                pixmap_item = QGraphicsPixmapItem(
                    QPixmap.fromImage(overlay_image))
                pixmap_item.setZValue(-1)
                self.scene.addItem(pixmap_item)
                entry["pixmap_item"] = pixmap_item
                entry["tag_items"] = self.draw_elements(
                    elements,
                    overlay_image.width(),
                    overlay_image.height(),
                )
                entry["elements_drawn"] = True
            else:
                self._pageRenderRequested.emit(page_number)

            self.overlay_items[page_number] = entry

        self.on_overlay_changed()

    def resizeEvent(self, event) -> None:
        super().resizeEvent(event)

        if not self.scene.sceneRect().isEmpty():
            self.view.fitInView(
                self.scene.sceneRect(),
                Qt.AspectRatioMode.KeepAspectRatio,
            )

    def draw_elements(
        self,
        elements: List[ElementRecord],
        page_width: int,
        page_height: int,
    ) -> List[QGraphicsRectItem]:
        items: List[QGraphicsRectItem] = []

        for element in elements:
            x1 = float(element.bbox_x1) * page_width
            y1 = float(element.bbox_y1) * page_height
            x2 = float(element.bbox_x2) * page_width
            y2 = float(element.bbox_y2) * page_height

            rect_item = QGraphicsRectItem(QRectF(
                x1,
                y1,
                x2 - x1,
                y2 - y1,
            ))
            self._apply_element_style(rect_item, element)

            rect_item.setFlag(
                QGraphicsItem.GraphicsItemFlag.ItemIsSelectable,
                True,
            )
            rect_item.setData(
                Qt.ItemDataRole.UserRole,
                element,
            )

            self.scene.addItem(rect_item)
            items.append(rect_item)

        return items

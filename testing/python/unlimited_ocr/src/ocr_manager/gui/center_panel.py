import io
import logging
from typing import Optional, List, Dict

from PyQt6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QHBoxLayout,
    QPushButton,
    QGraphicsView,
    QGraphicsScene,
    QGraphicsPixmapItem,
    QGraphicsRectItem,
    QGraphicsItem,
    QCheckBox,
    QLabel,
    QLineEdit,
)
from PyQt6.QtGui import QPixmap, QPen, QColor, QBrush, QImage
from PyQt6.QtCore import Qt, QRectF
from sqlalchemy import select
from sqlalchemy.orm import sessionmaker

from ocr_manager.gui.range_slider import RangeSlider
from ocr_manager.ocr_db import DocumentRecord, ElementRecord, InputFileRecord, PageRecord

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


class CustomGraphicsView(QGraphicsView):

    def mousePressEvent(self, event):
        if event.button() == Qt.MouseButton.RightButton:
            event.accept()
            return
        super().mousePressEvent(event)


class CenterPanel(QWidget):

    def __init__(self,
                 session_factory: sessionmaker,
                 parent: Optional[QWidget] = None) -> None:
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

        # State
        self.current_page_idx: int = 1
        self.overlay_mode: bool = False
        self.overlay_items: List[dict] = []
        self.document_id: Optional[int] = None
        self.total_pages: int = 0
        # element_id -> QGraphicsRectItem for the current scene
        self.current_elements: List[ElementRecord] = []

        self.btn_prev.clicked.connect(self.load_prev)
        self.btn_next.clicked.connect(self.load_next)
        self.page_input.returnPressed.connect(self.on_page_input_changed)

    # ------------------------------------------------------------------ DB access

    def _load_document(self, file_path: str) -> None:
        with self.session_factory() as session:
            input_rec = session.scalar(
                select(InputFileRecord).where(
                    InputFileRecord.absolute_path == file_path))
            if not input_rec:
                self.document_id = None
                self.total_pages = 0
                logging.warning(f"CenterPanel: No DB record for {file_path}")
                return
            doc = session.scalar(
                select(DocumentRecord).where(
                    DocumentRecord.file_sha256 == input_rec.file_sha256))
            if not doc:
                self.document_id = None
                self.total_pages = 0
                return
            self.document_id = doc.id
            self.total_pages = session.query(PageRecord).filter(
                PageRecord.document_id == doc.id).count()

    def _load_page(
            self,
            page_number: int) -> tuple[Optional[QPixmap], List[ElementRecord]]:
        """Returns (page pixmap, elements). Page image taken from first element blob."""
        if self.document_id is None:
            return None, []
        with self.session_factory() as session:
            page = session.scalar(
                select(PageRecord).where(
                    PageRecord.document_id == self.document_id,
                    PageRecord.page_number == page_number,
                ))
            if not page:
                return None, []
            elements = list(
                session.scalars(
                    select(ElementRecord).where(
                        ElementRecord.page_id == page.id).order_by(
                            ElementRecord.element_index)))
            for e in elements:
                session.expunge(e)

            pixmap = None
            if elements:
                pixmap = QPixmap()
                if not pixmap.loadFromData(elements[0].image_blob):
                    pixmap = None
            return pixmap, elements

    def _save_elements(self, elements: List[ElementRecord]) -> None:
        if not elements:
            return
        ids = [e.id for e in elements]
        with self.session_factory() as session:
            db_elements = {
                e.id: e
                for e in session.scalars(
                    select(ElementRecord).where(ElementRecord.id.in_(ids)))
            }
            for e in elements:
                db_e = db_elements.get(e.id)
                if db_e is None:
                    continue
                db_e.enabled = e.enabled
                db_e.text = e.text
                db_e.label = e.label
            session.commit()
        logging.info(f"CenterPanel: Saved {len(elements)} elements to DB.")

    # ------------------------------------------------------------------ UI logic

    def on_mode_changed(self, state: int) -> None:
        self.overlay_mode = (state == Qt.CheckState.Checked.value
                             or state == 2)
        self.nav_widget.setVisible(not self.overlay_mode)
        self.overlay_widget.setVisible(self.overlay_mode)
        self.overlay_items = []
        self.load_page_data()

    def on_overlay_changed(self, lower: int = -1, upper: int = -1) -> None:
        if lower == -1 or upper == -1:
            start = self.slider.lowerValue()
            end = self.slider.upperValue()
        else:
            start, end = lower, upper

        self.lbl_overlay_range.setText(f"Range: {start} - {end}")

        if self.overlay_mode:
            first_visible = None
            for i, item_data in enumerate(self.overlay_items):
                page_num = i + 1
                is_visible = (start <= page_num <= end)
                if is_visible and first_visible is None:
                    first_visible = item_data['pixmap_item']

                if item_data['pixmap_item']:
                    item_data['pixmap_item'].setVisible(is_visible)
                for tag_item in item_data['tag_items']:
                    tag_item.setVisible(is_visible)

            if first_visible:
                rect = first_visible.boundingRect()
                self.scene.setSceneRect(rect)
                self.view.fitInView(self.scene.sceneRect(),
                                    Qt.AspectRatioMode.KeepAspectRatio)

    def on_page_input_changed(self) -> None:
        try:
            page = int(self.page_input.text())
            if 0 < page <= max(self.total_pages, 1):
                self.current_page_idx = page
                self.load_page_data()
        except ValueError:
            self.page_input.setText(str(self.current_page_idx))

    def on_context_menu(self, pos) -> None:
        from PyQt6.QtWidgets import QMenu, QInputDialog
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
                (i
                 for i in selected_items if isinstance(i, QGraphicsRectItem)),
                None)
            if first_item:
                element = first_item.data(Qt.ItemDataRole.UserRole)
                if element:
                    new_text, ok = QInputDialog.getText(
                        self, "Edit Text", "Text:", QLineEdit.EchoMode.Normal,
                        element.text or "")
                    if ok:
                        self.edit_text_selected_items(selected_items, new_text)
        elif action == change_tag_action:
            tags = [
                "h1", "h2", "h3", "p", "table", "list", "picture", "footnote",
                "formula"
            ]
            first_item = next(
                (i
                 for i in selected_items if isinstance(i, QGraphicsRectItem)),
                None)
            current_tag = ""
            if first_item:
                element = first_item.data(Qt.ItemDataRole.UserRole)
                if element:
                    current_tag = element.label

            current_idx = tags.index(current_tag) if current_tag in tags else 0
            new_tag, ok = QInputDialog.getItem(self, "Change Tag", "Tag:",
                                               tags, current_idx, True)
            if ok and new_tag:
                self.change_tag_selected_items(selected_items, new_tag)

    def _selected_elements(self,
                           items: List[QGraphicsItem]) -> List[ElementRecord]:
        elements = []
        for item in items:
            if isinstance(item, QGraphicsRectItem):
                element = item.data(Qt.ItemDataRole.UserRole)
                if element:
                    elements.append(element)
        return elements

    def edit_text_selected_items(self, items: List[QGraphicsItem],
                                 new_text: str) -> None:
        elements = self._selected_elements(items)
        for e in elements:
            e.text = new_text
        self._save_elements(elements)

    def change_tag_selected_items(self, items: List[QGraphicsItem],
                                  new_tag: str) -> None:
        elements = self._selected_elements(items)
        for e in elements:
            e.label = new_tag
        self._save_elements(elements)
        self.load_page_data()  # refresh colors

    def set_removed_status(self, items: List[QGraphicsItem],
                           status: bool) -> None:
        elements = []
        for item in items:
            if isinstance(item, QGraphicsRectItem):
                element = item.data(Qt.ItemDataRole.UserRole)
                if element:
                    element.enabled = not status
                    elements.append(element)
                    self._apply_element_style(item, element)
        self._save_elements(elements)

    def _apply_element_style(self, rect_item: QGraphicsRectItem,
                             element: ElementRecord) -> None:
        if not element.enabled:
            brush = QBrush(USER_REMOVED_COLOR)
            brush.setStyle(Qt.BrushStyle.BDiagPattern)
            rect_item.setBrush(brush)
            pen = QPen(USER_REMOVED_COLOR)
        else:
            rect_item.setBrush(QBrush(Qt.BrushStyle.NoBrush))
            pen_color = TAG_COLORS.get(element.label, QColor(255, 0, 0, 150))
            pen = QPen(pen_color)
        pen.setWidth(2)
        rect_item.setPen(pen)

    # ------------------------------------------------------------------ navigation

    def load_prev(self) -> None:
        if self.current_page_idx > 1:
            self.current_page_idx -= 1
            self.load_page_data()

    def load_next(self) -> None:
        if self.current_page_idx < self.total_pages:
            self.current_page_idx += 1
            self.load_page_data()

    def load_pdf(self, pdf_path, output_dir=None) -> None:
        self.current_pdf_path = str(Path(pdf_path).absolute())
        self.current_page_idx = 1
        self.overlay_items = []

        logging.info(
            f"CenterPanel: Loading PDF from DB: {self.current_pdf_path}")
        self._load_document(self.current_pdf_path)

        if self.total_pages > 0:
            self.slider.setRange(1, self.total_pages)
            self.slider.setLowerValue(5)
            self.slider.setUpperValue(min(20, self.total_pages))

        self.load_page_data()

    def get_all_pages_data(self) -> List[dict]:
        """Returns list of {'page_number': int, 'elements': [ElementRecord]} for HTML generation."""
        pages = []
        if self.document_id is None:
            return pages
        with self.session_factory() as session:
            page_recs = list(
                session.scalars(
                    select(PageRecord).where(
                        PageRecord.document_id == self.document_id).order_by(
                            PageRecord.page_number)))
            for page in page_recs:
                elements = list(
                    session.scalars(
                        select(ElementRecord).where(
                            ElementRecord.page_id == page.id).order_by(
                                ElementRecord.element_index)))
                for e in elements:
                    session.expunge(e)
                pages.append({
                    'page_number': page.page_number,
                    'elements': elements
                })
        return pages

    # ------------------------------------------------------------------ rendering

    def _create_transparent_pixmap(self, pixmap: QPixmap) -> QPixmap:
        import numpy as np
        from PyQt6.QtGui import QPainter

        image = pixmap.toImage().convertToFormat(QImage.Format.Format_RGBA8888)

        width = image.width()
        height = image.height()
        ptr = image.bits()
        array = np.frombuffer(ptr, dtype=np.uint8).reshape((height, width, 4))

        rgb = array[:, :, :3].astype(np.float32)
        gray = 0.299 * rgb[:, :, 0] + 0.587 * rgb[:, :, 1] + 0.114 * rgb[:, :,
                                                                         2]

        contrast = 2.5
        contrasted = (gray - 128.0) * contrast + 128.0
        contrasted = np.clip(contrasted, 0, 255).astype(np.uint8)

        alpha = (255 - contrasted).astype(np.uint8)

        mask_data = np.zeros((height, width, 4), dtype=np.uint8)
        mask_data[:, :, 3] = alpha

        mask = QImage(mask_data.data, width, height, width * 4,
                      QImage.Format.Format_RGBA8888).copy()

        result = QPixmap(width, height)
        result.fill(Qt.GlobalColor.transparent)

        painter = QPainter(result)
        painter.fillRect(result.rect(), QColor(0, 0, 0))
        painter.setCompositionMode(
            QPainter.CompositionMode.CompositionMode_DestinationIn)
        painter.drawImage(0, 0, mask)
        painter.end()

        return result

    def pre_load_overlays(self) -> None:
        self.scene.clear()
        self.overlay_items = []

        logging.info(
            f"CenterPanel: Preloading {self.total_pages} overlay pages...")

        for p_idx in range(1, self.total_pages + 1):
            pixmap, elements = self._load_page(p_idx)
            if pixmap is None or pixmap.isNull():
                self.overlay_items.append({
                    'pixmap_item': None,
                    'tag_items': []
                })
                continue

            transparent = self._create_transparent_pixmap(pixmap)
            pixmap_item = QGraphicsPixmapItem(transparent)
            pixmap_item.setZValue(-1)
            self.scene.addItem(pixmap_item)

            tag_items = self.draw_elements(elements)
            self.overlay_items.append({
                'pixmap_item': pixmap_item,
                'tag_items': tag_items
            })

        self.on_overlay_changed()

    def load_page_data(self) -> None:
        if self.document_id is None:
            self.scene.clear()
            self.scene.addText("No document loaded from DB.")
            return

        if self.overlay_mode:
            if not self.overlay_items:
                self.pre_load_overlays()
            else:
                self.on_overlay_changed()
        else:
            self.scene.clear()
            self.overlay_items = []
            self.page_input.setText(str(self.current_page_idx))

            pixmap, elements = self._load_page(self.current_page_idx)
            self.current_elements = elements

            if pixmap is None:
                self.scene.addText(
                    f"Page {self.current_page_idx} not found in DB.")
                return

            pixmap_item = QGraphicsPixmapItem(pixmap)
            self.scene.addItem(pixmap_item)
            self.draw_elements(elements)

            self.scene.setSceneRect(pixmap_item.boundingRect())
            self.view.fitInView(self.scene.sceneRect(),
                                Qt.AspectRatioMode.KeepAspectRatio)

    def resizeEvent(self, event) -> None:
        super().resizeEvent(event)
        if not self.scene.sceneRect().isEmpty():
            self.view.fitInView(self.scene.sceneRect(),
                                Qt.AspectRatioMode.KeepAspectRatio)

    def draw_elements(
            self, elements: List[ElementRecord]) -> List[QGraphicsRectItem]:
        items: List[QGraphicsRectItem] = []
        for element in elements:
            x = element.bbox_x1
            y = element.bbox_y1
            w = element.bbox_x2 - element.bbox_x1
            h = element.bbox_y2 - element.bbox_y1

            rect_item = QGraphicsRectItem(QRectF(x, y, w, h))
            self._apply_element_style(rect_item, element)

            rect_item.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable,
                              True)
            rect_item.setData(Qt.ItemDataRole.UserRole, element)

            self.scene.addItem(rect_item)
            items.append(rect_item)
        return items

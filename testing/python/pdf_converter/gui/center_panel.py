import json
import logging
from pathlib import Path
from typing import Optional, List

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QGraphicsRectItem, QGraphicsItem, QCheckBox, QSlider, QLabel, QLineEdit
from PySide6.QtGui import QPixmap, QPen, QColor, QBrush, QImage
from PySide6.QtCore import Qt, QRectF

from models import DocTag, PageData
from gui.range_slider import RangeSlider

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
    "unspecified": QColor(150, 150, 150, 150)
}

class CenterPanel(QWidget):
    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        layout = QVBoxLayout(self)
        
        # Graphics View for displaying the page and bounding boxes
        self.scene = QGraphicsScene(self)
        self.view = QGraphicsView(self.scene, self)
        self.view.setDragMode(QGraphicsView.DragMode.RubberBandDrag)
        self.view.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.view.customContextMenuRequested.connect(self.on_context_menu)
        layout.addWidget(self.view)
        
        # Top Controls (Mode toggle)
        top_layout = QHBoxLayout()
        self.cb_overlay = QCheckBox("Overlay Mode", self)
        self.cb_overlay.stateChanged.connect(self.on_mode_changed)
        top_layout.addWidget(self.cb_overlay)
        top_layout.addStretch()
        layout.addLayout(top_layout)

        # Navigation Controls (Single Mode)
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
        
        # Overlay Controls (Overlay Mode)
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
        
        # State variables
        self.current_page_idx: int = 1
        self.page_data: Optional[PageData] = None
        self.overlay_mode: bool = False
        self.overlay_items: List[dict] = []
        
        # Signal connections
        self.btn_prev.clicked.connect(self.load_prev)
        self.btn_next.clicked.connect(self.load_next)
        self.page_input.returnPressed.connect(self.on_page_input_changed)

    def on_mode_changed(self, state: int) -> None:
        self.overlay_mode = (state == Qt.CheckState.Checked.value or state == 2)
        self.nav_widget.setVisible(not self.overlay_mode)
        self.overlay_widget.setVisible(self.overlay_mode)
        self.load_page_data()

    def on_overlay_changed(self, lower: int = -1, upper: int = -1) -> None:
        if lower == -1 or upper == -1:
            start = self.slider.lowerValue()
            end = self.slider.upperValue()
        else:
            start = lower
            end = upper
            
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
                self.view.fitInView(self.scene.sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)

    def on_page_input_changed(self) -> None:
        try:
            page = int(self.page_input.text())
            if page > 0:
                self.current_page_idx = page
                self.load_page_data()
        except ValueError:
            self.page_input.setText(str(self.current_page_idx))

    def on_context_menu(self, pos) -> None:
        from PySide6.QtWidgets import QMenu
        selected_items = self.scene.selectedItems()
        if not selected_items:
            return
            
        menu = QMenu(self)
        toggle_action = menu.addAction("Toggle Removed Status")
        
        action = menu.exec(self.view.viewport().mapToGlobal(pos))
        if action == toggle_action:
            self.toggle_selected_items(selected_items)

    def toggle_selected_items(self, items: List[QGraphicsItem]) -> None:
        modified_pages = set()
        
        for item in items:
            if isinstance(item, QGraphicsRectItem):
                tag = item.data(Qt.ItemDataRole.UserRole)
                page_meta = item.data(Qt.ItemDataRole.UserRole + 1)
                
                if tag and page_meta:
                    tag.user_removed = not getattr(tag, 'user_removed', False)
                    if tag.user_removed:
                        item.setBrush(QBrush(QColor(100, 100, 100, 100)))
                    else:
                        item.setBrush(QBrush(Qt.BrushStyle.NoBrush))
                    
                    modified_pages.add(page_meta)

        for page_data, json_path in modified_pages:
            try:
                with open(json_path, 'w') as f:
                    f.write(page_data.model_dump_json(indent=2))
                logging.info(f"CenterPanel: Saved updated annotations to {json_path.name}")
            except Exception as e:
                logging.error(f"Failed to save JSON: {e}")

    def load_prev(self) -> None:
        if self.current_page_idx > 1:
            self.current_page_idx -= 1
            self.load_page_data()
        
    def load_next(self) -> None:
        if self.page_data:
            self.current_page_idx += 1
            self.load_page_data()

    def load_pdf(self, pdf_path: Path, output_dir: Path) -> None:
        self.current_pdf_path = Path(pdf_path)
        self.output_dir = Path(output_dir)
        self.pdf_output_dir = self.output_dir / self.current_pdf_path.stem
        self.current_page_idx = 1
        
        logging.info(f"CenterPanel: Loading PDF: {self.current_pdf_path}")
        
        total = 0
        if self.pdf_output_dir.exists():
            total = len(list(self.pdf_output_dir.glob("page_*.json")))
        if total > 0:
            self.slider.setRange(1, total)
            self.slider.setLowerValue(1)
            self.slider.setUpperValue(min(3, total))
            
        self.load_page_data()
        
    def _create_transparent_pixmap(self, image_path: str) -> QPixmap:
        import numpy as np
        from PySide6.QtCore import Qt
        from PySide6.QtGui import QColor, QImage, QPainter, QPixmap

        image = QImage(image_path).convertToFormat(QImage.Format.Format_RGBA8888)

        width = image.width()
        height = image.height()
        ptr = image.bits()
        array = np.frombuffer(ptr, dtype=np.uint8).reshape((height, width, 4))

        rgb = array[:, :, :3].astype(np.float32)
        gray = 0.299 * rgb[:, :, 0] + 0.587 * rgb[:, :, 1] + 0.114 * rgb[:, :, 2]

        contrast = 2.5
        contrasted = (gray - 128.0) * contrast + 128.0
        contrasted = np.clip(contrasted, 0, 255).astype(np.uint8)

        alpha = (255 - contrasted).astype(np.uint8)

        mask_data = np.zeros((height, width, 4), dtype=np.uint8)
        mask_data[:, :, 3] = alpha

        mask = QImage(
            mask_data.data,
            width,
            height,
            width * 4,
            QImage.Format.Format_RGBA8888,
        ).copy()

        result = QPixmap(width, height)
        result.fill(Qt.GlobalColor.transparent)

        painter = QPainter(result)
        painter.fillRect(result.rect(), QColor(0, 0, 0))
        painter.setCompositionMode(QPainter.CompositionMode.CompositionMode_DestinationIn)
        painter.drawImage(0, 0, mask)
        painter.end()

        return result

    def pre_load_overlays(self) -> None:
        self.scene.clear()
        self.overlay_items = []
        
        total = 0
        if self.pdf_output_dir.exists():
            total = len(list(self.pdf_output_dir.glob("page_*.json")))
            
        logging.info(f"CenterPanel: Preloading {total} overlay pages...")
            
        for p_idx in range(1, total + 1):
            json_path = self.pdf_output_dir / f"page_{p_idx:03d}.json"
            if not json_path.exists():
                self.overlay_items.append({'pixmap_item': None, 'tag_items': []})
                continue
                
            try:
                with open(json_path, 'r') as f:
                    data = json.load(f)
                    p_data = PageData(**data)
                    
                    pixmap = self._create_transparent_pixmap(p_data.image_cache_path)
                    tag_items = []
                    
                    if not pixmap.isNull():
                        pixmap_item = QGraphicsPixmapItem(pixmap)
                        pixmap_item.setZValue(-1)
                        self.scene.addItem(pixmap_item)
                        
                        tag_items = self.draw_spatial_tags(p_data.spatial_tags, float(pixmap.width()), float(pixmap.height()), p_data, json_path)
                        
                        self.overlay_items.append({'pixmap_item': pixmap_item, 'tag_items': tag_items})
                    else:
                        self.overlay_items.append({'pixmap_item': None, 'tag_items': []})
            except Exception as e:
                logging.error(f"Error loading overlay page {p_idx}: {e}")
                self.overlay_items.append({'pixmap_item': None, 'tag_items': []})
        
        self.on_overlay_changed() # Update visibility based on sliders

    def load_page_data(self) -> None:
        if not hasattr(self, 'pdf_output_dir'): return
        
        if self.overlay_mode:
            if not self.overlay_items:
                self.pre_load_overlays()
            else:
                self.on_overlay_changed()
        else:
            self.scene.clear()
            self.overlay_items = []
            self.page_input.setText(str(self.current_page_idx))
            json_path = self.pdf_output_dir / f"page_{self.current_page_idx:03d}.json"
            
            if json_path.exists():
                try:
                    with open(json_path, 'r') as f:
                        data = json.load(f)
                        self.page_data = PageData(**data)
                        logging.info(f"CenterPanel: Loaded page {self.current_page_idx}")
                        self.load_page(self.page_data.image_cache_path, self.page_data.spatial_tags, json_path)
                except Exception as e:
                    logging.error(f"Failed to load page data: {e}")
                    self.scene.addText(f"Failed to load page data:\n{e}")
            else:
                self.page_data = None
                self.scene.addText(f"Page {self.current_page_idx} not processed yet.")

    def load_page(self, image_path: str, spatial_tags: List[DocTag], json_path: Path) -> None:
        pixmap = QPixmap(image_path)
        if not pixmap.isNull():
            pixmap_item = QGraphicsPixmapItem(pixmap)
            self.scene.addItem(pixmap_item)
            self.draw_spatial_tags(spatial_tags, float(pixmap.width()), float(pixmap.height()), self.page_data, json_path)
            
            self.scene.setSceneRect(pixmap_item.boundingRect())
            self.view.fitInView(self.scene.sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)

    def draw_spatial_tags(self, spatial_tags: List[DocTag], p_width: float, p_height: float, page_data: Optional[PageData], json_path: Path) -> List[QGraphicsRectItem]:
        items: List[QGraphicsRectItem] = []
        def draw_tags(tags: List[DocTag]) -> None:
            for tag in tags:
                if tag.bbox:
                    x = tag.bbox.x * p_width
                    y = tag.bbox.y * p_height
                    w = tag.bbox.width * p_width
                    h = tag.bbox.height * p_height
                    
                    rect_item = QGraphicsRectItem(QRectF(x, y, w, h))
                    
                    pen_color = TAG_COLORS.get(tag.tag_name, QColor(255, 0, 0, 150))
                    pen = QPen(pen_color)
                    pen.setWidth(2)
                    rect_item.setPen(pen)
                    
                    if getattr(tag, 'user_removed', False):
                        rect_item.setBrush(QBrush(QColor(100, 100, 100, 100)))
                    
                    rect_item.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, True)
                    rect_item.setData(Qt.ItemDataRole.UserRole, tag)
                    rect_item.setData(Qt.ItemDataRole.UserRole + 1, (page_data, json_path))
                    
                    self.scene.addItem(rect_item)
                    items.append(rect_item)
                
                if tag.children:
                    draw_tags(tag.children)
        draw_tags(spatial_tags)
        return items

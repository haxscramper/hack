import json
import logging
from pathlib import Path
from typing import Optional, List

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QGraphicsRectItem, QGraphicsItem, QCheckBox, QSlider, QLabel, QLineEdit
from PySide6.QtGui import QPixmap, QPen, QColor, QBrush, QImage
from PySide6.QtCore import Qt, QRectF

from models import DocTag, PageData

class CenterPanel(QWidget):
    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        layout = QVBoxLayout(self)
        
        # Graphics View for displaying the page and bounding boxes
        self.scene = QGraphicsScene(self)
        self.view = QGraphicsView(self.scene, self)
        self.view.setDragMode(QGraphicsView.DragMode.RubberBandDrag)
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
        
        self.slider_start = QSlider(Qt.Orientation.Horizontal, self)
        self.slider_start.valueChanged.connect(self.on_overlay_changed)
        
        self.slider_end = QSlider(Qt.Orientation.Horizontal, self)
        self.slider_end.valueChanged.connect(self.on_overlay_changed)
        
        self.lbl_overlay_range = QLabel("Range: 1 - 1", self)
        
        overlay_layout.addWidget(QLabel("Start:", self))
        overlay_layout.addWidget(self.slider_start)
        overlay_layout.addWidget(QLabel("End:", self))
        overlay_layout.addWidget(self.slider_end)
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

    def on_overlay_changed(self) -> None:
        start = self.slider_start.value()
        end = self.slider_end.value()
        
        if start > end:
            start, end = end, start
            
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
            self.slider_start.setMinimum(1)
            self.slider_start.setMaximum(total)
            self.slider_start.setValue(1)
            
            self.slider_end.setMinimum(1)
            self.slider_end.setMaximum(total)
            self.slider_end.setValue(min(3, total))
            
        self.load_page_data()
        
    def _create_transparent_pixmap(self, image_path: str) -> QPixmap:
        image = QImage(image_path)
        if image.isNull(): return QPixmap()
        
        from PySide6.QtGui import QBitmap
        image = image.convertToFormat(QImage.Format.Format_Mono)
        image = image.convertToFormat(QImage.Format.Format_ARGB32)
        mask = image.createMaskFromColor(QColor(255, 255, 255).rgb(), Qt.MaskMode.MaskOutColor)
        
        black_image = QImage(image.size(), QImage.Format.Format_ARGB32)
        black_image.fill(QColor(0, 0, 0, 80)) # Semi-transparent black
        
        pixmap = QPixmap.fromImage(black_image)
        pixmap.setMask(QBitmap.fromImage(mask))
        return pixmap

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
                        
                        tag_items = self.draw_spatial_tags_overlay(p_data.spatial_tags, float(pixmap.width()), float(pixmap.height()))
                        
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
            self.page_input.setText(str(self.current_page_idx))
            json_path = self.pdf_output_dir / f"page_{self.current_page_idx:03d}.json"
            
            if json_path.exists():
                try:
                    with open(json_path, 'r') as f:
                        data = json.load(f)
                        self.page_data = PageData(**data)
                        logging.info(f"CenterPanel: Loaded page {self.current_page_idx}")
                        self.load_page(self.page_data.image_cache_path, self.page_data.spatial_tags)
                except Exception as e:
                    logging.error(f"Failed to load page data: {e}")
                    self.scene.addText(f"Failed to load page data:\n{e}")
            else:
                self.page_data = None
                self.scene.addText(f"Page {self.current_page_idx} not processed yet.")

    def load_page(self, image_path: str, spatial_tags: List[DocTag]) -> None:
        pixmap = QPixmap(image_path)
        if not pixmap.isNull():
            pixmap_item = QGraphicsPixmapItem(pixmap)
            self.scene.addItem(pixmap_item)
            self.draw_spatial_tags(spatial_tags, float(pixmap.width()), float(pixmap.height()))
            
            self.scene.setSceneRect(pixmap_item.boundingRect())
            self.view.fitInView(self.scene.sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)

    def draw_spatial_tags(self, spatial_tags: List[DocTag], p_width: float, p_height: float) -> None:
        def draw_tags(tags: List[DocTag]) -> None:
            for tag in tags:
                if tag.bbox:
                    x = tag.bbox.x * p_width
                    y = tag.bbox.y * p_height
                    w = tag.bbox.width * p_width
                    h = tag.bbox.height * p_height
                    
                    rect_item = QGraphicsRectItem(QRectF(x, y, w, h))
                    
                    pen = QPen(QColor(255, 0, 0, 150))
                    pen.setWidth(2)
                    rect_item.setPen(pen)
                    
                    if getattr(tag, 'user_removed', False):
                        rect_item.setBrush(QBrush(QColor(100, 100, 100, 100)))
                    
                    rect_item.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, True)
                    rect_item.setData(Qt.ItemDataRole.UserRole, tag)
                    
                    self.scene.addItem(rect_item)
                
                if tag.children:
                    draw_tags(tag.children)
        draw_tags(spatial_tags)

    def draw_spatial_tags_overlay(self, spatial_tags: List[DocTag], p_width: float, p_height: float) -> List[QGraphicsRectItem]:
        items: List[QGraphicsRectItem] = []
        def draw_tags(tags: List[DocTag]) -> None:
            for tag in tags:
                if tag.bbox:
                    x = tag.bbox.x * p_width
                    y = tag.bbox.y * p_height
                    w = tag.bbox.width * p_width
                    h = tag.bbox.height * p_height
                    
                    rect_item = QGraphicsRectItem(QRectF(x, y, w, h))
                    
                    pen = QPen(QColor(255, 0, 0, 150))
                    pen.setWidth(2)
                    rect_item.setPen(pen)
                    
                    if getattr(tag, 'user_removed', False):
                        rect_item.setBrush(QBrush(QColor(100, 100, 100, 100)))
                    
                    # Not selectable in overlay mode
                    rect_item.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, False)
                    
                    self.scene.addItem(rect_item)
                    items.append(rect_item)
                
                if tag.children:
                    draw_tags(tag.children)
        draw_tags(spatial_tags)
        return items

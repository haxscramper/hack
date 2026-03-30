from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QGraphicsView, QGraphicsScene, QGraphicsPixmapItem, QGraphicsRectItem, QGraphicsItem
from PySide6.QtGui import QPixmap, QPen, QColor, QBrush
from PySide6.QtCore import Qt, QRectF

class CenterPanel(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        
        # Graphics View for displaying the page and bounding boxes
        self.scene = QGraphicsScene(self)
        self.view = QGraphicsView(self.scene, self)
        self.view.setDragMode(QGraphicsView.DragMode.RubberBandDrag)
        layout.addWidget(self.view)
        
        # Navigation Controls
        nav_layout = QHBoxLayout()
        self.btn_prev = QPushButton("< Prev", self)
        
        from PySide6.QtWidgets import QLineEdit
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
        
        layout.addLayout(nav_layout)
        
        # State variables
        self.current_page_idx = 0
        self.page_data = None
        
        # Signal connections (placeholders)
        self.btn_prev.clicked.connect(self.load_prev)
        self.btn_next.clicked.connect(self.load_next)
        self.page_input.returnPressed.connect(self.on_page_input_changed)

    def on_page_input_changed(self):
        try:
            page = int(self.page_input.text())
            if page > 0:
                self.current_page_idx = page
                self.load_page_data()
        except ValueError:
            self.page_input.setText(str(self.current_page_idx))

    def load_prev(self):
        if self.current_page_idx > 1:
            self.current_page_idx -= 1
            self.load_page_data()
        
    def load_next(self):
        if self.page_data:
            self.current_page_idx += 1
            self.load_page_data()

    def load_pdf(self, pdf_path, output_dir):
        from pathlib import Path
        self.current_pdf_path = Path(pdf_path)
        self.output_dir = Path(output_dir)
        self.pdf_output_dir = self.output_dir / self.current_pdf_path.stem
        self.current_page_idx = 1
        self.load_page_data()
        
    def load_page_data(self):
        if not hasattr(self, 'pdf_output_dir'): return
        
        self.page_input.setText(str(self.current_page_idx))
        json_path = self.pdf_output_dir / f"page_{self.current_page_idx:03d}.json"
        self.scene.clear()
        
        if json_path.exists():
            import json
            from models import PageData
            try:
                with open(json_path, 'r') as f:
                    data = json.load(f)
                    self.page_data = PageData(**data)
                    self.load_page(self.page_data.image_cache_path, self.page_data.spatial_tags)
            except Exception as e:
                self.scene.addText(f"Failed to load page data:\n{e}")
        else:
            self.page_data = None
            self.scene.addText(f"Page {self.current_page_idx} not processed yet.")

    def load_page(self, image_path, spatial_tags):
        self.scene.clear()
        
        # Add Image
        pixmap = QPixmap(image_path)
        if not pixmap.isNull():
            pixmap_item = QGraphicsPixmapItem(pixmap)
            self.scene.addItem(pixmap_item)
            
            # Helper to recursively draw spatial tags bounding boxes
            def draw_tags(tags):
                for tag in tags:
                    if tag.bbox:
                        # In docling, bounding boxes might be normalized or pixel values.
                        # Assuming normalized values [0, 1] based on standard Docling, scale by pixmap size:
                        x = tag.bbox.x * pixmap.width()
                        y = tag.bbox.y * pixmap.height()
                        w = tag.bbox.width * pixmap.width()
                        h = tag.bbox.height * pixmap.height()
                        
                        rect_item = QGraphicsRectItem(QRectF(x, y, w, h))
                        
                        # Style the bounding box
                        pen = QPen(QColor(255, 0, 0, 150))
                        pen.setWidth(2)
                        rect_item.setPen(pen)
                        
                        # Highlight removed tags
                        if getattr(tag, 'user_removed', False):
                            rect_item.setBrush(QBrush(QColor(100, 100, 100, 100)))
                        
                        rect_item.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, True)
                        rect_item.setData(Qt.ItemDataRole.UserRole, tag) # Store reference to the DocTag
                        
                        self.scene.addItem(rect_item)
                    
                    if tag.children:
                        draw_tags(tag.children)

            draw_tags(spatial_tags)
            
            self.scene.setSceneRect(pixmap_item.boundingRect())
            self.view.fitInView(self.scene.sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)


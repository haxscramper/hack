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
        self.btn_next = QPushButton("Next >", self)
        nav_layout.addStretch()
        nav_layout.addWidget(self.btn_prev)
        nav_layout.addWidget(self.btn_next)
        nav_layout.addStretch()
        
        layout.addLayout(nav_layout)
        
        # State variables
        self.current_page_idx = 0
        self.page_data = None
        
        # Signal connections (placeholders)
        self.btn_prev.clicked.connect(self.load_prev)
        self.btn_next.clicked.connect(self.load_next)

    def load_prev(self):
        pass
        
    def load_next(self):
        pass

    def load_page(self, image_path, spatial_tags):
        self.scene.clear()
        
        # Add Image
        pixmap = QPixmap(image_path)
        if not pixmap.isNull():
            pixmap_item = QGraphicsPixmapItem(pixmap)
            self.scene.addItem(pixmap_item)
            
            # Draw spatial tags bounding boxes
            for tag in spatial_tags:
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
            
            self.scene.setSceneRect(pixmap_item.boundingRect())
            self.view.fitInView(self.scene.sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)


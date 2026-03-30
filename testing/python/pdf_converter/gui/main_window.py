from PySide6.QtWidgets import QMainWindow, QWidget, QHBoxLayout, QSplitter
from PySide6.QtCore import Qt

from .left_panel import LeftPanel
from .center_panel import CenterPanel
from .right_panel import RightPanel

class MainWindow(QMainWindow):
    def __init__(self, config=None):
        super().__init__()
        self.config = config
        self.setWindowTitle("PDF OCR & Post-Processing Tool")
        self.resize(1200, 800)
        
        # Central Widget
        central_widget = QWidget(self)
        self.setCentralWidget(central_widget)
        
        main_layout = QHBoxLayout(central_widget)
        main_layout.setContentsMargins(0, 0, 0, 0)
        
        # Splitter for 3 columns
        splitter = QSplitter(Qt.Orientation.Horizontal, self)
        
        self.left_panel = LeftPanel(config=self.config, parent=self)
        self.center_panel = CenterPanel(self)
        self.right_panel = RightPanel(self)
        
        splitter.addWidget(self.left_panel)
        splitter.addWidget(self.center_panel)
        splitter.addWidget(self.right_panel)
        
        # 1/3 layout logic
        splitter.setSizes([400, 400, 400])
        
        main_layout.addWidget(splitter)

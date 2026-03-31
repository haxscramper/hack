import logging
from typing import Optional

from PySide6.QtWidgets import QMainWindow, QWidget, QHBoxLayout, QSplitter
from PySide6.QtCore import Qt, QModelIndex

from .left_panel import LeftPanel
from .center_panel import CenterPanel
from .right_panel import RightPanel
from config import AppConfig

class MainWindow(QMainWindow):
    def __init__(self, config: Optional[AppConfig] = None) -> None:
        super().__init__()
        self.config = config
        self.setWindowTitle("PDF OCR & Post-Processing Tool")
        self.resize(1200, 800)
        
        logging.info("MainWindow: Initializing GUI.")
        
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
        
        # Connect signals
        self.left_panel.list_view.selectionModel().currentChanged.connect(self.on_pdf_selected)
        self.right_panel.update_btn.clicked.connect(self.on_update_html_clicked)

    def on_update_html_clicked(self) -> None:
        logging.info("MainWindow: Update HTML clicked.")
        if hasattr(self.center_panel, 'get_all_pages_data'):
            pages = self.center_panel.get_all_pages_data()
            if pages:
                self.right_panel.generate_html(pages)
            else:
                logging.warning("MainWindow: No pages data found for HTML generation.")

    def on_pdf_selected(self, current: QModelIndex, previous: QModelIndex) -> None:
        if current.isValid():
            # Get the actual index from the proxy model
            source_index = self.left_panel.proxy_model.mapToSource(current)
            file_path = self.left_panel.model.pdf_files[source_index.row()]
            logging.info(f"MainWindow: User selected PDF: {file_path}")
            if self.config:
                self.center_panel.load_pdf(file_path, self.config.output_dir)

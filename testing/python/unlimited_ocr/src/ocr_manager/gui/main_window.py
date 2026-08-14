import argparse
import logging
import os
from pathlib import Path
from typing import Optional
from beartype import beartype

from PyQt6.QtWidgets import QMainWindow, QWidget, QHBoxLayout, QSplitter
from PyQt6.QtCore import Qt, QModelIndex
from sqlalchemy.orm import sessionmaker

from src.ocr_manager.ocr_db import create_engine_and_tables

from .left_panel import LeftPanel
from .center_panel import CenterPanel
from .right_panel import RightPanel


class MainWindow(QMainWindow):

    def __init__(self, session_factory: Optional[sessionmaker] = None) -> None:
        super().__init__()
        self.session_factory = session_factory
        self.setWindowTitle("PDF OCR & Post-Processing Tool")
        self.resize(1200, 800)

        logging.info("MainWindow: Initializing GUI.")

        central_widget = QWidget(self)
        self.setCentralWidget(central_widget)

        main_layout = QHBoxLayout(central_widget)
        main_layout.setContentsMargins(0, 0, 0, 0)

        splitter = QSplitter(Qt.Orientation.Horizontal, self)

        self.left_panel = LeftPanel(session_factory=self.session_factory,
                                    parent=self)
        self.center_panel = CenterPanel(session_factory=self.session_factory,
                                        parent=self)
        self.right_panel = RightPanel(self)

        splitter.addWidget(self.left_panel)
        splitter.addWidget(self.center_panel)
        splitter.addWidget(self.right_panel)

        splitter.setSizes([400, 400, 400])

        main_layout.addWidget(splitter)

        self.left_panel.list_view.selectionModel().currentChanged.connect(
            self.on_pdf_selected)
        self.right_panel.update_btn.clicked.connect(
            self.on_update_html_clicked)

    def on_update_html_clicked(self) -> None:
        logging.info("MainWindow: Update HTML clicked.")
        pages = self.center_panel.get_all_pages_data()
        if pages:
            output_epub_path = None
            if hasattr(self, 'current_pdf_path') and self.config:
                base_name = os.path.splitext(
                    os.path.basename(self.current_pdf_path))[0]
                output_epub_path = os.path.join(self.config.output_dir,
                                                f"{base_name}.epub")
            self.right_panel.generate_html(pages, output_epub_path)
        else:
            logging.warning(
                "MainWindow: No pages data found for HTML generation.")

    def on_pdf_selected(self, current: QModelIndex,
                        previous: QModelIndex) -> None:
        if current.isValid():
            source_index = self.left_panel.proxy_model.mapToSource(current)
            record = self.left_panel.model.pdf_files[source_index.row()]
            self.current_pdf_path = record.absolute_path
            logging.info(
                f"MainWindow: User selected PDF: {self.current_pdf_path}")
            self.center_panel.load_pdf(self.current_pdf_path)


@beartype
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("db", type=Path)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    db_path = args.db_path.resolve()
    session_factory = create_engine_and_tables(db_path)


if __name__ == "__main__":
    main()

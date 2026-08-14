from loguru import logger
from typing import Optional, List

from PyQt6.QtWidgets import QWidget, QVBoxLayout, QPushButton, QTextBrowser, QCheckBox, QHBoxLayout
from PyQt6.QtCore import QThread, pyqtSignal

from src.ocr_manager.export.html_generator import generate_html_content


class HtmlGeneratorThread(QThread):
    html_ready = pyqtSignal(str)

    def __init__(self,
                 pages_data: List[dict],
                 use_llm: bool,
                 output_epub_path: Optional[str] = None,
                 parent=None):
        super().__init__(parent)
        self.pages_data = pages_data
        self.use_llm = use_llm
        self.output_epub_path = output_epub_path

    def run(self):
        final_html = generate_html_content(
            self.pages_data, output_epub_path=self.output_epub_path)
        self.html_ready.emit(final_html)


class RightPanel(QWidget):

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._layout = QVBoxLayout(self)

        self.html_view = QTextBrowser(self)
        self._layout.addWidget(self.html_view)

        self.bottom_layout = QHBoxLayout()
        self.use_llm_checkbox = QCheckBox("Use LLM for Post-processing", self)
        self.bottom_layout.addWidget(self.use_llm_checkbox)

        self.update_btn = QPushButton("Update HTML", self)
        self.bottom_layout.addWidget(self.update_btn)

        self._layout.addLayout(self.bottom_layout)

        logger.info("RightPanel: Initialized.")

    def generate_html(self,
                      pages_data: List[dict],
                      output_epub_path: Optional[str] = None) -> None:
        use_llm = self.use_llm_checkbox.isChecked()
        self.update_btn.setEnabled(False)
        self.html_view.setHtml(
            "<html><body><p>Generating HTML... Please wait.</p></body></html>")

        self.html_thread = HtmlGeneratorThread(pages_data, use_llm,
                                               output_epub_path)
        self.html_thread.html_ready.connect(self.on_html_ready)
        self.html_thread.start()

    def on_html_ready(self, final_html: str) -> None:
        self.html_view.setHtml(final_html)
        self.update_btn.setEnabled(True)
        self.html_thread = None

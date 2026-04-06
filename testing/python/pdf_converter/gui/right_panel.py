import logging
from typing import Optional, List

from PySide6.QtWidgets import QWidget, QVBoxLayout, QPushButton, QTextBrowser, QCheckBox, QHBoxLayout
from PySide6.QtCore import QThread, Signal
from models import PageData, DocTag
from html_generator import generate_html_content, map_tag_to_html

class HtmlGeneratorThread(QThread):
    html_ready = Signal(str)

    def __init__(self, pages_data: List[PageData], use_llm: bool, output_epub_path: Optional[str] = None, parent=None):
        super().__init__(parent)
        self.pages_data = pages_data
        self.use_llm = use_llm
        self.output_epub_path = output_epub_path

    def run(self):
        final_html = generate_html_content(self.pages_data, output_epub_path=self.output_epub_path)
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
        
        logging.info("RightPanel: Initialized.")

    def map_tag_to_html(self, tag_name: str) -> str:
        mapping = {
            "section_header": "h2",
            "text": "p",
            "list": "ul",
            "list_item": "li",
            "table": "table",
            "footnote": "p",
            "formula": "p",
            "page_footer": "footer",
            "picture": "div",
            "root": "div",
            "unspecified": "div",
            "h1": "h1",
            "h2": "h2",
            "h3": "h3",
            "p": "p",
        }
        return mapping.get(tag_name, "div")

    def generate_html(self, pages_data: List[PageData], output_epub_path: Optional[str] = None) -> None:
        """Generates HTML from the parsed blocks in a separate thread."""
        use_llm = self.use_llm_checkbox.isChecked()
        self.update_btn.setEnabled(False)
        self.html_view.setHtml("<html><body><p>Generating HTML... Please wait.</p></body></html>")
        
        self.html_thread = HtmlGeneratorThread(pages_data, use_llm, output_epub_path)
        self.html_thread.html_ready.connect(self.on_html_ready)
        self.html_thread.start()

    def on_html_ready(self, final_html: str) -> None:
        self.html_view.setHtml(final_html)
        self.update_btn.setEnabled(True)
        self.html_thread = None

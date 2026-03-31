import logging
from typing import Optional, List

from PySide6.QtWidgets import QWidget, QVBoxLayout, QPushButton, QTextBrowser, QCheckBox, QHBoxLayout
from models import PageData, DocTag
from html_generator import generate_html_content, map_tag_to_html

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

    def generate_html(self, pages_data: List[PageData]) -> None:
        """Generates simple mechanical HTML from the parsed blocks."""
        use_llm = self.use_llm_checkbox.isChecked()
        final_html = generate_html_content(pages_data, use_llm=use_llm)
        self.html_view.setHtml(final_html)

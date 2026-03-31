import logging
from typing import Optional, List

from PySide6.QtWidgets import QWidget, QVBoxLayout, QPushButton, QTextBrowser
from models import PageData, DocTag

class RightPanel(QWidget):
    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._layout = QVBoxLayout(self)
        
        self.html_view = QTextBrowser(self)
        self._layout.addWidget(self.html_view)
        
        self.update_btn = QPushButton("Update HTML", self)
        self._layout.addWidget(self.update_btn)
        
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
        html_parts = ["<html><head><style>body { font-family: sans-serif; }</style></head><body>"]
        
        for page in pages_data:
            html_parts.append(f"<hr/><h3>Page {page.page_number}</h3>")
            
            def process_tag(tag: DocTag):
                if tag.user_removed:
                    return
                raw_tag_name = tag.user_tag_override or tag.tag_name
                html_tag = self.map_tag_to_html(raw_tag_name)
                
                text = tag.user_edited_text if tag.user_edited_text is not None else tag.text

                if text:
                    html_parts.append(f"<{html_tag}>{text}</{html_tag}>")
                else:
                    html_parts.append(f"<{html_tag}>")
                    for child in tag.children:
                        process_tag(child)
                    html_parts.append(f"</{html_tag}>")
            
            for tag in page.spatial_tags:
                process_tag(tag)
                
        html_parts.append("</body></html>")
        final_html = "\n".join(html_parts)
        self.html_view.setHtml(final_html)

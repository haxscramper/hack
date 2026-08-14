import logging
from typing import Optional

from PyQt6.QtWidgets import QWidget, QVBoxLayout, QPushButton, QTextBrowser
from sqlalchemy.orm import sessionmaker

from ocr_manager.export.html_generator import generate_markdown_content


class RightPanel(QWidget):

    def __init__(self,
                 session_factory: sessionmaker,
                 parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.session_factory = session_factory
        self._layout = QVBoxLayout(self)

        self.md_view = QTextBrowser(self)
        self._layout.addWidget(self.md_view)

        self.update_btn = QPushButton("Update Markdown", self)
        self._layout.addWidget(self.update_btn)

        logging.info("RightPanel: Initialized.")

    def generate_markdown(self,
                          absolute_path: str,
                          output_md_path: Optional[str] = None) -> None:
        if not self.session_factory:
            logging.warning("RightPanel: No session factory configured.")
            return
        self.update_btn.setEnabled(False)
        try:
            final_md = generate_markdown_content(self.session_factory,
                                                 absolute_path)
            self.md_view.setMarkdown(final_md)
        finally:
            self.update_btn.setEnabled(True)

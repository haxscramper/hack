import logging
from typing import Optional

from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel

class RightPanel(QWidget):
    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        layout = QVBoxLayout(self)
        layout.addWidget(QLabel("Right Panel (HTML & LLM)", self))
        logging.info("RightPanel: Initialized.")

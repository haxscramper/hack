from __future__ import annotations

from PySide6.QtWidgets import (
    QDialog,
    QPlainTextEdit,
    QPushButton,
    QVBoxLayout,
)
from PySide6.QtGui import QFont
from PySide6.QtCore import Qt


class ExportDialog(QDialog):

    def __init__(self, text: str, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Export ASCII Diagram")
        self.resize(600, 400)

        layout = QVBoxLayout(self)

        self._text_edit = QPlainTextEdit()
        font = QFont("Courier New", 11)
        font.setStyleHint(QFont.Monospace)
        self._text_edit.setFont(font)
        self._text_edit.setReadOnly(True)
        self._text_edit.setPlainText(text)
        layout.addWidget(self._text_edit)

        copy_btn = QPushButton("Copy to Clipboard")
        copy_btn.clicked.connect(self._copy)
        layout.addWidget(copy_btn)

    def _copy(self) -> None:
        from PySide6.QtWidgets import QApplication

        QApplication.clipboard().setText(self._text_edit.toPlainText())

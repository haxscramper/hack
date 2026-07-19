import json
import subprocess

from beartype.typing import Optional

from PyQt6.QtCore import (
    QTimer,)

from PyQt6.QtWidgets import (
    QPlainTextEdit,
    QVBoxLayout,
    QWidget,
)


class JsonPreviewWidget(QWidget):

    def __init__(self, parent: Optional[QWidget] = None, REFLOW_DEBOUNCE_MS=200) -> None:
        super().__init__(parent)
        self._view = QPlainTextEdit()
        self._view.setReadOnly(True)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self._view)
        self._doc: Optional[dict] = None
        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.timeout.connect(self.reflow)
        self.REFLOW_DEBOUNCE_MS = REFLOW_DEBOUNCE_MS

    def set_doc(self, doc: Optional[dict]) -> None:
        self._doc = doc
        self.reflow()

    def resizeEvent(self, event) -> None:  # type: ignore[override]
        super().resizeEvent(event)
        self._timer.start(self.REFLOW_DEBOUNCE_MS)

    def reflow(self) -> None:
        if self._doc is None:
            self._view.setPlainText("<no document>")
            return
        char_width = self._view.fontMetrics().horizontalAdvance("0")
        px_width = self._view.viewport().width() - 2
        width = max(40, px_width // max(1, char_width))
        proc = subprocess.run(
            ["fjson", "-w", str(width)],
            input=json.dumps(self._doc, default=str),
            capture_output=True,
            text=True,
            check=True,
        )
        self._view.setPlainText(proc.stdout)

from beartype import beartype
from PySide6.QtCore import Signal
from PySide6.QtWidgets import QPushButton


class ClickablePill(QPushButton):
    removed = Signal()

    def __init__(self, text: str, removable: bool = False, parent=None):
        super().__init__(text, parent)
        self.setCheckable(False)
        self.removable = removable

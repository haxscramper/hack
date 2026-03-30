from PySide6.QtWidgets import QWidget, QVBoxLayout, QLabel

class CenterPanel(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        layout.addWidget(QLabel("Center Panel (Preview & Overlay)", self))

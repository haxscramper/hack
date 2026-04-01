from pathlib import Path
from PySide6.QtCore import Qt, QSize
from PySide6.QtGui import QPixmap
from PySide6.QtWidgets import QLabel, QWidget, QVBoxLayout


class ImageThumbWidget(QWidget):
    def __init__(self, image_path: Path, size: int = 120, parent=None):
        super().__init__(parent)
        self.image_path = image_path
        layout = QVBoxLayout(self)

        self.image_label = QLabel()
        self.image_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.image_label.setFixedSize(QSize(size, size))

        pix = QPixmap(str(image_path))
        if not pix.isNull():
            pix = pix.scaled(
                size,
                size,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation,
            )
            self.image_label.setPixmap(pix)
        else:
            self.image_label.setText(image_path.name)

        self.text_label = QLabel(image_path.name)
        self.text_label.setWordWrap(True)

        layout.addWidget(self.image_label)
        layout.addWidget(self.text_label)

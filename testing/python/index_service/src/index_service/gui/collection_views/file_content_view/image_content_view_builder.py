from PyQt6.QtCore import Qt
from PyQt6.QtGui import QPixmap
from PyQt6.QtWidgets import QLabel, QWidget
from beartype import beartype

from index_service.gui.collection_views.file_content_view.file_content_builder import FileContentViewBuilder


@beartype
class ImageFileContentViewBuilder(FileContentViewBuilder):

    def can_build(self, mime: str) -> bool:
        return mime.startswith("image/")

    def build(self, absolute_path: str) -> QWidget:
        label = QLabel()
        label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        pixmap = QPixmap(absolute_path)
        label.setPixmap(pixmap)
        return label

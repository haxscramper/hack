from PyQt6.QtCore import Qt, QSize
from PyQt6.QtGui import QPixmap
from PyQt6.QtWidgets import QLabel, QWidget, QSizePolicy
from beartype import beartype

from index_service.gui.collection_views.file_content_view.file_content_builder import FileContentViewBuilder


@beartype
class ScaledImagePreview(QLabel):

    def __init__(self, absolute_path: str, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._source = QPixmap(absolute_path)
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Ignored)
        self.setMinimumSize(0, 0)
        self._rescale()

    def sizeHint(self) -> QSize:
        return QSize(320, 240)

    def minimumSizeHint(self) -> QSize:
        return QSize(0, 0)

    def resizeEvent(self, event) -> None:
        super().resizeEvent(event)
        self._rescale()

    def _rescale(self) -> None:
        if self._source.isNull():
            self.clear()
            return
        self.setPixmap(
            self._source.scaled(
                self.size(),
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation,
            ))


@beartype
class ImageFileContentViewBuilder(FileContentViewBuilder):

    def can_build(self, mime: str) -> bool:
        return mime.startswith("image/")

    def build(self, absolute_path: str) -> QWidget:
        return ScaledImagePreview(absolute_path)

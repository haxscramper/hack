from PyQt6.QtCore import QUrl
from PyQt6.QtMultimedia import QMediaPlayer, QAudioOutput
from PyQt6.QtMultimediaWidgets import QVideoWidget
from PyQt6.QtWidgets import QWidget, QVBoxLayout
from beartype import beartype

from index_service.gui.collection_views.file_content_view.file_content_builder import FileContentViewBuilder


@beartype
class VideoFileContentViewBuilder(FileContentViewBuilder):

    def can_build(self, mime: str) -> bool:
        return mime.startswith("video/")

    def build(self, absolute_path: str) -> QWidget:
        container = QWidget()
        layout = QVBoxLayout(container)
        layout.setContentsMargins(0, 0, 0, 0)

        video = QVideoWidget(container)
        layout.addWidget(video)

        player = QMediaPlayer(container)
        audio = QAudioOutput(container)
        audio.setVolume(0.0)

        player.setAudioOutput(audio)
        player.setVideoOutput(video)
        player.setSource(QUrl.fromLocalFile(absolute_path))
        player.play()

        container._player = player  # keep refs alive
        container._audio = audio
        return container

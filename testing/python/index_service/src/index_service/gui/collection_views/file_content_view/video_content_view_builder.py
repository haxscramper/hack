from PyQt6.QtCore import QUrl, Qt, QSize
from PyQt6.QtMultimedia import QMediaPlayer, QAudioOutput
from PyQt6.QtMultimediaWidgets import QVideoWidget
from PyQt6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QLabel, QSlider, QStyle, QSizePolicy
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
        layout.setSpacing(0)

        video = QVideoWidget(container)
        layout.addWidget(video, 1)

        player = QMediaPlayer(container)
        audio = QAudioOutput(container)
        audio.setVolume(0.5)

        player.setAudioOutput(audio)
        player.setVideoOutput(video)
        player.setSource(QUrl.fromLocalFile(absolute_path))

        controls = QWidget(container)
        controls.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Fixed)
        controls_layout = QHBoxLayout(controls)
        controls_layout.setContentsMargins(4, 4, 4, 4)

        style = container.style()
        icon_play = style.standardIcon(QStyle.StandardPixmap.SP_MediaPlay)
        icon_pause = style.standardIcon(QStyle.StandardPixmap.SP_MediaPause)
        icon_volume = style.standardIcon(QStyle.StandardPixmap.SP_MediaVolume)

        play_btn = QPushButton(controls)
        play_btn.setIcon(icon_pause)
        play_btn.setFixedSize(28, 28)

        position = QSlider(Qt.Orientation.Horizontal, controls)
        time_label = QLabel("0:00 / 0:00", controls)

        volume_icon = QLabel(controls)
        volume_icon.setPixmap(icon_volume.pixmap(16, 16))

        volume = QSlider(Qt.Orientation.Horizontal, controls)
        volume.setRange(0, 100)
        volume.setValue(50)
        volume.setFixedWidth(100)

        controls_layout.addWidget(play_btn)
        controls_layout.addWidget(position, 1)
        controls_layout.addWidget(time_label)
        controls_layout.addWidget(volume_icon)
        controls_layout.addWidget(volume)
        layout.addWidget(controls)

        def toggle_play():
            if player.playbackState() == QMediaPlayer.PlaybackState.PlayingState:
                player.pause()
                play_btn.setIcon(icon_play)
            else:
                player.play()
                play_btn.setIcon(icon_pause)

        def fmt(ms: int) -> str:
            s = ms // 1000
            return f"{s // 60}:{s % 60:02d}"

        def on_duration(dur: int):
            position.setRange(0, dur)
            time_label.setText(f"{fmt(player.position())} / {fmt(dur)}")

        def on_position(pos: int):
            if not position.isSliderDown():
                position.setValue(pos)
            time_label.setText(f"{fmt(pos)} / {fmt(player.duration())}")

        play_btn.clicked.connect(toggle_play)
        position.sliderMoved.connect(player.setPosition)
        volume.valueChanged.connect(lambda v: audio.setVolume(v / 100.0))
        player.durationChanged.connect(on_duration)
        player.positionChanged.connect(on_position)

        player.play()

        container._player = player  # keep refs alive
        container._audio = audio
        return container

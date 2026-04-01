from __future__ import annotations

from pathlib import Path

from PySide6.QtWidgets import QMainWindow, QWidget, QHBoxLayout, QSplitter
from PySide6.QtCore import Qt

from image_tagger.db.repository import Repository
from image_tagger.gui.image_directory_view import MixedTreeTileView
from image_tagger.gui.center_panel import CenterPanel
from image_tagger.gui.right_panel import RightPanel


class MainWindow(QMainWindow):
    def __init__(self, root_dir: Path, repository: Repository):
        super().__init__()
        self.root_dir = root_dir
        self.repository = repository
        self.current_image = None

        self.setWindowTitle("Image Tagger")
        self.resize(1600, 900)

        central = QWidget()
        self.setCentralWidget(central)
        layout = QHBoxLayout(central)

        splitter = QSplitter(Qt.Horizontal)

        self.left_panel = MixedTreeTileView(root_dir)
        self.center_panel = CenterPanel()
        self.right_panel = RightPanel(root_dir)

        splitter.addWidget(self.left_panel)
        splitter.addWidget(self.center_panel)
        splitter.addWidget(self.right_panel)
        splitter.setSizes([350, 700, 550])

        layout.addWidget(splitter)

        self.left_panel.fileSelected.connect(self.on_file_selected)
        self.center_panel.probabilisticTagAdded.connect(self.on_prob_tag_added)
        self.center_panel.regularTagAdded.connect(self.on_regular_tag_added)
        self.center_panel.regularTagDeleted.connect(self.on_regular_tag_deleted)
        self.center_panel.descriptionSaved.connect(self.on_description_saved)

    def on_file_selected(self, file_path: str):
        entry = self.repository.get_image_by_path(file_path)
        if entry is None:
            entry = self.repository.upsert_image(self.root_dir, Path(file_path))

        self.current_image = entry

        prob_rows = self.repository.list_probabilistic_tags(entry.id)
        prob_tags = [(tag.name, rel.probability) for rel, tag in prob_rows]
        self.center_panel.set_probabilistic_tags(prob_tags)

        reg_rows = self.repository.list_regular_tags(entry.id)
        reg_tags = [(tag.category, tag.name) for rel, tag in reg_rows]
        self.center_panel.set_regular_tags(reg_tags)

        desc = self.repository.get_description(entry.id)
        self.center_panel.set_description(desc.description if desc else "")

    def on_prob_tag_added(self, name: str, probability: float):
        if not self.current_image:
            return
        self.repository.set_probabilistic_tag(self.current_image.id, name, probability)
        self.on_file_selected(self.current_image.full_path)

    def on_regular_tag_added(self, category: str, name: str):
        if not self.current_image:
            return
        self.repository.add_regular_tag(self.current_image.id, category, name)
        self.on_file_selected(self.current_image.full_path)

    def on_regular_tag_deleted(self, category: str, name: str):
        if not self.current_image:
            return
        self.repository.delete_regular_tag(self.current_image.id, category, name)
        self.on_file_selected(self.current_image.full_path)

    def on_description_saved(self, text: str):
        if not self.current_image:
            return
        self.repository.set_description(self.current_image.id, text)

from io import TextIOWrapper
from pathlib import Path
import json
import logging
from typing import Any

from pydantic import BaseModel
from PyQt6.QtCore import pyqtSignal, QModelIndex, Qt
from PyQt6.QtWidgets import QHeaderView, QPushButton, QTableView, QVBoxLayout, QWidget, QHBoxLayout

from index_service.gui.common.qt_model_roles import CustomModelRole
from index_service.gui.file_tree.query_filter import ActionListModel
from index_service.services.core.types import FileHash
from index_service.services.pydantic_utils import model_to_json_data

log = logging.getLogger(__name__)


class ActionListView(QWidget):
    file_hash_activated = pyqtSignal(object)

    def __init__(self,
                 actions: ActionListModel,
                 action_file: Path,
                 parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self.action_file = action_file
        self.list_view = QTableView(self)
        self.list_view.setModel(actions)
        self.list_view.setSelectionBehavior(QTableView.SelectionBehavior.SelectRows)
        self.list_view.setSelectionMode(QTableView.SelectionMode.SingleSelection)
        self.list_view.verticalHeader().setVisible(True)  # auto row numbers
        self.list_view.verticalHeader().setDefaultSectionSize(18)
        self.list_view.doubleClicked.connect(self._on_tree_item_double_clicked)

        # Keep full cell content visible via horizontal scrolling instead of truncation.
        header = self.list_view.horizontalHeader()
        assert header
        header.setStretchLastSection(False)
        header.setSectionResizeMode(
            QHeaderView.ResizeMode.Interactive)  # no per-resize full scan
        self.list_view.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOn)
        self.list_view.setHorizontalScrollMode(QTableView.ScrollMode.ScrollPerPixel)
        self.list_view.setTextElideMode(Qt.TextElideMode.ElideNone)
        self.list_view.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOn)
        self.list_view.resizeColumnsToContents()

        self.save_actions_button = QPushButton("save actions", self)
        self.save_actions_button.clicked.connect(self._on_save_actions_clicked)

        self.overwrite_actions_button = QPushButton("overwrite actions", self)
        self.overwrite_actions_button.clicked.connect(self._overwrite_actions)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.list_view)

        buttons_layout = QHBoxLayout()
        buttons_layout.addWidget(self.save_actions_button)
        buttons_layout.addWidget(self.overwrite_actions_button)

        layout.addLayout(buttons_layout)

    def _on_tree_item_double_clicked(self, index: QModelIndex) -> None:
        hash_value = index.data(CustomModelRole.HashRole.value)
        if hash_value is not None:
            assert isinstance(hash_value, str), type(hash_value)
            self.file_hash_activated.emit(FileHash(hash=hash_value))

    def _write_actions(self, out: TextIOWrapper):
        model = self.list_view.model()
        assert model is not None

        for row in range(model.rowCount()):
            index = model.index(row, 0)
            action = index.data(CustomModelRole.ActionRole.value)
            if action is None:
                continue

            assert isinstance(action, BaseModel), type(action)
            json_data = model_to_json_data(action)
            out.write(json.dumps(json_data, ensure_ascii=False))
            out.write("\n")

    def _overwrite_actions(self) -> None:
        with self.action_file.open("w", encoding="utf-8") as out:
            self._write_actions(out)

        log.info(f"Saved actions to {self.action_file}")

    def _on_save_actions_clicked(self) -> None:
        with self.action_file.open("a", encoding="utf-8") as out:
            self._write_actions(out)

        log.info(f"Saved actions to {self.action_file}")

from pathlib import Path
import json
import logging
from typing import Any

from pydantic import BaseModel
from PyQt6.QtCore import pyqtSignal, QModelIndex, Qt
from PyQt6.QtWidgets import QHeaderView, QPushButton, QTableView, QVBoxLayout, QWidget

from index_service.gui.common.qt_model_roles import CustomModelRole
from index_service.gui.file_tree.query_filter import ActionListModel
from index_service.services.core.types import FileHash
from index_service.services.pydantic_utils import model_to_json_data

log = logging.getLogger(__name__)

ACTIONS_FILE = Path("/tmp/index-result-actions.jsonl")


class ActionListView(QWidget):
    file_hash_activated = pyqtSignal(object)

    def __init__(self, actions: ActionListModel, parent: QWidget | None = None) -> None:
        super().__init__(parent)

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

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.list_view)
        layout.addWidget(self.save_actions_button)

    def _on_tree_item_double_clicked(self, index: QModelIndex) -> None:
        log.info("double click on the item tree action")
        hash_value = index.data(CustomModelRole.HashRole.value)
        log.info(f"hash data is {hash_value}")
        if hash_value is None:
            log.info("hash value is None")
        else:
            log.info("Emit file hash activated")
            assert isinstance(hash_value, str), type(hash_value)
            self.file_hash_activated.emit(FileHash(hash=hash_value))

    def _on_save_actions_clicked(self) -> None:
        model = self.list_view.model()
        assert model is not None

        with ACTIONS_FILE.open("a", encoding="utf-8") as out:
            for row in range(model.rowCount()):
                index = model.index(row, 0)
                action = index.data(CustomModelRole.ActionRole.value)
                if action is None:
                    continue

                assert isinstance(action, BaseModel), type(action)
                json_data = model_to_json_data(action)
                out.write(json.dumps(json_data, ensure_ascii=False))
                out.write("\n")

        log.info(f"Saved actions to {ACTIONS_FILE}")

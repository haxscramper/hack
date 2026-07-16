from PyQt6.QtCore import pyqtSignal, QModelIndex
from PyQt6.QtWidgets import QListView, QVBoxLayout, QWidget

from index_service.gui.common.qt_model_roles import CustomModelRole
import logging

from index_service.gui.file_tree.query_filter import ActionListModel
from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)


class ActionListView(QWidget):
    file_hash_activated = pyqtSignal(object)

    def __init__(self, actions: ActionListModel, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self.list_view = QListView(self)
        self.list_view.setUniformItemSizes(True)
        self.list_view.setLayoutMode(QListView.LayoutMode.Batched)
        self.list_view.setBatchSize(512)
        self.list_view.setModel(actions)
        self.list_view.doubleClicked.connect(self._on_tree_item_double_clicked)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.list_view)

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

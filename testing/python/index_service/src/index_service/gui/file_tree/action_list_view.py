from PyQt6.QtWidgets import QListView, QVBoxLayout, QWidget

from index_service.gui.file_tree.action_list_model import ActionListModel
from index_service.gui.file_tree.query_filter import BaseAction


class ActionListView(QWidget):

    def __init__(self, actions: ActionListModel, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self.list_view = QListView(self)
        self.list_view.setModel(actions)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.list_view)

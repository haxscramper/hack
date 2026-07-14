from PyQt6.QtCore import QAbstractListModel, QModelIndex, Qt

from index_service.gui.file_tree.query_filter import BaseAction


class ActionListModel(QAbstractListModel):

    def __init__(self, actions: list[BaseAction], parent=None) -> None:
        super().__init__(parent)
        self._actions = actions

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self._actions)

    def data(self, index: QModelIndex, role: int = Qt.ItemDataRole.DisplayRole) -> object:
        if not index.isValid():
            return None

        if role == Qt.ItemDataRole.DisplayRole:
            return str(self._actions[index.row()])

        return None

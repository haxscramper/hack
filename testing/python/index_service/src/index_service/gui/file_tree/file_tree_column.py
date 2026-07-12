from PySide6.QtCore import QModelIndex, Qt
from beartype import beartype

from index_service.gui.file_tree.base_tree_model import FileTreeNode
from index_service.gui.file_tree.column_model import ColumnSpec
from beartype.typing import Any, cast


@beartype
class FileTreeColumnSpec(ColumnSpec):

    def __init__(self, title: str) -> None:
        self.title = title

    def setData(
        self,
        index: QModelIndex,
        value: Any,
        role: int = Qt.ItemDataRole.EditRole,
    ) -> bool:
        return False

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        return Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable

    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        match orientation, role:
            case Qt.Orientation.Horizontal, Qt.ItemDataRole.DisplayRole:
                return self.title
            case _:
                return None

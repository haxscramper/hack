from PySide6.QtCore import QModelIndex, Qt
from beartype import beartype

from beartype.typing import Any

from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec


@beartype
class FileNameColumnSpec(FileTreeColumnSpec):

    def __init__(self) -> None:
        super().__init__("Name")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        match role:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole:
                return self.node(index).path.name
            case _:
                return None

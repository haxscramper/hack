from PySide6.QtCore import QModelIndex, Qt
from beartype import beartype
from beartype.typing import Any

from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec


@beartype
class FileHashColumnSpec(FileTreeColumnSpec):

    def __init__(self) -> None:
        super().__init__("Hash")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        node = self.node(index)

        match role, node.hash:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole, None:
                return ""
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole, hash:
                assert hash
                return str(hash.hash)
            case _:
                return None

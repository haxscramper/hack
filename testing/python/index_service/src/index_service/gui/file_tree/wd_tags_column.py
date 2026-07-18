from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype
from beartype.typing import Any

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec


@beartype
class WdTagsColumnSpec(FileTreeColumnSpec):

    def __init__(self) -> None:
        super().__init__("WD Tags")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        present = "wd_tags" in self.node(index).assets

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return "Yes" if present else ""
            case Qt.ItemDataRole.CheckStateRole:
                return Qt.CheckState.Checked if present else Qt.CheckState.Unchecked
            case _:
                return None

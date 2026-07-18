from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype
from beartype.typing import Any
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from beartype.typing import cast, Optional


class FileHashData(BaseModel, extra="forbid"):
    hash: str


@beartype
class FileHashColumnSpec(FileTreeColumnSpec):
    column_name = "file_hash"
    column_type = FileHashData

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        if hash:
            return FileHashData(hash=hash.hash)

        else:
            return None

    def __init__(self) -> None:
        super().__init__("Hash")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        node = cast(FileHashData, self.getColumnData(index))

        match role, node:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole, FileHashData():
                return node.hash

            case _:
                return None

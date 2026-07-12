from pathlib import Path

from PySide6.QtCore import QModelIndex, Qt
from beartype import beartype
from beartype.typing import Any
from pydantic import BaseModel

from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
from index_service.services.core.types import FileHash
from beartype.typing import cast, Optional


class FileHashData(BaseModel, extra="forbid"):
    hash: str


@beartype
class FileHashColumnSpec(FileTreeColumnSpec):
    column_name = "file_hash"
    column_type = FileHashData

    @staticmethod
    def initColumnData(path: Path, hash: FileHash,
                       assets: dict[str, BaseModel]) -> Optional[BaseModel]:
        return FileHashData(hash=hash.hash)

    def __init__(self) -> None:
        super().__init__("Hash")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        node = cast(FileHashData, self.getColumnData(index))

        match role:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole:
                return node.hash

            case _:
                return None

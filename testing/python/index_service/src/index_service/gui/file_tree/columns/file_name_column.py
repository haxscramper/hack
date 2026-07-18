from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype

from beartype.typing import Any, cast, Optional
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash


class FileNameData(BaseModel, extra="forbid"):
    name: str


@beartype
class FileNameColumnSpec(FileTreeColumnSpec):
    column_type = FileNameData
    column_name = "file_name"

    @staticmethod
    def initColumnData(
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> BaseModel:
        return FileNameData(name=path.name)

    def __init__(self) -> None:
        super().__init__("Name")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        data = cast(FileNameData, self.getColumnData(index))
        assert data is not None, "File name column spec cannot be null"
        match role:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole:
                return data.name

            case _:
                return None

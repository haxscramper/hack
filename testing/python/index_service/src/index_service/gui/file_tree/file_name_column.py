from pathlib import Path

from PySide6.QtCore import QModelIndex, Qt
from beartype import beartype

from beartype.typing import Any, cast
from pydantic import BaseModel

from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
from index_service.services.core.types import FileHash


class FileNameData(BaseModel, extra="forbid"):
    name: str


@beartype
class FileNameColumnSpec(FileTreeColumnSpec):
    column_type = FileNameData
    column_name = "file_name"

    @staticmethod
    def initColumnData(path: Path, hash: FileHash, assets: dict[str,
                                                                BaseModel]) -> BaseModel:
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

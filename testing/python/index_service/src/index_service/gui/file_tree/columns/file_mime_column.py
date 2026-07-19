from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype
from beartype.typing import Any, Optional, cast
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from index_service.services.indexers.mime_indexer import FileMimeIndexer, FileMimeIndexerResult


class FileMimeData(BaseModel, extra="forbid"):
    mime_type: str


@beartype
class FileMimeColumnSpec(FileTreeColumnSpec):
    column_name = "file_mime"
    column_type = FileMimeData

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        if FileMimeIndexer.asset_name in assets:
            result = cast(FileMimeIndexerResult, assets[FileMimeIndexer.asset_name])
            return FileMimeData(mime_type=result.mime_type)

        return None

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        mime = cast(Optional[FileMimeData], self.getColumnData(index))

        if not mime:
            return None

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return mime.mime_type
            case _:
                return None

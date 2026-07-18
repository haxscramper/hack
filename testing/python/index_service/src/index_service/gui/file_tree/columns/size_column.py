from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype

from beartype.typing import Any, cast, Optional
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer, FFProbeIndexerResult, FFProbeInfoModel
from index_service.services.indexers.file_stats import FileStatsIndexerResult, FileStatsIndexer
from index_service.services.utils import format_size


class EntrySizeData(BaseModel, extra="forbid"):
    size_bytes: int


@beartype
class EntrySizeColumnSpec(FileTreeColumnSpec):
    column_name = "entry_size"
    column_type = EntrySizeData

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        if is_directory:
            size_bytes = 0
            for node in nested:
                duplicate_data = cast(
                    Optional[EntrySizeData],
                    node.columns.get(self.column_name),
                )

                if duplicate_data is None:
                    continue

                size_bytes += duplicate_data.size_bytes

            return EntrySizeData(size_bytes=size_bytes)

        elif FileStatsIndexer.asset_name in assets:
            result = cast(FileStatsIndexerResult, assets[FileStatsIndexer.asset_name])
            return EntrySizeData(size_bytes=result.size_bytes)

        else:
            return None

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        rate = cast(Optional[EntrySizeData], self.getColumnData(index))

        if not rate:
            return None

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return format_size(rate.size_bytes)

            case _:
                return None

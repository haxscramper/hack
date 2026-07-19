from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype

from beartype.typing import Any, cast, Optional
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer, FFProbeIndexerResult, FFProbeInfoModel


class VideoResolutionData(BaseModel, extra="forbid"):
    probe: FFProbeInfoModel


@beartype
def format_resolution(probe: FFProbeInfoModel) -> str:
    if probe.width is None or probe.height is None:
        return ""

    return f"{probe.width}x{probe.height}"


@beartype
class VideoResolutionColumnSpec(FileTreeColumnSpec):
    column_name = "video_resolution"
    column_type = VideoResolutionData

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        if FFProbeIndexer.asset_name in assets:
            result = cast(FFProbeIndexerResult, assets[FFProbeIndexer.asset_name])
            if result.probe:
                return VideoResolutionData(probe=result.probe)

        return None

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        res = cast(Optional[VideoResolutionData], self.getColumnData(index))

        if not res:
            return None

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return format_resolution(res.probe)

            case Qt.ItemDataRole.UserRole:
                if res.probe.width is None or res.probe.height is None:
                    return None

                return res.probe.width * res.probe.height

            case _:
                return None

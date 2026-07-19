from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype

from beartype.typing import Any, cast, Optional
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer, FFProbeIndexerResult, FFProbeInfoModel


class VideoFramerateData(BaseModel, extra="forbid"):
    probe: FFProbeInfoModel


@beartype
def format_framerate(probe: FFProbeInfoModel) -> str:
    if probe.fps is None:
        return ""

    if abs(probe.fps - round(probe.fps)) < 1e-6:
        return f"{int(round(probe.fps))} fps"

    return f"{probe.fps:.3f} fps"


@beartype
class VideoFramerateColumnSpec(FileTreeColumnSpec):
    column_name = "video_framerate"
    column_type = VideoFramerateData

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
                return VideoFramerateData(probe=result.probe)

        return None

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        rate = cast(Optional[VideoFramerateData], self.getColumnData(index))

        if not rate:
            return None

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return format_framerate(rate.probe)

            case Qt.ItemDataRole.UserRole:
                return rate.probe.fps

            case _:
                return None

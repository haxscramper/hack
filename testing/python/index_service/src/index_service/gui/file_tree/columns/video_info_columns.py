from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype

from beartype.typing import Any, cast, Optional
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer, FFProbeIndexerResult, FFProbeInfoModel


class VideoBitrateData(BaseModel, extra="forbid"):
    probe: FFProbeInfoModel


@beartype
def format_bitrate(probe: FFProbeInfoModel) -> str:
    if probe.bitrate_bps is None:
        return ""

    result = f"{probe.bitrate_bps / 1_000_000:.2f} Mbps"

    if probe.width and probe.height:
        result += f" ({probe.bitrate_bps / (probe.height * probe.width):.2f} bpps)"

    return result


@beartype
class VideoBitrateColumnSpec(FileTreeColumnSpec):
    column_name = "video_bitrate"
    column_type = VideoBitrateData

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
                return VideoBitrateData(probe=result.probe)

        return None

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        rate = cast(Optional[VideoBitrateData], self.getColumnData(index))

        if not rate:
            return None

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return format_bitrate(rate.probe)

            case _:
                return None

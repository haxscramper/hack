from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt, QAbstractItemModel
from PyQt6.QtWidgets import QStyledItemDelegate, QWidget, QStyleOptionViewItem, QComboBox, QAbstractItemDelegate
from beartype import beartype

from beartype.typing import Any, cast, Optional
from pydantic import BaseModel
from rich.console import NO_CHANGE

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer
from index_service.services.utils import format_size
import enum

import logging

log = logging.getLogger(__name__)


class VideoConvertTarget(str, enum.Enum):
    NO_CHANGE = "NO_CHANGE"
    SD_30 = "480p30"
    HD_30 = "720p30"
    FULL_HD_30 = "1080p30"


class VideoConvertData(BaseModel, extra="forbid"):
    target: VideoConvertTarget = VideoConvertTarget.NO_CHANGE


class PresetDelegate(QStyledItemDelegate):

    @beartype
    def __init__(
        self,
        target_enum: type[VideoConvertTarget] = VideoConvertTarget,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._target_enum = target_enum

    @beartype
    def createEditor(
        self,
        parent: QWidget,
        option: QStyleOptionViewItem,
        index: QModelIndex,
    ) -> QWidget:
        editor = QComboBox(parent)
        for target in self._target_enum:
            editor.addItem(target.value, target)
        return editor

    @beartype
    def setEditorData(self, editor: QWidget, index: QModelIndex) -> None:
        if not isinstance(editor, QComboBox):
            return

        raw_value = index.data(Qt.ItemDataRole.EditRole)

        if isinstance(raw_value, self._target_enum):
            value = raw_value.value
        elif raw_value is None:
            value = VideoConvertTarget.NO_CHANGE.value
        else:
            value = str(raw_value)

        pos = editor.findText(value)
        editor.setCurrentIndex(pos if pos >= 0 else 0)

    @beartype
    def setModelData(
        self,
        editor: QWidget,
        model: QAbstractItemModel,
        index: QModelIndex,
    ) -> None:
        if not isinstance(editor, QComboBox):
            return

        model.setData(index, editor.currentText(), Qt.ItemDataRole.EditRole)


@beartype
class VideoConvertColumnSpec(FileTreeColumnSpec):
    column_name = "video_convert"
    column_type = VideoConvertData

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        if not is_directory and FFProbeIndexer.asset_name in assets:
            return VideoConvertData()

        else:
            return None

    def setData(
        self,
        index: QModelIndex,
        value: Any,
        role: int = Qt.ItemDataRole.EditRole,
    ) -> bool:
        self.setColumnData(index, VideoConvertData(target=VideoConvertTarget(str(value))))
        return True

    def getDelegate(self) -> QAbstractItemDelegate | None:
        return PresetDelegate()

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        rate = cast(Optional[VideoConvertData], self.getColumnData(index))
        if rate:
            return super().flags(index) | Qt.ItemFlag.ItemIsEditable

        else:
            return super().flags(index)

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        rate = cast(Optional[VideoConvertData], self.getColumnData(index))

        if not rate:
            return None

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return str(rate.target.value)

            case _:
                return None

import os
from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from PyQt6.QtGui import QPainter
from PyQt6.QtWidgets import (
    QApplication,
    QAbstractItemDelegate,
    QStyle,
    QStyleOptionProgressBar,
    QStyleOptionViewItem,
    QStyledItemDelegate,
)
from beartype import beartype
from beartype.typing import Any, Literal, Optional, cast
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.core.types import FileHash
from index_service.services.indexers.file_stats import FileStatsIndexer, FileStatsIndexerResult
import logging

log = logging.getLogger(__name__)


class SizeShareData(BaseModel, extra="forbid"):
    size_self: int
    size_parent: float


@beartype
def _compute_directory_size_bytes(parent_directories: list[Path]) -> dict[Path, int]:
    directory_size_bytes: dict[Path, int] = {}
    assert parent_directories

    def walk(directory: Path) -> int:
        assert directory.exists()
        own_size = 0
        subdirs: list[Path] = []
        try:
            with os.scandir(directory) as entries:
                for entry in entries:
                    # is_dir/is_file use cached d_type from readdir, no extra syscall
                    if entry.is_dir(follow_symlinks=False):
                        subdirs.append(Path(entry.path).absolute())

                    else:
                        # entry.stat caches its result; only one stat per file
                        own_size += entry.stat(follow_symlinks=False).st_size

        except (PermissionError, FileNotFoundError):
            pass

        total = own_size
        for subdir in subdirs:
            total += walk(subdir)

        directory_size_bytes[directory.absolute()] = total

        return total

    for root in parent_directories:
        walk(root.resolve())

    return directory_size_bytes


@beartype
class SizeShareDelegate(QStyledItemDelegate):

    def paint(self, painter: QPainter, option: QStyleOptionViewItem,
              index: QModelIndex) -> None:
        share = cast(Optional[float], index.data(Qt.ItemDataRole.UserRole))
        if share is None:
            super().paint(painter, option, index)
            return

        progress = int(round(share * 1000))
        if progress < 0:
            progress = 0
        if 1000 < progress:
            progress = 1000

        progress_option = QStyleOptionProgressBar()
        progress_option.rect = option.rect
        progress_option.minimum = 0
        progress_option.maximum = 1000
        progress_option.progress = progress
        progress_option.text = f"{share * 100:.1f}%"
        progress_option.textVisible = True
        progress_option.textAlignment = Qt.AlignmentFlag.AlignCenter

        QApplication.style().drawControl(QStyle.ControlElement.CE_ProgressBar,
                                         progress_option, painter)


@beartype
class SizeShareColumnSpec(FileTreeColumnSpec):
    column_name = "size_share"
    column_type = SizeShareData

    def __init__(
        self,
        name: str,
        parent_directories: list[Path],
        share_mode: Literal["global", "parent"] = "parent",
    ) -> None:
        super().__init__(name)
        self.share_mode = share_mode
        self.parent_roots = [path.resolve() for path in parent_directories]
        self.directory_size_bytes = _compute_directory_size_bytes(self.parent_roots)
        self.global_size_bytes = sum(
            self.directory_size_bytes[root] for root in self.parent_roots)
        self.delegate = SizeShareDelegate()

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        resolved_path = path.resolve()

        def get_self_size() -> int | None:
            if is_directory:
                return self.directory_size_bytes.get(resolved_path, None)

            else:
                if FileStatsIndexer.asset_name in assets:
                    result = cast(FileStatsIndexerResult,
                                  assets[FileStatsIndexer.asset_name])
                    return result.size_bytes

                else:
                    return None

        def get_parent() -> int:
            if self.share_mode == "global":
                return self.global_size_bytes

            else:
                return self.directory_size_bytes[resolved_path.parent]

        self_size = get_self_size()
        if not self_size:
            log.warning(f"no size for {path}")
            return None

        if resolved_path.parent not in self.directory_size_bytes:
            log.warning(
                f"Missing precomputed size for parent directory: {resolved_path.parent}")
            return None

        return SizeShareData(size_self=self_size, size_parent=get_parent())

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        entry = cast(Optional[SizeShareData], self.getColumnData(index))
        if entry is None:
            return None

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return f"{entry.size_parent * 100:.1f}%"
            case Qt.ItemDataRole.UserRole:
                return float(entry.size_self) / float(entry.size_parent)
            case _:
                return None

    def getDelegate(self) -> QAbstractItemDelegate | None:
        return self.delegate

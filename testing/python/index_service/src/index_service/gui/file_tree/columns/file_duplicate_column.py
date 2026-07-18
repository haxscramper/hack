from pathlib import Path

from PyQt6.QtCore import QModelIndex, Qt
from beartype import beartype
from beartype.typing import Any, Optional, cast
from pydantic import BaseModel

from index_service.gui.file_tree.columns.file_tree_column import (
    FileTreeColumnSpec,
    FileTreeNode,
)
from index_service.services.core.types import FileHash


class FileDuplicateData(BaseModel, extra="forbid"):
    hash: Optional[str] = None
    matches: Optional[list[Path]] = None
    duplicate_count: int = 0
    total_count: int = 0


@beartype
class FileDuplicateColumnSpec(FileTreeColumnSpec):
    column_name = "file_duplicate"
    column_type = FileDuplicateData

    def __init__(self, reference_tree: Optional[FileTreeNode]) -> None:
        super().__init__("Duplicate")

        self._paths_by_hash: dict[str, list[Path]] = {}
        self._match_cache: dict[tuple[Path, str], list[Path]] = {}

        if reference_tree is None:
            return

        pending = [reference_tree]

        while pending:
            node = pending.pop()
            pending.extend(node.nested)

            duplicate_data = cast(
                Optional[FileDuplicateData],
                node.columns.get(self.column_name),
            )

            if duplicate_data is None or duplicate_data.hash is None:
                continue

            self._paths_by_hash.setdefault(
                duplicate_data.hash,
                [],
            ).append(node.path)

        for paths in self._paths_by_hash.values():
            paths.sort(key=str)

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        if is_directory:
            duplicate_count = 0
            total_count = 0

            for node in nested:
                duplicate_data = cast(
                    Optional[FileDuplicateData],
                    node.columns.get(self.column_name),
                )

                if duplicate_data is None:
                    continue

                duplicate_count += duplicate_data.duplicate_count
                total_count += duplicate_data.total_count

            return FileDuplicateData(
                duplicate_count=duplicate_count,
                total_count=total_count,
            )

        if hash is None:
            return None

        matches = self._matches(path, hash.hash)

        return FileDuplicateData(
            hash=hash.hash,
            matches=matches,
            duplicate_count=1 if matches else 0,
            total_count=1,
        )

    def _matches(
        self,
        path: Path,
        file_hash: str,
    ) -> list[Path]:
        cache_key = (path, file_hash)
        cached = self._match_cache.get(cache_key)

        if cached is not None:
            return cached

        matches = sorted(
            {
                candidate for candidate in self._paths_by_hash.get(file_hash, [])
                if candidate != path
            },
            key=str,
        )

        self._match_cache[cache_key] = matches
        return matches

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        duplicate_data = cast(
            Optional[FileDuplicateData],
            self.getColumnData(index),
        )

        if duplicate_data is None:
            return None

        if duplicate_data.hash is None:
            if role in (
                    Qt.ItemDataRole.DisplayRole,
                    Qt.ItemDataRole.EditRole,
            ):
                return (f"{duplicate_data.duplicate_count}"
                        f"/{duplicate_data.total_count}")

            return None

        if role in (
                Qt.ItemDataRole.DisplayRole,
                Qt.ItemDataRole.EditRole,
        ):
            if not duplicate_data.matches:
                return None

            return str(duplicate_data.matches[0])

        if (role == Qt.ItemDataRole.ToolTipRole and duplicate_data.matches):
            return "\n".join(str(path) for path in duplicate_data.matches)

        return None

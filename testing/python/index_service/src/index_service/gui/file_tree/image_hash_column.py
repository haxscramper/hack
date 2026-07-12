import heapq
from pathlib import Path
from typing import Optional

from beartype.typing import Any, cast

import pybktree
from PyQt6.QtCore import QModelIndex, Qt
from PyQt6.QtGui import QBrush, QColor
from beartype import beartype
from pydantic import BaseModel

from index_service.gui.file_tree.base_tree_model import FileTreeNode
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
from index_service.services.core.types import FileHash
from index_service.services.indexers.image_hash import ImageHashIndexerResult, ImageHashIndexer


class ImageHashData(BaseModel, extra="forbid"):
    hash: Optional[str] = None
    matches: Optional[list[tuple[int, Path]]] = None
    duplicate_count: int = 0
    total_count: int = 0
    foreground: Optional[tuple[int, int, int]] = None


@beartype
class ImageHashColumnSpec(FileTreeColumnSpec):
    maximum_distance = 5
    maximum_tooltip_matches = 5
    column_name = "image_hash"
    column_type = ImageHashData

    def __init__(self, reference_tree: Optional[FileTreeNode]) -> None:
        super().__init__("Similar image")

        self._paths_by_hash: dict[int, list[Path]] = {}
        self._match_cache: dict[int, list[tuple[int, Path]]] = {}

        if not reference_tree:
            self._index = pybktree.BKTree(
                self._hamming_distance,
                self._paths_by_hash,
            )
            return

        pending = [reference_tree]

        while pending:
            node = pending.pop()
            pending.extend(node.nested)

            image_hash = node.columns.get(ImageHashColumnSpec.column_name)
            if image_hash is None:
                continue

            assert isinstance(image_hash, ImageHashData), type(image_hash)

            if image_hash.hash is None:
                continue

            perceptual_hash = self._parse_hash(image_hash.hash)
            self._paths_by_hash.setdefault(perceptual_hash, []).append(node.path)

        for paths in self._paths_by_hash.values():
            paths.sort(key=str)

        self._index = pybktree.BKTree(
            self._hamming_distance,
            self._paths_by_hash,
        )

    def initColumnData(
        self,
        path: Path,
        hash: Optional[FileHash],
        is_directory: bool,
        assets: dict[str, BaseModel],
        nested: list[FileTreeNode],
    ) -> Optional[BaseModel]:
        if is_directory:
            nested_data = (cast(Optional[ImageHashData],
                                node.columns.get(self.column_name)) for node in nested)

            duplicate_count = 0
            total_count = 0

            for image_hash in nested_data:
                if image_hash is None:
                    continue

                duplicate_count += image_hash.duplicate_count
                total_count += image_hash.total_count

            return ImageHashData(
                duplicate_count=duplicate_count,
                total_count=total_count,
            )

        result = cast(
            Optional[ImageHashIndexerResult],
            assets.get(ImageHashIndexer.asset_name),
        )

        if result is None or not result.perceptual:
            return None

        matches = self._matches(result.perceptual)
        closest_distance = matches[0][0] if matches else None

        return ImageHashData(
            hash=result.perceptual,
            matches=matches,
            duplicate_count=1 if matches else 0,
            total_count=1,
            foreground=(self._foreground(closest_distance)
                        if closest_distance is not None else None),
        )

    @staticmethod
    def _parse_hash(value: str) -> int:
        return int(value.replace("-", ""), 16)

    @staticmethod
    def _hamming_distance(left: int, right: int) -> int:
        return (left ^ right).bit_count()

    def _matches(self, image_hash: str) -> list[tuple[int, Path]]:
        perceptual_hash = self._parse_hash(image_hash)
        cached = self._match_cache.get(perceptual_hash)

        if cached is not None:
            return cached

        hash_matches = self._index.find(
            perceptual_hash,
            self.maximum_distance,
        )

        matches = heapq.nsmallest(
            self.maximum_tooltip_matches,
            ((distance, path)
             for distance, matched_hash in hash_matches
             for path in self._paths_by_hash[matched_hash]),
            key=lambda match: (match[0], str(match[1])),
        )

        self._match_cache[perceptual_hash] = matches
        return matches

    @staticmethod
    def _foreground(distance: int) -> tuple[int, int, int]:
        if distance == 0:
            return (220, 70, 70)

        else:
            return (220, 160, 50)

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        image_hash = cast(
            Optional[ImageHashData],
            self.getColumnData(index),
        )

        if image_hash is None:
            return None

        if image_hash.hash is None:
            if role in (
                    Qt.ItemDataRole.DisplayRole,
                    Qt.ItemDataRole.EditRole,
            ):
                return f"{image_hash.duplicate_count}/{image_hash.total_count}"

            return None

        matches = image_hash.matches
        if not matches:
            return None

        _, closest_path = matches[0]

        match role:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole:
                return str(closest_path)

            case Qt.ItemDataRole.ToolTipRole:
                return "\n".join(str(path) for _, path in matches)

            case Qt.ItemDataRole.ForegroundRole:
                if image_hash.foreground:
                    return QBrush(QColor(*list(image_hash.foreground)))

                else:
                    return None

            case _:
                return None

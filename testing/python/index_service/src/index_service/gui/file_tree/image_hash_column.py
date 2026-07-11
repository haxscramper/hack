import heapq
from pathlib import Path
from beartype.typing import Any

import pybktree
from PySide6.QtCore import QModelIndex
from PySide6.QtGui import QBrush, QColor, Qt
from beartype import beartype

from index_service.gui.file_tree.base_tree_model import FileTreeNode
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
from index_service.services.indexers.image_hash import ImageHashIndexerResult


@beartype
class ImageHashColumnSpec(FileTreeColumnSpec):
    maximum_distance = 5
    maximum_tooltip_matches = 5

    def __init__(self, reference_tree: FileTreeNode) -> None:
        super().__init__("Similar image")

        self._paths_by_hash: dict[int, list[Path]] = {}
        self._match_cache: dict[int, list[tuple[int, Path]]] = {}

        pending = [reference_tree]

        while pending:
            node = pending.pop()
            pending.extend(node.nested)

            image_hash = node.assets.get("image_hash", None)
            if image_hash is None:
                continue

            assert isinstance(image_hash, ImageHashIndexerResult), type(image_hash)

            perceptual_hash = self._parse_hash(image_hash.perceptual)
            self._paths_by_hash.setdefault(perceptual_hash, []).append(node.path)

        for paths in self._paths_by_hash.values():
            paths.sort(key=str)

        self._index = pybktree.BKTree(
            self._hamming_distance,
            self._paths_by_hash,
        )

    @staticmethod
    def _parse_hash(value: str) -> int:
        return int(value.replace("-", ""), 16)

    @staticmethod
    def _hamming_distance(left: int, right: int) -> int:
        return (left ^ right).bit_count()

    def _matches(self, node: FileTreeNode) -> list[tuple[int, Path]]:
        image_hash = node.assets.get("image_hash")

        if not isinstance(image_hash, ImageHashIndexerResult):
            return []

        perceptual_hash = self._parse_hash(image_hash.perceptual)
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
    def _foreground(distance: int) -> QBrush:
        if distance == 0:
            return QBrush(QColor(220, 70, 70))

        else:
            return QBrush(QColor(220, 160, 50))

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        matches = self._matches(self.node(index))

        if not matches:
            return None

        closest_distance, closest_path = matches[0]

        match role:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole:
                return str(closest_path)

            case Qt.ItemDataRole.ToolTipRole:
                return "\n".join(str(path) for _, path in matches)

            case Qt.ItemDataRole.ForegroundRole:
                return self._foreground(closest_distance)

            case _:
                return None

from enum import Enum, auto
from dataclasses import dataclass
import logging
from beartype.typing import Optional
from pathlib import Path
from sqlalchemy.orm import Session
from sqlalchemy import select
from math import sqrt
from .models import (ImageEntry, ImageProbabilisticTag)


class SortMode(Enum):
    NAME_ASC = auto()
    NAME_DESC = auto()
    SIZE_ASC = auto()
    SIZE_DESC = auto()
    MTIME_ASC = auto()
    MTIME_DESC = auto()
    SIMILARITY = auto()
    SIMILARITY_TO_REFERENCE = auto()


@dataclass(slots=True)
class SparseImageVector:
    values: dict[int, float]
    norm: float


class SimilarityIndex:

    def __init__(self, root_path: Path) -> None:
        self.root_path = root_path.resolve()
        self.relative_path_to_image_id: dict[str, int] = {}
        self.image_id_to_vector: dict[int, SparseImageVector] = {}

    @staticmethod
    def _compute_norm(values: dict[int, float]) -> float:
        return sqrt(sum(value * value for value in values.values()))

    def build(self, session: Session) -> None:
        self.relative_path_to_image_id.clear()
        self.image_id_to_vector.clear()

        for image_id, relative_path in session.execute(
                select(ImageEntry.id, ImageEntry.relative_path)):
            if relative_path is not None:
                self.relative_path_to_image_id[relative_path] = image_id

        rows = session.execute(
            select(
                ImageProbabilisticTag.image_id,
                ImageProbabilisticTag.tag_id,
                ImageProbabilisticTag.probability,
            ).order_by(
                ImageProbabilisticTag.image_id,
                ImageProbabilisticTag.tag_id,
            ))

        current_image_id: int | None = None
        current_values: dict[int, float] = {}

        for image_id, tag_id, probability in rows:
            if current_image_id is None:
                current_image_id = image_id

            if image_id != current_image_id:
                self.image_id_to_vector[current_image_id] = SparseImageVector(
                    values=current_values,
                    norm=self._compute_norm(current_values),
                )
                current_image_id = image_id
                current_values = {}

            current_values[tag_id] = probability

        if current_image_id is not None:
            self.image_id_to_vector[current_image_id] = SparseImageVector(
                values=current_values,
                norm=self._compute_norm(current_values),
            )

        logging.info(f"constructed {len(self.image_id_to_vector)} ids")

    def get_vector_for_path(self, path: Path) -> SparseImageVector | None:
        relative_path = str(path.resolve().relative_to(self.root_path))
        image_id = self.relative_path_to_image_id.get(relative_path)
        if image_id is None:
            return None
        return self.image_id_to_vector.get(image_id)


def cosine_similarity(left: SparseImageVector,
                      right: SparseImageVector) -> float:
    if left.norm == 0.0 or right.norm == 0.0:
        return 0.0

    if len(left.values) > len(right.values):
        left, right = right, left

    dot = 0.0
    for tag_id, left_value in left.values.items():
        right_value = right.values.get(tag_id)
        if right_value is not None:
            dot += left_value * right_value

    return dot / (left.norm * right.norm)


def _path_name_key(path: Path) -> str:
    return path.name.lower()


def _path_size_key(path: Path) -> tuple[int, str]:
    return (path.stat().st_size, path.name.lower())


def _path_mtime_key(path: Path) -> tuple[float, str]:
    return (path.stat().st_mtime, path.name.lower())


def order_paths_by_similarity(
    paths: list[Path],
    similarity_index: SimilarityIndex,
) -> list[Path]:
    with_vectors: list[tuple[Path, SparseImageVector]] = []
    without_vectors: list[Path] = []

    for path in paths:
        vector = similarity_index.get_vector_for_path(path)
        if vector is None:
            without_vectors.append(path)
        else:
            with_vectors.append((path, vector))

    if not with_vectors:
        return sorted(paths, key=_path_name_key)

    remaining = sorted(with_vectors, key=lambda item: item[0].name.lower())
    ordered: list[Path] = []

    current_path, current_vector = remaining.pop(0)
    ordered.append(current_path)

    while remaining:
        best_index = 0
        best_score = -1.0

        for idx, (_, candidate_vector) in enumerate(remaining):
            score = cosine_similarity(current_vector, candidate_vector)
            if score > best_score:
                best_score = score
                best_index = idx

        current_path, current_vector = remaining.pop(best_index)
        ordered.append(current_path)

    ordered.extend(sorted(without_vectors, key=_path_name_key))
    return ordered


def order_paths_by_similarity_to_reference(
    paths: list[Path],
    similarity_index: SimilarityIndex,
    reference_path: Path,
) -> list[Path]:
    reference_vector = similarity_index.get_vector_for_path(reference_path)
    if reference_vector is None:
        return sorted(paths, key=_path_name_key)

    scored: list[tuple[float, Path]] = []
    unscored: list[Path] = []

    for path in paths:
        vector = similarity_index.get_vector_for_path(path)
        if vector is None:
            unscored.append(path)
            continue

        score = cosine_similarity(reference_vector, vector)
        scored.append((score, path))

    scored.sort(key=lambda item: (-item[0], item[1].name.lower()))
    unscored.sort(key=_path_name_key)
    return [path for _, path in scored] + unscored


def sort_paths(
    paths: list[Path],
    sort_mode: SortMode,
    similarity_index: SimilarityIndex | None = None,
    reference_path: Path | None = None,
) -> list[Path]:
    if sort_mode == SortMode.NAME_ASC:
        return sorted(paths, key=_path_name_key)

    if sort_mode == SortMode.NAME_DESC:
        return sorted(paths, key=_path_name_key, reverse=True)

    if sort_mode == SortMode.SIZE_ASC:
        return sorted(paths, key=_path_size_key)

    if sort_mode == SortMode.SIZE_DESC:
        return sorted(paths, key=_path_size_key, reverse=True)

    if sort_mode == SortMode.MTIME_ASC:
        return sorted(paths, key=_path_mtime_key)

    if sort_mode == SortMode.MTIME_DESC:
        return sorted(paths, key=_path_mtime_key, reverse=True)

    if sort_mode == SortMode.SIMILARITY:
        assert similarity_index is not None
        return order_paths_by_similarity(paths, similarity_index)

    if sort_mode == SortMode.SIMILARITY_TO_REFERENCE:
        assert similarity_index is not None
        assert reference_path is not None
        return order_paths_by_similarity_to_reference(
            paths=paths,
            similarity_index=similarity_index,
            reference_path=reference_path,
        )

    raise ValueError(sort_mode)

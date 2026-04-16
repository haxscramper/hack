from __future__ import annotations

import hashlib
from pathlib import Path
from beartype.typing import Iterable
from beartype import beartype

from sqlalchemy import select
from sqlalchemy.orm import Session

from .models import (
    ImageEntry,
    ProbabilisticTag,
    ImageProbabilisticTag,
    RegularTag,
    ImageRegularTag,
    ImageDescription,
)


def get_md5(file_path: Path) -> str:
    h = hashlib.md5()
    with file_path.open("rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


class Repository:
    def __init__(self, session: Session):
        self.session = session

    def upsert_image(self, root_dir: Path, file_path: Path) -> ImageEntry:
        md5_digest = get_md5(file_path)
        relative_path = str(file_path.resolve().relative_to(root_dir.resolve()))
        entry = self.session.scalar(
            select(ImageEntry).where(ImageEntry.md5_digest == md5_digest)
        )
        if entry is None:
            entry = ImageEntry(
                relative_path=relative_path,
                original_name=file_path.name,
                md5_digest=md5_digest,
            )
            self.session.add(entry)
            self.session.commit()
            self.session.refresh(entry)
        else:
            if entry.relative_path != relative_path:
                entry.relative_path = relative_path
                entry.original_name = file_path.name
                self.session.commit()
                self.session.refresh(entry)
        return entry

    def get_image_by_path(self, relative_path: str) -> ImageEntry | None:
        return self.session.scalar(
            select(ImageEntry).where(ImageEntry.relative_path == relative_path)
        )

    def get_fully_annotated_paths(self) -> set[str]:
        stmt = (
            select(ImageEntry.relative_path)
            .join(
                ImageProbabilisticTag, ImageEntry.id == ImageProbabilisticTag.image_id
            )
            .join(ImageRegularTag, ImageEntry.id == ImageRegularTag.image_id)
            .join(ImageDescription, ImageEntry.id == ImageDescription.image_id)
            .where(ImageDescription.description != "")
            .where(ImageDescription.description.is_not(None))
            .distinct()
        )
        return set(self.session.scalars(stmt).all())

    def list_probabilistic_tags(self, image_id: int):
        rows = self.session.execute(
            select(ImageProbabilisticTag, ProbabilisticTag)
            .join(ProbabilisticTag, ImageProbabilisticTag.tag_id == ProbabilisticTag.id)
            .where(ImageProbabilisticTag.image_id == image_id)
            .order_by(ImageProbabilisticTag.probability.desc())
        ).all()
        return rows

    def list_regular_tags(self, image_id: int):
        rows = self.session.execute(
            select(ImageRegularTag, RegularTag)
            .join(RegularTag, ImageRegularTag.tag_id == RegularTag.id)
            .where(ImageRegularTag.image_id == image_id)
            .order_by(RegularTag.category, RegularTag.name)
        ).all()
        return rows

    def get_description(self, image_id: int) -> ImageDescription | None:
        return self.session.scalar(
            select(ImageDescription).where(ImageDescription.image_id == image_id)
        )

    def get_or_create_probabilistic_tag(
        self,
        name: str,
        category: str = "user",
        external_tag_id: int | None = None,
    ) -> ProbabilisticTag:
        tag = self.session.scalar(
            select(ProbabilisticTag).where(ProbabilisticTag.name == name)
        )
        if tag is None:
            tag = ProbabilisticTag(
                name=name,
                category=category,
                external_tag_id=external_tag_id,
            )
            self.session.add(tag)
            self.session.commit()
            self.session.refresh(tag)
        return tag

    def set_probabilistic_tag(
        self, image_id: int, tag_name: str, probability: float, category: str = "user"
    ):
        tag = self.get_or_create_probabilistic_tag(tag_name, category=category)
        rel = self.session.scalar(
            select(ImageProbabilisticTag)
            .where(ImageProbabilisticTag.image_id == image_id)
            .where(ImageProbabilisticTag.tag_id == tag.id)
        )
        if rel is None:
            rel = ImageProbabilisticTag(
                image_id=image_id,
                tag_id=tag.id,
                probability=probability,
            )
            self.session.add(rel)
        else:
            rel.probability = probability
        self.session.commit()

    def delete_probabilistic_tag(self, image_id: int, tag_name: str):
        tag = self.session.scalar(
            select(ProbabilisticTag).where(ProbabilisticTag.name == tag_name)
        )
        if not tag:
            return
        rel = self.session.scalar(
            select(ImageProbabilisticTag)
            .where(ImageProbabilisticTag.image_id == image_id)
            .where(ImageProbabilisticTag.tag_id == tag.id)
        )
        if rel:
            self.session.delete(rel)
            self.session.commit()

    def get_or_create_regular_tag(self, category: str, name: str) -> RegularTag:
        tag = self.session.scalar(
            select(RegularTag)
            .where(RegularTag.category == category)
            .where(RegularTag.name == name)
        )
        if tag is None:
            tag = RegularTag(category=category, name=name)
            self.session.add(tag)
            self.session.commit()
            self.session.refresh(tag)
        return tag

    def add_regular_tag(self, image_id: int, category: str, name: str):
        tag = self.get_or_create_regular_tag(category, name)
        rel = self.session.scalar(
            select(ImageRegularTag)
            .where(ImageRegularTag.image_id == image_id)
            .where(ImageRegularTag.tag_id == tag.id)
        )
        if rel is None:
            self.session.add(ImageRegularTag(image_id=image_id, tag_id=tag.id))
            self.session.commit()

    def delete_regular_tag(self, image_id: int, category: str, name: str):
        tag = self.session.scalar(
            select(RegularTag)
            .where(RegularTag.category == category)
            .where(RegularTag.name == name)
        )
        if not tag:
            return
        rel = self.session.scalar(
            select(ImageRegularTag)
            .where(ImageRegularTag.image_id == image_id)
            .where(ImageRegularTag.tag_id == tag.id)
        )
        if rel:
            self.session.delete(rel)
            self.session.commit()

    def set_description(
        self, image_id: int, description: str, model_name: str | None = None
    ):
        row = self.session.scalar(
            select(ImageDescription).where(ImageDescription.image_id == image_id)
        )
        if row is None:
            row = ImageDescription(
                image_id=image_id,
                description=description,
                model_name=model_name,
            )
            self.session.add(row)
        else:
            row.description = description
            row.model_name = model_name
        self.session.commit()

    def replace_probabilistic_annotations(
        self,
        image_id: int,
        items: Iterable[tuple[str, str, float]],
    ):
        self.session.query(ImageProbabilisticTag).filter_by(image_id=image_id).delete()
        self.session.commit()

        seen = set()
        for category, name, probability in items:
            if (category, name) in seen:
                continue
            seen.add((category, name))
            tag = self.get_or_create_probabilistic_tag(name=name, category=category)
            self.session.add(
                ImageProbabilisticTag(
                    image_id=image_id,
                    tag_id=tag.id,
                    probability=probability,
                )
            )
        self.session.commit()

    def replace_regular_annotations(
        self,
        image_id: int,
        items: Iterable[tuple[str, str]],
    ):
        self.session.query(ImageRegularTag).filter_by(image_id=image_id).delete()
        self.session.commit()

        seen = set()
        for category, name in items:
            if (category, name) in seen:
                continue
            seen.add((category, name))
            tag = self.get_or_create_regular_tag(category=category, name=name)
            self.session.add(ImageRegularTag(image_id=image_id, tag_id=tag.id))
        self.session.commit()

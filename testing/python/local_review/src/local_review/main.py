#!/usr/bin/env python

import argparse
import sys
from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from PyQt6.QtWidgets import QApplication

from local_review.annotation_store import AnnotationStore
from local_review.models import ReviewDocument
from local_review.review_repository import GitReviewRepository
from local_review.window import ReviewWindow


@dataclass(frozen=True)
class Arguments:
    repository: Path
    annotations: Path
    base: str | None


@beartype
def parse_arguments() -> Arguments:
    parser = argparse.ArgumentParser(
        description="Review local Git changes with editable annotations")
    parser.add_argument(
        "repository",
        type=Path,
        help="Path inside the Git repository",
    )
    parser.add_argument(
        "annotations",
        type=Path,
        help="Path to the annotation JSON document",
    )
    parser.add_argument(
        "--base",
        help="Base revision, defaulting to master for new reviews",
    )
    namespace = parser.parse_args()
    return Arguments(
        repository=namespace.repository.resolve(),
        annotations=namespace.annotations.resolve(),
        base=namespace.base,
    )


@beartype
def load_review(
    arguments: Arguments, ) -> tuple[GitReviewRepository, AnnotationStore]:
    if arguments.annotations.exists():
        document = ReviewDocument.model_validate_json(
            arguments.annotations.read_text(encoding="utf-8"))
        if arguments.base is not None and arguments.base != document.base_revision:
            raise ValueError(
                f"Annotation document uses base revision "
                f"{document.base_revision!r}, but --base specified "
                f"{arguments.base!r}")

        repository = GitReviewRepository(
            arguments.repository,
            document.base_revision,
            document.base_commit_oid,
        )
        expected_path = str(repository.worktree_path)
        if Path(document.repository_path).resolve() != Path(expected_path):
            raise ValueError(
                f"Annotation document belongs to repository "
                f"{document.repository_path!r}, not {expected_path!r}")
    else:
        base_revision = arguments.base or "master"
        repository = GitReviewRepository(
            arguments.repository,
            base_revision,
            None,
        )
        document = ReviewDocument(
            repository_path=str(repository.worktree_path),
            base_revision=base_revision,
            base_commit_oid=str(repository.base_commit_oid),
        )

    store = AnnotationStore(arguments.annotations, document)
    store.save()
    return repository, store


@beartype
def main() -> int:
    arguments = parse_arguments()
    repository, store = load_review(arguments)
    application = QApplication(sys.argv)
    window = ReviewWindow(repository, store)
    window.show()
    return application.exec()


if __name__ == "__main__":
    sys.exit(main())

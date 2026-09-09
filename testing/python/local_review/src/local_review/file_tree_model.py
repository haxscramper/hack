from collections.abc import Sequence
from pathlib import Path

from beartype import beartype
from PyQt6.QtCore import Qt
from PyQt6.QtGui import (
    QStandardItem,
    QStandardItemModel,
)

from local_review.models import AnnotationThread, DiffFile


@beartype
class FileTreeModel(QStandardItemModel):
    PATH_ROLE = int(Qt.ItemDataRole.UserRole) + 1

    def __init__(self) -> None:
        super().__init__()
        self.files_by_path: dict[str, DiffFile] = {}
        self.file_annotation_items: dict[str, QStandardItem] = {}
        self.directory_annotation_items: dict[str, QStandardItem] = {}
        self.setHorizontalHeaderLabels(["File", "+", "-"])

    def replace_files(
        self,
        files: Sequence[DiffFile],
        threads: Sequence[AnnotationThread],
    ) -> None:
        self.clear()
        self.setHorizontalHeaderLabels(["File", "+", "-", "Annotations"])
        self.files_by_path = {
            diff_file.display_path: diff_file
            for diff_file in files
        }
        self.file_annotation_items.clear()
        self.directory_annotation_items.clear()

        root = self.invisibleRootItem()
        directories: dict[
            str,
            tuple[
                QStandardItem,
                QStandardItem,
                QStandardItem,
                QStandardItem,
            ],
        ] = {}

        for diff_file in files:
            parts = Path(diff_file.display_path).parts
            parent = root
            accumulated: list[str] = []

            for part in parts[:-1]:
                accumulated.append(part)
                directory_path = "/".join(accumulated)
                existing = directories.get(directory_path)
                if existing is None:
                    name_item = QStandardItem(part)
                    additions_item = QStandardItem("0")
                    deletions_item = QStandardItem("0")
                    annotation_item = QStandardItem("0")

                    for item in (
                            name_item,
                            additions_item,
                            deletions_item,
                            annotation_item,
                    ):
                        item.setEditable(False)

                    parent.appendRow([
                        name_item,
                        additions_item,
                        deletions_item,
                        annotation_item,
                    ])
                    existing = (
                        name_item,
                        additions_item,
                        deletions_item,
                        annotation_item,
                    )
                    directories[directory_path] = existing
                    self.directory_annotation_items[
                        directory_path] = annotation_item

                additions_item = existing[1]
                deletions_item = existing[2]
                additions_item.setText(
                    str(int(additions_item.text()) + diff_file.additions))
                deletions_item.setText(
                    str(int(deletions_item.text()) + diff_file.deletions))
                parent = existing[0]

            name_item = QStandardItem(parts[-1])
            additions_item = QStandardItem(str(diff_file.additions))
            deletions_item = QStandardItem(str(diff_file.deletions))
            annotation_item = QStandardItem("0")
            name_item.setData(
                diff_file.display_path,
                self.PATH_ROLE,
            )

            for item in (
                    name_item,
                    additions_item,
                    deletions_item,
                    annotation_item,
            ):
                item.setEditable(False)

            parent.appendRow([
                name_item,
                additions_item,
                deletions_item,
                annotation_item,
            ])
            self.file_annotation_items[
                diff_file.display_path] = annotation_item

        self.update_annotation_counts(threads)

    def update_annotation_counts(
        self,
        threads: Sequence[AnnotationThread],
    ) -> None:
        for item in self.file_annotation_items.values():
            item.setText("0")

        for item in self.directory_annotation_items.values():
            item.setText("0")

        for diff_file in self.files_by_path.values():
            paths = {
                diff_file.old_path,
                diff_file.new_path,
            }
            count = sum(1 for thread in threads if thread.anchor.path in paths)
            self.file_annotation_items[diff_file.display_path].setText(
                str(count))

            accumulated: list[str] = []
            for part in Path(diff_file.display_path).parts[:-1]:
                accumulated.append(part)
                directory_path = "/".join(accumulated)
                item = self.directory_annotation_items[directory_path]
                item.setText(str(int(item.text()) + count))

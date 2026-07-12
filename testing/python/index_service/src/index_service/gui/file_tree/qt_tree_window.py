from pathlib import Path

from PySide6.QtCore import QCoreApplication, QSettings
from PySide6.QtGui import QCloseEvent
from beartype import beartype
from beartype.typing import Sequence

from PySide6.QtWidgets import QMainWindow, QTreeView, QWidget
from networkx.algorithms.flow import build_flow_dict

from index_service.cli.cli_config import FileTreeViewConfig
from index_service.gui.common.qt_utils import print_model_tree
from index_service.gui.file_tree.base_tree_model import build_file_tree
from index_service.gui.file_tree.column_model import ColumnSpec
from index_service.gui.file_tree.file_duplicate_column import FileDuplicateColumnSpec
from index_service.gui.file_tree.file_hash_column import FileHashColumnSpec
from index_service.gui.file_tree.file_name_column import FileNameColumnSpec
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
from index_service.gui.file_tree.image_hash_column import ImageHashColumnSpec
from index_service.gui.file_tree.qt_tree_model import FileTreeModel
from index_service.gui.file_tree.wd_tags_column import WdTagsColumnSpec
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
import logging

log = logging.getLogger(__name__)


@beartype
class FileTreeQueryWindow(QMainWindow):

    def __init__(
        self,
        ctx: RunContext,
        file_tree_view: FileTreeViewConfig,
        db: IndexDatabase,
        indexer_instances: Sequence[BaseIndexer],
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)

        assert file_tree_view

        QCoreApplication.setOrganizationName("haxscramper")
        QCoreApplication.setApplicationName("haxdex-tree-view")

        columns: list[FileTreeColumnSpec] = [
            FileNameColumnSpec(),
        ]

        if file_tree_view.reference_dir:
            reference_tree = build_file_tree(
                ctx=ctx,
                db=db,
                root_directories=[Path(file_tree_view.reference_dir)],
                indexers=indexer_instances,
                columns=columns + [
                    ImageHashColumnSpec(None),
                    FileDuplicateColumnSpec(None),
                ],
            )

            columns.append(ImageHashColumnSpec(reference_tree=reference_tree[0]))
            columns.append(FileDuplicateColumnSpec(reference_tree=reference_tree[0]))

        nodes = build_file_tree(
            ctx=ctx,
            db=db,
            root_directories=[
                Path(path).expanduser().absolute() for path in file_tree_view.root_dirs
            ],
            indexers=indexer_instances,
            columns=columns,
        )

        self.resize(1200, 800)

        self.model = FileTreeModel(nodes=nodes, parent=self, columns=columns)
        self.tree_view = QTreeView(self)
        self.tree_view.setIndentation(20)
        self.tree_view.setModel(self.model)
        self.model.configureView(self.tree_view)
        self.setCentralWidget(self.tree_view)

        self.restore_ui_state()

    def restore_ui_state(self) -> None:
        settings = QSettings()

        geometry = settings.value("window/geometry")
        if geometry is not None:
            self.restoreGeometry(geometry)

        window_state = settings.value("window/state")
        if window_state is not None:
            self.restoreState(window_state)

        header_state = settings.value("tree/headerState")
        if header_state is not None:
            self.tree_view.header().restoreState(header_state)

    def save_ui_state(self) -> None:
        settings = QSettings()
        settings.setValue("window/geometry", self.saveGeometry())
        settings.setValue("window/state", self.saveState())
        settings.setValue(
            "tree/headerState",
            self.tree_view.header().saveState(),
        )

    def closeEvent(self, event: QCloseEvent) -> None:
        self.save_ui_state()
        super().closeEvent(event)

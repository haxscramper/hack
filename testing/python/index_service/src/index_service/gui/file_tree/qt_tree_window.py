from pathlib import Path

from beartype import beartype
from beartype.typing import Sequence

from PySide6.QtWidgets import QMainWindow, QTreeView, QWidget

from index_service.cli.cli_config import FileTreeViewConfig
from index_service.gui.file_tree.base_tree_model import build_file_tree
from index_service.gui.file_tree.qt_tree_model import FileTreeModel
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer


@beartype
class FileTreeQueryWindow(QMainWindow):

    def __init__(
        self,
        file_tree_view: FileTreeViewConfig,
        db: IndexDatabase,
        indexer_instances: Sequence[BaseIndexer],
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)

        assert file_tree_view

        nodes = build_file_tree(
            db=db,
            root_directories=[
                Path(path).expanduser().absolute() for path in file_tree_view.root_dirs
            ],
            indexers=indexer_instances,
        )

        self.resize(1200, 800)
        self.model = FileTreeModel(nodes=nodes, parent=self)
        self.tree_view = QTreeView(self)
        self.tree_view.setIndentation(20)
        self.tree_view.expandAll()
        self.tree_view.setModel(self.model)
        self.model.configureView(self.tree_view)
        self.setCentralWidget(self.tree_view)

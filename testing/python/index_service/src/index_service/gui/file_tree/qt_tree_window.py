from PyQt6.QtCore import QCoreApplication, QSettings, Qt, QModelIndex
from PyQt6.QtGui import QCloseEvent
from beartype import beartype
from beartype.typing import Sequence

from PyQt6.QtWidgets import (
    QMainWindow,
    QMessageBox,
    QSplitter,
    QWidget,
)

from index_service.cli.cli_config import FileTreeViewConfig
from index_service.gui.abstract_models.column_model import AbstractColumnItemModel
from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.preview_pane import FilePreviewPane
from index_service.gui.common.qt_model_roles import CustomModelRole
from index_service.gui.common.qt_utils import get_settings
from index_service.gui.file_tree.actions.action_list_view import ActionListView
from index_service.gui.file_tree.base_tree_model import build_file_tree, FileTreeNode
from index_service.gui.file_tree.columns.file_duplicate_column import FileDuplicateColumnSpec
from index_service.gui.file_tree.columns.file_name_column import FileNameColumnSpec
from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec
from index_service.gui.file_tree.python_code_editor import QueryError
from index_service.gui.file_tree.qt_tree_model import FileTreeModel
from index_service.gui.file_tree.qt_tree_region import FileTreeRegion
from index_service.gui.file_tree.query_filter import ActionListModel
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
import logging
from pathlib import Path

from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)


@beartype
class FileTreeQueryWindow(QMainWindow):

    def __init__(
        self,
        ctx: RunContext,
        file_tree_view: FileTreeViewConfig,
        db: IndexDatabase,
        indexer_instances: Sequence[BaseIndexer],
        builders: Sequence[WidgetBuilder],
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
            reference_tree: list[FileTreeNode] = build_file_tree(
                ctx=ctx,
                db=db,
                root_directories=[Path(file_tree_view.reference_dir)],
                indexers=indexer_instances,
                columns=columns + [
                    # ImageHashColumnSpec(None),
                    FileDuplicateColumnSpec(None),
                ],
                cache_path=Path("/tmp/reference_tree_cache.sqlite"),
            )

            # columns.append(ImageHashColumnSpec(reference_tree=reference_tree[0]))
            columns.append(FileDuplicateColumnSpec(reference_tree=reference_tree[0]))

        nodes = build_file_tree(
            ctx=ctx,
            db=db,
            root_directories=[
                Path(path).expanduser().absolute() for path in file_tree_view.root_dirs
            ],
            indexers=indexer_instances,
            columns=columns,
            cache_path=Path("/tmp/input_tree_cache.sqlite"),
        )

        model = FileTreeModel(
            columns=columns,
            nodes=nodes,
            parent=self,
        )

        self.resize(1200, 800)

        self.columns = columns
        self.regions: list[FileTreeRegion] = []
        self.region_widgets: list[QWidget] = []

        self.main_splitter = QSplitter(Qt.Orientation.Horizontal, self)
        self.setCentralWidget(self.main_splitter)

        self.region_splitter = QSplitter(
            Qt.Orientation.Horizontal,
            self.main_splitter,
        )

        self.preview_pane = FilePreviewPane(
            db=db,
            collection_names=[t.asset_name for t in indexer_instances],
            builders=builders,
            parent=self.main_splitter,
        )

        self.main_splitter.addWidget(self.region_splitter)
        self.main_splitter.addWidget(self.preview_pane)
        self.main_splitter.setStretchFactor(0, 2)
        self.main_splitter.setStretchFactor(1, 1)

        self._add_region(model)

        self.restore_ui_state()
        first_index = model.first_index_with_hash()
        if first_index.isValid():
            self.preview_pane.show_hash(
                FileHash(hash=first_index.data(CustomModelRole.HashRole.value)))

    def _refresh_named_queries(self) -> None:
        for region in self.regions:
            region.refresh_named_queries()

    def _add_action_region(self, actions: ActionListModel) -> ActionListView:
        log.info("add action list view")
        view = ActionListView(actions, parent=self.region_splitter)
        view.file_hash_activated.connect(self.preview_pane.show_hash)
        self.region_splitter.addWidget(view)
        self.region_widgets.append(view)
        return view

    def _add_region(self, model: AbstractColumnItemModel) -> FileTreeRegion:
        region = FileTreeRegion(
            model=model,
            columns=self.columns,
            parent=self.region_splitter,
            region_id=f"region_{len(self.regions)}",
        )

        region.query_submitted.connect(self._on_query_submitted)
        region.named_queries_changed.connect(self._refresh_named_queries)
        region.file_hash_activated.connect(self.preview_pane.show_hash)

        self.region_splitter.addWidget(region)
        self.regions.append(region)
        self.region_widgets.append(region)

        return region

    def _on_query_submitted(self, source_region: FileTreeRegion) -> None:
        try:
            result = source_region.compute_filtered()
            log.info("compute filtered OK")

        except QueryError as error:
            source_region.query_edit.show_query_error(error)
            return

        except Exception as error:
            log.exception("query failed")
            QMessageBox.warning(self, "Query error", str(error))
            return

        source_index = self.region_widgets.index(source_region)
        while len(self.region_widgets) > source_index + 1:
            stale = self.region_widgets.pop()
            if isinstance(stale, FileTreeRegion):
                self.regions.remove(stale)
            stale.setParent(None)
            stale.deleteLater()

        if isinstance(result, AbstractColumnItemModel):
            self._add_region(result)
        elif isinstance(result, ActionListModel):
            self._add_action_region(result)
        else:
            raise TypeError(f"Unsupported query result model: {type(result)!r}")

    def restore_ui_state(self) -> None:
        settings = get_settings()

        geometry = settings.value("window/geometry")
        if geometry is not None:
            self.restoreGeometry(geometry)

        window_state = settings.value("window/state")
        if window_state is not None:
            self.restoreState(window_state)

        header_state = settings.value("tree/headerState")
        if header_state is not None and self.regions:
            self.regions[0].tree_view.header().restoreState(header_state)

        # Splitters must be restored explicitly
        main_splitter_state = settings.value("splitter/main")
        if main_splitter_state is not None and not self.main_splitter.restoreState(
                main_splitter_state):
            self.main_splitter.setSizes([800, 400])  # fallback 2:1
        elif main_splitter_state is None:
            self.main_splitter.setSizes([800, 400])  # default 2:1 on first run

        region_splitter_state = settings.value("splitter/region")
        if region_splitter_state is not None:
            self.region_splitter.restoreState(region_splitter_state)

    def save_ui_state(self) -> None:
        settings = get_settings()
        settings.setValue("window/geometry", self.saveGeometry())
        settings.setValue("window/state", self.saveState())

        # Save splitters explicitly
        settings.setValue("splitter/main", self.main_splitter.saveState())
        settings.setValue("splitter/region", self.region_splitter.saveState())

        if self.regions:
            settings.setValue(
                "tree/headerState",
                self.regions[0].tree_view.header().saveState(),
            )

    def closeEvent(self, event: QCloseEvent) -> None:
        self.save_ui_state()
        super().closeEvent(event)

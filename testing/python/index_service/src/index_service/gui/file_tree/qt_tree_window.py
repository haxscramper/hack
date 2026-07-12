from pathlib import Path

from PySide6.QtCore import QCoreApplication, QSettings, Qt, Signal
from PySide6.QtGui import QCloseEvent, QFont, QKeySequence, QShortcut
from beartype import beartype
from beartype.typing import Callable, Sequence

from PySide6.QtWidgets import (
    QAbstractItemView,
    QMainWindow,
    QMessageBox,
    QPlainTextEdit,
    QPushButton,
    QSplitter,
    QTreeView,
    QVBoxLayout,
    QWidget,
)

from index_service.cli.cli_config import FileTreeViewConfig
from index_service.gui.file_tree.base_tree_model import build_file_tree, FileTreeNode
from index_service.gui.file_tree.file_duplicate_column import FileDuplicateColumnSpec
from index_service.gui.file_tree.file_name_column import FileNameColumnSpec
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
from index_service.gui.file_tree.image_hash_column import ImageHashColumnSpec
from index_service.gui.file_tree.qt_tree_model import FileTreeModel
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
import logging

log = logging.getLogger(__name__)

FilterFn = Callable[[list[FileTreeNode]], list[FileTreeNode]]


@beartype
def filter_tree(nodes: list[FileTreeNode], filter_fn: FilterFn) -> list[FileTreeNode]:
    """
    Apply `filter_fn` to every level of the tree, leaves first.

    For each node the children are filtered before the node's sibling list is
    passed to `filter_fn`. A node is deleted by not being returned from
    `filter_fn`; an empty return simply yields an empty `nested` on the parent.
    """
    rebuilt: list[FileTreeNode] = []
    for node in nodes:
        copied = node.model_copy()
        copied.nested = filter_tree(node.nested, filter_fn)
        rebuilt.append(copied)

    return list(filter_fn(rebuilt))


@beartype
def build_filter(query_text: str) -> FilterFn:
    namespace: dict = {}
    exec("import glom", namespace)
    namespace["FileTreeNode"] = FileTreeNode

    code = compile(query_text, "<glom-query>", "exec")
    exec(code, namespace)

    filter_obj = namespace.get("filter")
    if filter_obj is None or not callable(filter_obj):
        raise ValueError("query must define a callable named 'filter'")

    def filter_fn(nodes: list[FileTreeNode]) -> list[FileTreeNode]:
        return list(filter_obj(nodes))

    return filter_fn


@beartype
class FileTreeRegion(QWidget):
    """A single vertical region: file tree on top, glom query field below."""

    # Emitted with `self` when the user submits the query in this region.
    query_submitted = Signal(object)

    def __init__(
        self,
        nodes: list[FileTreeNode],
        columns: list[FileTreeColumnSpec],
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)

        self._nodes = nodes
        self.columns = columns

        self.model = FileTreeModel(nodes=nodes, parent=self, columns=columns)
        self.tree_view = QTreeView(self)
        self.tree_view.setIndentation(20)
        self.tree_view.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.tree_view.setModel(self.model)
        self.model.configureView(self.tree_view)

        self.query_edit = QPlainTextEdit(self)
        self.query_edit.setFont(QFont("monospace"))
        self.query_edit.setPlaceholderText(
            "glom query (python), e.g. [n for n in nodes if not n.is_directory]")

        self.run_button = QPushButton("Filter \u2192", self)
        self.run_button.clicked.connect(self._on_run)

        # Ctrl+Enter submits from inside the text field.
        submit = QShortcut(QKeySequence("Ctrl+Return"), self.query_edit)
        submit.activated.connect(self._on_run)

        bottom = QWidget(self)
        bottom_layout = QVBoxLayout(bottom)
        bottom_layout.setContentsMargins(0, 0, 0, 0)
        bottom_layout.addWidget(self.query_edit)
        bottom_layout.addWidget(self.run_button)

        self.splitter = QSplitter(Qt.Orientation.Vertical, self)
        self.splitter.addWidget(self.tree_view)
        self.splitter.addWidget(bottom)
        self.splitter.setStretchFactor(0, 4)
        self.splitter.setStretchFactor(1, 1)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.splitter)

    def query_text(self) -> str:
        return self.query_edit.toPlainText().strip()

    def selected_nodes(self) -> list[FileTreeNode]:
        selection = self.tree_view.selectionModel()
        nodes: list[FileTreeNode] = []
        for index in selection.selectedRows():
            node = index.internalPointer()
            if node is not None:
                nodes.append(node)

        return nodes

    def compute_filtered(self) -> list[FileTreeNode] | None:
        text = self.query_text()
        if not text:
            return None

        filter_fn = build_filter(text)
        # No focused rows -> filter everything, otherwise scope to the selection.
        selected = self.selected_nodes()
        scope = selected if selected else self._nodes
        log.info("Running filter")
        result = filter_tree(scope, filter_fn)
        log.debug("filter OK")
        return result

    def _on_run(self) -> None:
        self.query_submitted.emit(self)


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
                    # ImageHashColumnSpec(None),
                    FileDuplicateColumnSpec(None),
                ],
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
        )

        self.resize(1200, 800)

        self.columns = columns
        self.regions: list[FileTreeRegion] = []

        self.region_splitter = QSplitter(Qt.Orientation.Horizontal, self)
        self.setCentralWidget(self.region_splitter)

        self._add_region(nodes)

        self.restore_ui_state()

    def _add_region(self, nodes: list[FileTreeNode]) -> FileTreeRegion:
        region = FileTreeRegion(
            nodes=nodes,
            columns=self.columns,
            parent=self.region_splitter,
        )
        region.query_submitted.connect(self._on_query_submitted)
        self.region_splitter.addWidget(region)
        self.regions.append(region)
        return region

    def _on_query_submitted(self, source_region: FileTreeRegion) -> None:
        try:
            filtered = source_region.compute_filtered()
        except Exception as error:
            log.exception("glom query failed")
            QMessageBox.warning(self, "Query error", str(error))
            return

        if filtered is None:
            return

        # Drop every region to the right of the source (create-or-update).
        source_index = self.regions.index(source_region)
        while len(self.regions) > source_index + 1:
            stale = self.regions.pop()
            stale.setParent(None)
            stale.deleteLater()

        self._add_region(filtered)

    def restore_ui_state(self) -> None:
        settings = QSettings()

        geometry = settings.value("window/geometry")
        if geometry is not None:
            self.restoreGeometry(geometry)

        window_state = settings.value("window/state")
        if window_state is not None:
            self.restoreState(window_state)

        header_state = settings.value("tree/headerState")
        if header_state is not None and self.regions:
            self.regions[0].tree_view.header().restoreState(header_state)

    def save_ui_state(self) -> None:
        settings = QSettings()
        settings.setValue("window/geometry", self.saveGeometry())
        settings.setValue("window/state", self.saveState())
        if self.regions:
            settings.setValue(
                "tree/headerState",
                self.regions[0].tree_view.header().saveState(),
            )

    def closeEvent(self, event: QCloseEvent) -> None:
        self.save_ui_state()
        super().closeEvent(event)

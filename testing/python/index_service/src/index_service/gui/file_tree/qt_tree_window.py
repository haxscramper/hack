import json
import linecache
from pathlib import Path

from PyQt6.Qsci import QsciScintilla, QsciLexerPython
from PyQt6.QtCore import QCoreApplication, QSettings, Qt, pyqtSignal
from PyQt6.QtGui import QCloseEvent, QFont, QKeySequence, QShortcut
from beartype import beartype
from beartype.typing import Callable, Sequence

from PyQt6.QtWidgets import (
    QAbstractItemView,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QSplitter,
    QTreeView,
    QVBoxLayout,
    QWidget,
    QComboBox,
    QHBoxLayout,
    QInputDialog,
)

from index_service.cli.cli_config import FileTreeViewConfig
from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.preview_pane import FilePreviewPane
from index_service.gui.common.qt_model_roles import CustomModelRole
from index_service.gui.file_tree.base_tree_model import build_file_tree, FileTreeNode
from index_service.gui.file_tree.file_duplicate_column import FileDuplicateColumnSpec
from index_service.gui.file_tree.file_name_column import FileNameColumnSpec
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
from index_service.gui.file_tree.image_hash_column import ImageHashColumnSpec
from index_service.gui.file_tree.python_code_editor import PythonQueryEditor, QUERY_FILENAME, QueryError, as_query_error
from index_service.gui.file_tree.qt_tree_model import FileTreeModel
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
import logging

from index_service.services.core.types import FileHash

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
    linecache.cache[QUERY_FILENAME] = (
        len(query_text),
        None,
        query_text.splitlines(keepends=True),
        QUERY_FILENAME,
    )

    namespace: dict = {}
    exec("import glom", namespace)
    namespace["FileTreeNode"] = FileTreeNode

    code = compile(query_text, QUERY_FILENAME, "exec")
    exec(code, namespace)

    filter_obj = namespace.get("filter")
    if filter_obj is None or not callable(filter_obj):
        raise QueryError("query must define a callable named 'filter'")

    def filter_fn(nodes: list[FileTreeNode]) -> list[FileTreeNode]:
        return list(filter_obj(nodes))

    return filter_fn


@beartype
class FileTreeRegion(QWidget):
    """A single vertical region: file tree on top, Python query editor below."""

    query_submitted = pyqtSignal(object)
    named_queries_changed = pyqtSignal()
    file_hash_activated = pyqtSignal(object)

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
        self.tree_view.doubleClicked.connect(self._on_tree_item_double_clicked)

        self.query_edit = PythonQueryEditor(self)

        self.saved_query_combo = QComboBox(self)
        self.saved_query_combo.activated.connect(self._on_saved_query_selected)

        self.save_query_button = QPushButton("Save query…", self)
        self.save_query_button.clicked.connect(self._save_named_query)

        query_toolbar = QWidget(self)
        query_toolbar_layout = QHBoxLayout(query_toolbar)
        query_toolbar_layout.setContentsMargins(0, 0, 0, 0)
        query_toolbar_layout.addWidget(self.saved_query_combo, 1)
        query_toolbar_layout.addWidget(self.save_query_button)

        self.run_button = QPushButton("Filter →", self)
        self.run_button.clicked.connect(self._on_run)

        submit = QShortcut(QKeySequence("Ctrl+Return"), self.query_edit)
        submit.activated.connect(self._on_run)

        bottom = QWidget(self)
        bottom_layout = QVBoxLayout(bottom)
        bottom_layout.setContentsMargins(0, 0, 0, 0)
        bottom_layout.addWidget(query_toolbar)
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

        self.refresh_named_queries()

    @staticmethod
    def _read_named_queries() -> dict[str, str]:
        serialized = QSettings().value("queries/named", "{}")
        queries = json.loads(str(serialized))

        if not isinstance(queries, dict):
            raise ValueError("queries/named must contain a JSON object")

        return {str(name): str(query) for name, query in queries.items()}

    @staticmethod
    def _write_named_queries(queries: dict[str, str]) -> None:
        QSettings().setValue(
            "queries/named",
            json.dumps(queries, sort_keys=True),
        )

    def _on_tree_item_double_clicked(self, index) -> None:
        hash_value = index.data(CustomModelRole.HashRole.value)
        if hash_value is None:
            log.info(f"hash value is None")
        else:
            self.file_hash_activated.emit(FileHash(hash=hash_value))

    def refresh_named_queries(self) -> None:
        selected_name = self.saved_query_combo.currentData()
        queries = self._read_named_queries()

        self.saved_query_combo.blockSignals(True)
        self.saved_query_combo.clear()
        self.saved_query_combo.addItem("Saved queries…", None)

        for name in sorted(queries, key=str.casefold):
            self.saved_query_combo.addItem(name, name)

        if selected_name is not None:
            index = self.saved_query_combo.findData(selected_name)
            if index >= 0:
                self.saved_query_combo.setCurrentIndex(index)

        self.saved_query_combo.blockSignals(False)

    def _on_saved_query_selected(self, index: int) -> None:
        name = self.saved_query_combo.itemData(index)
        if name is None:
            return

        query = self._read_named_queries()[name]
        self.query_edit.setText(query)
        self.query_edit.setFocus()

    def _save_named_query(self, checked: bool = False) -> None:
        name, accepted = QInputDialog.getText(
            self,
            "Save query",
            "Query name:",
        )
        if not accepted:
            return

        name = name.strip()
        if not name:
            return

        queries = self._read_named_queries()
        queries[name] = self.query_edit.text()
        self._write_named_queries(queries)

        self.named_queries_changed.emit()

        index = self.saved_query_combo.findData(name)
        if index >= 0:
            self.saved_query_combo.setCurrentIndex(index)

    def query_text(self) -> str:
        return self.query_edit.text()

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
        if not text.strip():
            return None

        try:
            filter_fn = build_filter(text)

            selected = self.selected_nodes()
            scope = selected if selected else self._nodes

            log.info("Running filter")
            result = filter_tree(scope, filter_fn)
            log.debug("filter OK")
            return result

        except QueryError:
            raise

        except Exception as exc:
            query_error = as_query_error(exc)
            if query_error is not None:
                raise query_error from exc

            # This was an application/data-model error, not an error in query code.
            raise

    def _on_run(self, checked: bool = False) -> None:
        self.query_submitted.emit(self)


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
        self.main_splitter.setStretchFactor(0, 4)
        self.main_splitter.setStretchFactor(1, 1)

        self._add_region(nodes)

        self.restore_ui_state()

    def _refresh_named_queries(self) -> None:
        for region in self.regions:
            region.refresh_named_queries()

    def _save_first_region_query(self) -> None:
        if self.regions:
            QSettings().setValue(
                "queries/firstRegionCurrent",
                self.regions[0].query_edit.text(),
            )

    def _add_region(self, nodes: list[FileTreeNode]) -> FileTreeRegion:
        is_first_region = not self.regions

        region = FileTreeRegion(
            nodes=nodes,
            columns=self.columns,
            parent=self.region_splitter,
        )
        region.query_submitted.connect(self._on_query_submitted)
        region.named_queries_changed.connect(self._refresh_named_queries)
        region.file_hash_activated.connect(self.preview_pane.show_hash)

        self.region_splitter.addWidget(region)
        self.regions.append(region)

        if is_first_region:
            region.query_edit.textChanged.connect(self._save_first_region_query)

        return region

    def _on_query_submitted(self, source_region: FileTreeRegion) -> None:
        try:
            filtered = source_region.compute_filtered()

        except QueryError as error:
            source_region.query_edit.show_query_error(error)
            return

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

            settings.setValue(
                "queries/firstRegionCurrent",
                self.regions[0].query_edit.text(),
            )

    def closeEvent(self, event: QCloseEvent) -> None:
        self.save_ui_state()
        super().closeEvent(event)

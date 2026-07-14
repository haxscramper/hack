import json

from beartype import beartype
from PyQt6.QtWidgets import QAbstractItemView, QComboBox, QHBoxLayout, QInputDialog, QPushButton, QSplitter, QTreeView, QVBoxLayout, QWidget
from PyQt6.QtCore import QSettings, Qt, pyqtSignal
from pytestqt.qtbot import QKeySequence
from PyQt6.QtGui import QKeySequence, QShortcut

from index_service.gui.abstract_models.column_model import AbstractColumnItemModel
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.gui.file_tree.python_code_editor import PythonQueryEditor
from index_service.gui.common.qt_model_roles import CustomModelRole
import logging

from index_service.gui.file_tree.qt_tree_model import FileTreeModel
from index_service.gui.file_tree.query_filter import QueryFilterEvaluator
from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)


@beartype
class FileTreeRegion(QWidget):
    """A single vertical region: file tree on top, Python query editor below."""

    query_submitted = pyqtSignal(object)
    named_queries_changed = pyqtSignal()
    file_hash_activated = pyqtSignal(object)

    def __init__(
        self,
        model: AbstractColumnItemModel,
        columns: list[FileTreeColumnSpec],
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)

        self.columns = columns

        self.model = model
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

    def compute_filtered(self) -> AbstractColumnItemModel:
        text = self.query_text()
        selected = self.selected_nodes()
        scope = selected if selected else None
        eval = QueryFilterEvaluator()

        return eval.filter_model(
            self.model,
            text,
            scope_nodes=scope,
        )

    def _on_run(self, checked: bool = False) -> None:
        log.info("run clicked")
        self.query_submitted.emit(self)

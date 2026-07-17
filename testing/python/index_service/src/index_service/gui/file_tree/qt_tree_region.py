import json
import logging

from beartype import beartype
from PyQt6.QtCore import QPoint, Qt, pyqtSignal, QSettings
from PyQt6.QtGui import QKeySequence, QShortcut
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QDialog,
    QHBoxLayout,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QSplitter,
    QToolButton,
    QTreeView,
    QVBoxLayout,
    QWidget,
)

from index_service.gui.abstract_models.column_model import AbstractColumnItemModel
from index_service.gui.common.qt_model_roles import CustomModelRole
from index_service.gui.common.qt_utils import get_settings
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.gui.file_tree.python_code_editor import PythonQueryEditor
from index_service.gui.file_tree.query_filter import QueryFilterEvaluator, QueryResultModel
from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)


@beartype
class QueryPickerPopup(QDialog):
    query_selected = pyqtSignal(str, str)

    def __init__(self, queries: dict[str, str], parent: QWidget | None = None) -> None:
        super().__init__(parent, Qt.WindowType.Popup)
        self.setWindowTitle("Select query")
        self.queries = queries

        self.search = QLineEdit(self)
        self.search.setPlaceholderText("Search queries...")

        self.list_widget = QListWidget(self)
        self.list_widget.itemClicked.connect(self._on_item_clicked)
        self.list_widget.itemActivated.connect(self._on_item_clicked)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(8, 8, 8, 8)
        layout.setSpacing(6)
        layout.addWidget(self.search)
        layout.addWidget(self.list_widget)

        self.search.textChanged.connect(self._rebuild_list)
        self.search.returnPressed.connect(self._activate_current)

        self._rebuild_list()
        self.resize(320, 360)
        self.search.setFocus()

    def _rebuild_list(self) -> None:
        filter_text = self.search.text().strip().lower()
        self.list_widget.clear()

        for name in sorted(self.queries, key=str.casefold):
            if filter_text and filter_text not in name.lower():
                continue
            self.list_widget.addItem(QListWidgetItem(name))

        if self.list_widget.count() > 0:
            self.list_widget.setCurrentRow(0)

    def _activate_current(self) -> None:
        item = self.list_widget.currentItem()
        if item is not None:
            self._on_item_clicked(item)

    def _on_item_clicked(self, item: QListWidgetItem) -> None:
        name = item.text()
        self.query_selected.emit(name, self.queries[name])
        self.close()


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
        region_id: str,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)

        self.columns = columns
        self.model = model
        self.region_id = region_id
        self.query_picker: QueryPickerPopup | None = None

        self.tree_view = QTreeView(self)
        self.tree_view.setIndentation(20)
        self.tree_view.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.tree_view.setModel(self.model)
        self.model.configureView(self.tree_view)
        self.tree_view.doubleClicked.connect(self._on_tree_item_double_clicked)

        self.query_edit = PythonQueryEditor(self)

        self.select_query_button = QToolButton(self)
        self.select_query_button.setText("☰")
        self.select_query_button.setToolTip("Select saved query")
        self.select_query_button.clicked.connect(self._show_query_picker)

        self.query_name_edit = QLineEdit(self)
        self.query_name_edit.setPlaceholderText("Query name")

        self.new_query_button = QPushButton("New", self)
        self.new_query_button.clicked.connect(self._new_query)

        self.save_query_button = QPushButton("Save", self)
        self.save_query_button.clicked.connect(self._save_named_query)

        query_toolbar = QWidget(self)
        query_toolbar_layout = QHBoxLayout(query_toolbar)
        query_toolbar_layout.setContentsMargins(0, 0, 0, 0)
        query_toolbar_layout.setSpacing(8)
        query_toolbar_layout.addWidget(self.select_query_button)
        query_toolbar_layout.addWidget(self.query_name_edit, 1)
        query_toolbar_layout.addWidget(self.new_query_button)
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

        self.query_name_edit.textChanged.connect(self._persist_current_query_state)
        if hasattr(self.query_edit, "textChanged"):
            self.query_edit.textChanged.connect(self._persist_current_query_state)

        self._load_current_query_state()

    @staticmethod
    def _read_named_queries() -> dict[str, str]:
        serialized = get_settings().value("queries/named", "{}")
        queries = json.loads(str(serialized))

        if not isinstance(queries, dict):
            raise ValueError("queries/named must contain a JSON object")

        return {str(name): str(query) for name, query in queries.items()}

    @staticmethod
    def _write_named_queries(queries: dict[str, str]) -> None:
        get_settings().setValue("queries/named", json.dumps(queries, sort_keys=True))

    def _current_query_settings_key(self) -> str:
        return f"queries/current/{self.region_id}"

    def _read_current_query_state(self) -> dict[str, str]:
        serialized = get_settings().value(self._current_query_settings_key(), "{}")
        state = json.loads(str(serialized))

        if not isinstance(state, dict):
            raise ValueError(
                f"{self._current_query_settings_key()} must contain a JSON object")

        return {
            "name": str(state.get("name", "")),
            "text": str(state.get("text", "")),
        }

    def _write_current_query_state(self, name: str, text: str) -> None:
        get_settings().setValue(
            self._current_query_settings_key(),
            json.dumps({
                "name": name,
                "text": text
            }, sort_keys=True),
        )

    def _persist_current_query_state(self, *args) -> None:
        self._write_current_query_state(
            self.query_name_edit.text(),
            self.query_edit.text(),
        )

    def _load_current_query_state(self) -> None:
        state = self._read_current_query_state()
        self.query_name_edit.setText(state["name"])
        self.query_edit.setText(state["text"])

    def _on_tree_item_double_clicked(self, index) -> None:
        hash_value = index.data(CustomModelRole.HashRole.value)
        if hash_value is None:
            log.info("hash value is None")
        else:
            self.file_hash_activated.emit(FileHash(hash=hash_value))

    def refresh_named_queries(self) -> None:
        # Kept for API compatibility with existing callers.
        pass

    def _show_query_picker(self, checked: bool = False) -> None:
        queries = self._read_named_queries()
        self.query_picker = QueryPickerPopup(queries, self)
        self.query_picker.query_selected.connect(self._load_named_query)

        button_pos = self.select_query_button.mapToGlobal(
            QPoint(0, self.select_query_button.height()))
        self.query_picker.move(button_pos)
        self.query_picker.show()

    def _load_named_query(self, name: str, query: str) -> None:
        self.query_name_edit.setText(name)
        self.query_edit.setText(query)
        self.query_edit.setFocus()
        self._persist_current_query_state()

    def _new_query(self, checked: bool = False) -> None:
        self.query_name_edit.clear()
        self.query_edit.setText("")
        self.query_edit.setFocus()
        self._persist_current_query_state()

    def _save_named_query(self, checked: bool = False) -> None:
        name = self.query_name_edit.text().strip()
        if not name:
            return

        queries = self._read_named_queries()
        queries[name] = self.query_edit.text()
        self._write_named_queries(queries)
        self.named_queries_changed.emit()
        self._persist_current_query_state()

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

    def compute_filtered(self) -> QueryResultModel:
        text = self.query_text()
        selected = self.selected_nodes()
        scope = selected if selected else None
        evaluator = QueryFilterEvaluator()

        return evaluator.filter_model(
            self.model,
            text,
            scope_nodes=scope,
        )

    def _on_run(self, checked: bool = False) -> None:
        log.info("run clicked")
        self.query_submitted.emit(self)

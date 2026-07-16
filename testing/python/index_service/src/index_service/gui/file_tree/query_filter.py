import ast
import linecache
import logging
from dataclasses import dataclass

from PyQt6.QtCore import QModelIndex, QObject, QAbstractListModel, Qt
from beartype import beartype
from beartype.typing import Callable
from pydantic import BaseModel, ConfigDict

from index_service.gui.abstract_models.column_model import AbstractColumnItemModel
from index_service.gui.file_tree.base_tree_model import FileTreeNode
from index_service.gui.file_tree.python_code_editor import (
    QUERY_FILENAME,
    QueryError,
    as_query_error,
)
from index_service.gui.file_tree.qt_tree_model import FileTreeModel

log = logging.getLogger(__name__)

FilterFn = Callable[[list[FileTreeNode]], list[FileTreeNode]]
ActionsFn = Callable[["ActionProvider", list[FileTreeNode]], object]


class BaseAction(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    def __str__(self) -> str:
        fields = ", ".join(f"{key}={value!r}" for key, value in self.__dict__.items())
        return f"{self.__class__.__name__}({fields})"


class TrashAction(BaseAction):
    file: FileTreeNode


class MoveAction(BaseAction):
    file: FileTreeNode
    dest: str


@beartype
class ActionProvider:

    def __init__(self) -> None:
        self.actions: list[BaseAction] = []

    def trash(self, file: FileTreeNode) -> None:
        self.actions.append(TrashAction(file=file))

    def move(self, file: FileTreeNode, dest: str) -> None:
        self.actions.append(MoveAction(file=file, dest=dest))


@beartype
class ActionListModel(QAbstractListModel):
    ActionRole = Qt.ItemDataRole.UserRole + 1

    def __init__(self, actions: list[BaseAction], parent: QObject | None = None) -> None:
        super().__init__(parent)
        self._actions = actions

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self._actions)

    def data(self, index: QModelIndex,
             role: int = int(Qt.ItemDataRole.DisplayRole)) -> object:
        if not index.isValid():
            return None

        action = self._actions[index.row()]

        match role:
            case Qt.ItemDataRole.DisplayRole:
                match action:
                    case TrashAction():
                        return f"trash {action.file.path}"

                    case _:
                        return str(action)

            case self.ActionRole:
                return action

        return None

    def roleNames(self) -> dict[int, bytes]:
        names = super().roleNames()
        names[self.ActionRole] = b"action"  # type: ignore
        return names  # type: ignore

    def actions(self) -> list[BaseAction]:
        return self._actions


QueryResultModel = AbstractColumnItemModel | ActionListModel


@dataclass(slots=True)
class QueryProgram:
    filter_fn: FilterFn | None
    actions_fn: ActionsFn | None
    action_provider: ActionProvider | None


@beartype
class QueryFilterEvaluator:

    def _actions_tree(
        self,
        nodes: list[FileTreeNode],
        actions_fn: ActionsFn,
        provider: ActionProvider,
    ) -> list[FileTreeNode]:
        rebuilt: list[FileTreeNode] = []

        for node in nodes:
            copied = node.model_copy()
            copied.nested = self._actions_tree(node.nested, actions_fn, provider)
            rebuilt.append(copied)

        actions_fn(provider, rebuilt)
        return rebuilt

    def _filter_tree(self, nodes: list[FileTreeNode],
                     filter_fn: FilterFn) -> list[FileTreeNode]:
        rebuilt: list[FileTreeNode] = []

        for node in nodes:
            copied = node.model_copy()
            copied.nested = self._filter_tree(node.nested, filter_fn)
            rebuilt.append(copied)

        return list(filter_fn(rebuilt))

    def _parse_query_shape(self, query_text: str) -> tuple[bool, bool]:
        tree = ast.parse(query_text, QUERY_FILENAME, "exec")
        has_filter = False
        has_actions = False

        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                if node.name == "filter":
                    has_filter = True
                elif node.name == "actions":
                    has_actions = True

        if has_filter and has_actions:
            raise QueryError(
                "query must define exactly one function: 'filter' or 'actions'")
        if not has_filter and not has_actions:
            raise QueryError(
                "query must define exactly one function: 'filter' or 'actions'")

        return has_filter, has_actions

    def _build_program(self, query_text: str) -> QueryProgram:
        has_filter, has_actions = self._parse_query_shape(query_text)

        linecache.cache[QUERY_FILENAME] = (
            len(query_text),
            None,
            query_text.splitlines(keepends=True),
            QUERY_FILENAME,
        )

        namespace: dict[str, object] = {}
        exec("import glom", namespace)
        namespace["FileTreeNode"] = FileTreeNode

        action_provider: ActionProvider | None = None
        if has_actions:
            action_provider = ActionProvider()
            namespace["act"] = action_provider

        code = compile(query_text, QUERY_FILENAME, "exec")
        exec(code, namespace)

        filter_fn: FilterFn | None = None
        if has_filter:
            filter_obj = namespace.get("filter")
            if filter_obj is None or not callable(filter_obj):
                raise QueryError("query defines 'filter' but it is not callable")

            def built_filter(nodes: list[FileTreeNode]) -> list[FileTreeNode]:
                return list(filter_obj(nodes))  # type: ignore

            filter_fn = built_filter

        actions_fn: ActionsFn | None = None
        if has_actions:
            actions_obj = namespace.get("actions")
            if actions_obj is None or not callable(actions_obj):
                raise QueryError("query defines 'actions' but it is not callable")

            def built_actions(provider: ActionProvider,
                              nodes: list[FileTreeNode]) -> object:
                return actions_obj(provider, nodes)  # type: ignore

            actions_fn = built_actions

        return QueryProgram(
            filter_fn=filter_fn,
            actions_fn=actions_fn,
            action_provider=action_provider,
        )

    def filter_model(
        self,
        model: AbstractColumnItemModel,
        query_text: str,
        *,
        scope_nodes: list[FileTreeNode] | None = None,
        parent: QObject | None = None,
    ) -> QueryResultModel:
        try:
            program = self._build_program(query_text)
            scope = scope_nodes if scope_nodes else model.index(
                0,
                0,
                QModelIndex(),
            ).internalPointer().nested

            if program.filter_fn:
                filtered_nodes = self._filter_tree(scope, program.filter_fn)
                return FileTreeModel(
                    nodes=filtered_nodes,
                    columns=model.columns,
                    parent=parent,
                )

            elif program.actions_fn:
                assert program.action_provider
                self._actions_tree(scope, program.actions_fn, program.action_provider)
                return ActionListModel(actions=program.action_provider.actions)

            else:
                raise QueryError("No `filter` or `actions` model, cannot filter")

        except QueryError:
            raise

        except Exception as exc:
            query_error = as_query_error(exc)
            if query_error is not None:
                raise query_error from exc
            raise

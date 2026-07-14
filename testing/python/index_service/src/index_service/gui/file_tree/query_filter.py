import ast
import linecache
import logging
from dataclasses import dataclass

from PyQt6.QtCore import QModelIndex, QObject
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


@dataclass(slots=True)
class QueryExecutionResult:
    filtered_model: AbstractColumnItemModel | None
    actions: list[BaseAction]


@dataclass(slots=True)
class QueryProgram:
    has_filter: bool
    has_actions: bool
    filter_fn: FilterFn | None
    actions_fn: ActionsFn | None
    action_provider: ActionProvider | None


@beartype
class QueryFilterEvaluator:

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

        return has_filter, has_actions

    def _build_program(self, query_text: str) -> QueryProgram:
        has_filter, has_actions = self._parse_query_shape(query_text)

        if not has_filter and not has_actions:
            raise QueryError("query must define function 'filter' and/or 'actions'")

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
                return list(filter_obj(nodes))

            filter_fn = built_filter

        actions_fn: ActionsFn | None = None
        if has_actions:
            actions_obj = namespace.get("actions")
            if actions_obj is None or not callable(actions_obj):
                raise QueryError("query defines 'actions' but it is not callable")

            def built_actions(provider: ActionProvider,
                              nodes: list[FileTreeNode]) -> object:
                return actions_obj(provider, nodes)

            actions_fn = built_actions

        return QueryProgram(
            has_filter=has_filter,
            has_actions=has_actions,
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
    ) -> QueryExecutionResult:
        try:
            program = self._build_program(query_text)
            scope = scope_nodes if scope_nodes else model.index(
                0,
                0,
                QModelIndex(),
            ).internalPointer().nested

            filtered_nodes: list[FileTreeNode] | None = None
            filtered_model: AbstractColumnItemModel | None = None

            if program.has_filter:
                if program.filter_fn is None:
                    raise QueryError("missing filter callable")
                filtered_nodes = self._filter_tree(scope, program.filter_fn)
                filtered_model = FileTreeModel(
                    nodes=filtered_nodes,
                    columns=model.columns,
                    parent=parent,
                )

            if program.has_actions:
                if program.actions_fn is None or program.action_provider is None:
                    raise QueryError("missing actions callable")
                action_scope = filtered_nodes if filtered_nodes is not None else scope
                program.actions_fn(program.action_provider, action_scope)

            actions: list[BaseAction] = []
            if program.action_provider is not None:
                actions = program.action_provider.actions

            return QueryExecutionResult(
                filtered_model=filtered_model,
                actions=actions,
            )

        except QueryError:
            raise

        except Exception as exc:
            query_error = as_query_error(exc)
            if query_error is not None:
                raise query_error from exc
            raise

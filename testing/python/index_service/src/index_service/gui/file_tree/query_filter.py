import ast
import linecache
import logging
from dataclasses import dataclass

from PyQt6.QtCore import QModelIndex, QObject
from beartype import beartype
from beartype.typing import Callable, cast

from index_service.gui.abstract_models.column_model import AbstractColumnItemModel
from index_service.gui.file_tree.actions.action_list_model import ActionListModel, ActionProvider
from index_service.gui.file_tree.columns.file_tree_column import FileTreeNode
from index_service.gui.file_tree.python_code_editor import (
    QUERY_FILENAME,
    QueryError,
    as_query_error,
)
from index_service.gui.file_tree.qt_tree_model import FileTreeModel

log = logging.getLogger(__name__)

FilterFn = Callable[[list[FileTreeNode]], list[FileTreeNode]]
TraverseFn = Callable[["ActionProvider", list[FileTreeNode]], object]
ActionsFn = TraverseFn

QueryResultModel = AbstractColumnItemModel | ActionListModel


@dataclass(slots=True)
class QueryProgram:
    filter_fn: FilterFn | None
    actions_fn: ActionsFn | None
    pre_traverse_fn: TraverseFn | None
    post_traverse_fn: TraverseFn | None
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

    def _parse_query_shape(self, query_text: str) -> str:
        tree = ast.parse(query_text, QUERY_FILENAME, "exec")
        main_defs: list[str] = []

        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                if node.name == "filter":
                    main_defs.append("filter")
                elif node.name in ("actions", "action"):
                    main_defs.append("actions")

        if len(main_defs) != 1:
            raise QueryError(
                "query must define exactly one main function: 'filter' or 'actions'/'action'"
            )

        return main_defs[0]

    def _build_program(self, query_text: str) -> QueryProgram:
        main_kind = self._parse_query_shape(query_text)

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
        if main_kind == "actions":
            action_provider = ActionProvider()
            namespace["act"] = action_provider

        code = compile(query_text, QUERY_FILENAME, "exec")
        exec(code, namespace)

        def build_traverse_fn(name: str) -> TraverseFn | None:
            obj = cast(TraverseFn | None, namespace.get(name))
            if obj is None:
                return None

            if not callable(obj):
                raise QueryError(f"query defines '{name}' but it is not callable")

            def built(provider: ActionProvider, nodes: list[FileTreeNode]) -> object:
                return obj(provider, nodes)  # type: ignore

            return built

        filter_fn: FilterFn | None = None
        if main_kind == "filter":
            filter_obj = cast(FilterFn | None, namespace.get("filter"))
            if filter_obj is None or not callable(filter_obj):
                raise QueryError("query defines 'filter' but it is not callable")

            def built_filter(nodes: list[FileTreeNode]) -> list[FileTreeNode]:
                return list(filter_obj(nodes))  # type: ignore

            filter_fn = built_filter

        actions_fn: ActionsFn | None = None
        if main_kind == "actions":
            actions_obj = cast(ActionsFn | None, namespace.get("actions"))
            if actions_obj is None:
                actions_obj = namespace.get("action")
            if actions_obj is None or not callable(actions_obj):
                raise QueryError(
                    "query defines 'actions'/'action' but it is not callable")

            def built_actions(provider: ActionProvider,
                              nodes: list[FileTreeNode]) -> object:
                return actions_obj(provider, nodes)  # type: ignore

            actions_fn = built_actions

        pre_traverse_fn = build_traverse_fn("pre_traverse")
        post_traverse_fn = build_traverse_fn("post_traverse")

        return QueryProgram(
            filter_fn=filter_fn,
            actions_fn=actions_fn,
            pre_traverse_fn=pre_traverse_fn,
            post_traverse_fn=post_traverse_fn,
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
                pre_provider = ActionProvider()
                post_provider = ActionProvider()

                if program.pre_traverse_fn:
                    self._actions_tree(scope, program.pre_traverse_fn, pre_provider)

                filtered_nodes = self._filter_tree(scope, program.filter_fn)

                if program.post_traverse_fn:
                    self._actions_tree(scope, program.post_traverse_fn, post_provider)

                return FileTreeModel(
                    nodes=filtered_nodes,
                    columns=model.columns,
                    parent=parent,
                )

            elif program.actions_fn:
                assert program.action_provider
                provider = program.action_provider

                if program.pre_traverse_fn:
                    self._actions_tree(scope, program.pre_traverse_fn, provider)

                self._actions_tree(scope, program.actions_fn, provider)

                if program.post_traverse_fn:
                    self._actions_tree(scope, program.post_traverse_fn, provider)

                return ActionListModel(actions=provider.actions)

            else:
                raise QueryError("No `filter` or `actions` model, cannot filter")

        except QueryError:
            raise

        except Exception as exc:
            query_error = as_query_error(exc)
            if query_error is not None:
                raise query_error from exc
            raise

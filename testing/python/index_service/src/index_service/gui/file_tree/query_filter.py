import linecache
import logging

from PyQt6.QtCore import QModelIndex, QObject
from beartype import beartype
from beartype.typing import Callable

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

    def _build_filter(self, query_text: str) -> FilterFn:
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

    def filter_model(
        self,
        model: AbstractColumnItemModel,
        query_text: str,
        *,
        scope_nodes: list[FileTreeNode] | None = None,
        parent: QObject | None = None,
    ) -> AbstractColumnItemModel:
        try:
            filter_fn = self._build_filter(query_text)
            scope = scope_nodes if scope_nodes else model.index(
                0, 0, QModelIndex()).internalPointer().nested
            filtered_nodes = self._filter_tree(scope, filter_fn)

            return FileTreeModel(
                nodes=filtered_nodes,
                columns=model.columns,
                parent=parent,
            )

        except QueryError:
            raise

        except Exception as exc:
            query_error = as_query_error(exc)
            if query_error is not None:
                raise query_error from exc

            raise

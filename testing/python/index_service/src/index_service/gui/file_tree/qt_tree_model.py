from beartype import beartype
from beartype.typing import Any, Sequence, cast

from PySide6.QtCore import QModelIndex, QObject, Qt

from index_service.gui.file_tree.base_tree_model import FileTreeNode
from index_service.gui.file_tree.column_model import AbstractColumnItemModel, ColumnSpec


@beartype
class FileTreeModel(AbstractColumnItemModel):

    def __init__(
        self,
        columns: Sequence[ColumnSpec],
        nodes: Sequence[FileTreeNode],
        parent: QObject | None = None,
    ) -> None:
        super().__init__(
            columns=columns,
            parent=parent,
        )
        self.nodes: tuple[FileTreeNode, ...] = tuple(nodes)
        self.parents: dict[int, FileTreeNode | None] = {}
        self.rows: dict[int, int] = {}
        self.registerNodes(self.nodes, None)

    def registerNodes(
        self,
        nodes: Sequence[FileTreeNode],
        parent: FileTreeNode | None,
    ) -> None:
        for row, node in enumerate(nodes):
            self.parents[id(node)] = parent
            self.rows[id(node)] = row
            self.registerNodes(node.nested, node)

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        match parent.isValid():
            case False:
                return len(self.nodes)
            case True if parent.column() == 0:
                node = cast(FileTreeNode, parent.internalPointer())
                return len(node.nested)
            case _:
                return 0

    def index(
            self,
            row: int,
            column: int,
            parent: QModelIndex = QModelIndex(),
    ) -> QModelIndex:
        if parent.isValid() and parent.column() != 0:
            return QModelIndex()

        nodes = (cast(FileTreeNode, parent.internalPointer()).nested
                 if parent.isValid() else self.nodes)

        if not (0 <= row < len(nodes) and 0 <= column < len(self.columns)):
            return QModelIndex()

        return self.createIndex(row, column, nodes[row])

    def parent(self, index: QModelIndex) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()

        node = cast(FileTreeNode, index.internalPointer())
        parent = self.parents[id(node)]

        if parent is None:
            return QModelIndex()

        return self.createIndex(self.rows[id(parent)], 0, parent)

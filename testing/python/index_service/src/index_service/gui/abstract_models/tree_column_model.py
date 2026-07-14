from abc import abstractmethod
from beartype.typing import Any, Generic, Sequence, TypeVar, cast
from beartype import beartype
from PyQt6.QtCore import QModelIndex, QObject

from index_service.gui.abstract_models.column_model import AbstractColumnItemModel, ColumnSpec

T = TypeVar("T")


@beartype
class AbstractTreeColumnModel(AbstractColumnItemModel, Generic[T]):

    def __init__(
        self,
        columns: Sequence[ColumnSpec],
        nodes: Sequence[T],
        parent: QObject | None = None,
    ) -> None:
        super().__init__(
            columns=columns,
            parent=parent,
        )
        self.nodes: tuple[T, ...] = tuple(nodes)
        self.parents: dict[int, T | None] = {}
        self.rows: dict[int, int] = {}
        self.registerNodes(self.nodes, None)

    @abstractmethod
    def getNestedNodes(self, node: T) -> list[T]:
        raise NotImplementedError

    def registerNodes(
        self,
        nodes: Sequence[T],
        parent: T | None,
    ) -> None:
        for row, node in enumerate(nodes):
            self.parents[id(node)] = parent
            self.rows[id(node)] = row
            self.registerNodes(self.getNestedNodes(node), node)

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        match parent.isValid():
            case False:
                return len(self.nodes)
            case True if parent.column() == 0:
                return len(self.getNestedNodes(self.node(parent)))
            case _:
                return 0

    def node(self, index: QModelIndex) -> T:
        return cast(T, index.internalPointer())

    def index(
            self,
            row: int,
            column: int,
            parent: QModelIndex = QModelIndex(),
    ) -> QModelIndex:
        if parent.isValid() and parent.column() != 0:
            return QModelIndex()

        nodes = (self.getNestedNodes(self.node(parent))
                 if parent.isValid() else self.nodes)

        if not (0 <= row < len(nodes) and 0 <= column < len(self.columns)):
            return QModelIndex()

        return self.createIndex(row, column, nodes[row])

    def parent(self, index: QModelIndex) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()

        node = self.node(index)
        parent = self.parents[id(node)]

        if parent is None:
            return QModelIndex()

        return self.createIndex(self.rows[id(parent)], 0, parent)

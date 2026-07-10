from beartype import beartype
from beartype.typing import Any, Sequence, cast

from PySide6.QtCore import QModelIndex, QObject, Qt

from index_service.gui.file_tree.base_tree_model import FileTreeNode
from index_service.gui.file_tree.column_model import AbstractColumnItemModel, ColumnSpec


@beartype
class FileTreeColumnSpec(ColumnSpec):

    def __init__(self, title: str) -> None:
        self.title = title

    def setData(
        self,
        index: QModelIndex,
        value: Any,
        role: int = Qt.ItemDataRole.EditRole,
    ) -> bool:
        return False

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        return Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable

    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        match orientation, role:
            case Qt.Orientation.Horizontal, Qt.ItemDataRole.DisplayRole:
                return self.title
            case _:
                return None

    def node(self, index: QModelIndex) -> FileTreeNode:
        return cast(FileTreeNode, index.internalPointer())


@beartype
class FileNameColumnSpec(FileTreeColumnSpec):

    def __init__(self) -> None:
        super().__init__("Name")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        match role:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole:
                return self.node(index).path.name
            case _:
                return None


@beartype
class FileHashColumnSpec(FileTreeColumnSpec):

    def __init__(self) -> None:
        super().__init__("Hash")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        node = self.node(index)

        match role, node.hash:
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole, None:
                return ""
            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole, hash:
                return str(hash)
            case _:
                return None


@beartype
class WdTagsColumnSpec(FileTreeColumnSpec):

    def __init__(self) -> None:
        super().__init__("WD Tags")

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        present = "wd_tags" in self.node(index).assets

        match role:
            case Qt.ItemDataRole.DisplayRole:
                return "Yes" if present else ""
            case Qt.ItemDataRole.CheckStateRole:
                return Qt.CheckState.Checked if present else Qt.CheckState.Unchecked
            case _:
                return None


@beartype
class FileTreeModel(AbstractColumnItemModel):

    def __init__(
        self,
        nodes: Sequence[FileTreeNode],
        parent: QObject | None = None,
    ) -> None:
        super().__init__(
            columns=[
                FileNameColumnSpec(),
                FileHashColumnSpec(),
                WdTagsColumnSpec(),
            ],
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
            self.registerNodes(node.children, node)

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        match parent.isValid():
            case False:
                return len(self.nodes)
            case True if parent.column() == 0:
                node = cast(FileTreeNode, parent.internalPointer())
                return len(node.children)
            case _:
                return 0

    def index(
            self,
            row: int,
            column: int,
            parent: QModelIndex = QModelIndex(),
    ) -> QModelIndex:
        match parent.isValid():
            case False:
                nodes = self.nodes
            case True:
                node = cast(FileTreeNode, parent.internalPointer())
                nodes = node.children

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

from abc import ABC, abstractmethod

from beartype import beartype
from beartype.typing import Any, Sequence

from PySide6.QtCore import QAbstractTableModel, QModelIndex, QObject, Qt, QAbstractItemModel
from PySide6.QtWidgets import QAbstractItemDelegate, QAbstractItemView


@beartype
class ColumnSpec(ABC):

    @abstractmethod
    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        raise NotImplementedError()

    @abstractmethod
    def setData(
        self,
        index: QModelIndex,
        value: Any,
        role: int = Qt.ItemDataRole.EditRole,
    ) -> bool:
        raise NotImplementedError()

    @abstractmethod
    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        raise NotImplementedError()

    @abstractmethod
    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        raise NotImplementedError()

    def getDelegate(self) -> QAbstractItemDelegate | None:
        return None


@beartype
class AbstractColumnItemModel(QAbstractItemModel):

    def __init__(
        self,
        columns: Sequence[ColumnSpec],
        parent: QObject | None = None,
        view: QAbstractItemView | None = None,
    ) -> None:
        super().__init__(parent)
        self.columns: tuple[ColumnSpec, ...] = tuple(columns)
        self.delegates: tuple[QAbstractItemDelegate | None, ...] = tuple(
            column.getDelegate() for column in self.columns)

        if view is not None:
            self.configureView(view)

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return len(self.columns)

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        match index.isValid():
            case False:
                return None
            case True:
                return self.columns[index.column()].data(index, role)

    def setData(
        self,
        index: QModelIndex,
        value: Any,
        role: int = Qt.ItemDataRole.EditRole,
    ) -> bool:
        match index.isValid():
            case False:
                return False
            case True:
                changed = self.columns[index.column()].setData(index, value, role)

        if changed:
            self.dataChanged.emit(index, index, [role])

        return changed

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        match index.isValid():
            case False:
                return Qt.ItemFlag.NoItemFlags
            case True:
                return self.columns[index.column()].flags(index)

    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        match orientation:
            case Qt.Orientation.Horizontal if 0 <= section < len(self.columns):
                return self.columns[section].headerData(section, orientation, role)
            case _:
                return None

    def configureView(self, view: QAbstractItemView) -> None:
        for column, delegate in enumerate(self.delegates):
            if delegate is not None:
                view.setItemDelegateForColumn(column, delegate)

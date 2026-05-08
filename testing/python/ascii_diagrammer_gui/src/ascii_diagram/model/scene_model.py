from __future__ import annotations

from typing import Any, Dict, List, Optional

from PySide6.QtCore import (
    QAbstractItemModel,
    QMimeData,
    QModelIndex,
    Qt,
    Signal,
)
from PySide6.QtCore import QByteArray, QDataStream, QIODevice

from ascii_diagram.model.enums import ShapeType
from ascii_diagram.model.scene_item import SceneItem
from ascii_diagram.model.shape_properties import ShapeProperties

SCENE_ITEM_MIME = "application/x-ascii-diagram-item"

ITEM_ROLE = Qt.ItemDataRole.UserRole + 100
PROPERTIES_ROLE = Qt.ItemDataRole.UserRole + 101
SHAPE_TYPE_ROLE = Qt.ItemDataRole.UserRole + 102
ITEM_ID_ROLE = Qt.ItemDataRole.UserRole + 103


class SceneModel(QAbstractItemModel):
    data_changed = Signal(QModelIndex, QModelIndex)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._root = SceneItem(ShapeType.RECTANGLE, item_id=0)
        SceneItem.reset_id_counter()

    def root_item(self) -> SceneItem:
        return self._root

    def item_from_index(self, index: QModelIndex) -> Optional[SceneItem]:
        if index.isValid():
            return index.internalPointer()
        return self._root

    def index_from_item(self, item: SceneItem) -> QModelIndex:
        if item is self._root:
            return QModelIndex()
        parent_item = item.parent()
        if parent_item is None:
            return QModelIndex()
        return self.createIndex(item.row(), 0, item)

    def index(self, row: int, column: int,
              parent: QModelIndex = QModelIndex()) -> QModelIndex:
        parent_item = self.item_from_index(parent)
        if parent_item is None:
            return QModelIndex()
        child = parent_item.child_at(row)
        if child is not None:
            return self.createIndex(row, column, child)
        return QModelIndex()

    def parent(self, index: QModelIndex = QModelIndex()) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()
        item = index.internalPointer()
        parent_item = item.parent()
        if parent_item is None or parent_item is self._root:
            return QModelIndex()
        return self.createIndex(parent_item.row(), 0, parent_item)

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        parent_item = self.item_from_index(parent)
        if parent_item is None:
            return 0
        return parent_item.child_count()

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return 1

    def data(self,
             index: QModelIndex,
             role: int = Qt.ItemDataRole.DisplayRole) -> Any:
        if not index.isValid():
            return None
        item: SceneItem = index.internalPointer()
        if role == Qt.ItemDataRole.DisplayRole:
            return item.display_name()
        if role == ITEM_ROLE:
            return item
        if role == PROPERTIES_ROLE:
            return item.properties
        if role == SHAPE_TYPE_ROLE:
            return item.shape_type
        if role == ITEM_ID_ROLE:
            return item.item_id
        return None

    def get_item_properties(self,
                            index: QModelIndex) -> Optional[ShapeProperties]:
        if not index.isValid():
            return None
        item: SceneItem = index.internalPointer()
        return item.properties

    def setData(self,
                index: QModelIndex,
                value: Any,
                role: int = Qt.ItemDataRole.EditRole) -> bool:
        if not index.isValid():
            return False
        if role == PROPERTIES_ROLE:
            item: SceneItem = index.internalPointer()
            item.properties = value
            self.dataChanged.emit(index, index)
            return True
        return False

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        if not index.isValid():
            return Qt.ItemFlag.ItemIsDropEnabled
        flags = (Qt.ItemFlag.ItemIsEnabled
                 | Qt.ItemFlag.ItemIsSelectable
                 | Qt.ItemFlag.ItemIsDragEnabled
                 | Qt.ItemFlag.ItemIsDropEnabled)
        return flags

    def insert_item(
        self,
        shape_type: ShapeType,
        parent_index: QModelIndex = QModelIndex(),
        row: int = -1,
    ) -> QModelIndex:
        parent_item = self.item_from_index(parent_index)
        if parent_item is None:
            return QModelIndex()
        if row < 0 or row > parent_item.child_count():
            row = parent_item.child_count()

        self.beginInsertRows(parent_index, row, row)
        new_child = SceneItem(shape_type)
        parent_item.insert_child(row, new_child)
        self.endInsertRows()
        return self.index(row, 0, parent_index)

    def insertRows(self,
                   row: int,
                   count: int,
                   parent: QModelIndex = QModelIndex()) -> bool:
        for i in range(count):
            self.insert_item(ShapeType.RECTANGLE, parent, row + i)
        return True

    def removeRows(self,
                   row: int,
                   count: int,
                   parent: QModelIndex = QModelIndex()) -> bool:
        parent_item = self.item_from_index(parent)
        if parent_item is None:
            return False
        if row < 0 or row + count > parent_item.child_count():
            return False

        self.beginRemoveRows(parent, row, row + count - 1)
        for _ in range(count):
            parent_item.remove_child(row)
        self.endRemoveRows()
        return True

    def remove_item(self, index: QModelIndex) -> bool:
        if not index.isValid():
            return False
        item = index.internalPointer()
        parent_item = item.parent()
        if parent_item is None:
            return False
        row = item.row()
        return self.removeRows(row, 1, self.parent(index))

    def moveRows(
        self,
        source_parent: QModelIndex,
        source_row: int,
        count: int,
        dest_parent: QModelIndex,
        dest_row: int,
    ) -> bool:
        if count != 1:
            return False
        if source_parent == dest_parent:
            return False

        source_item = self.item_from_index(source_parent)
        dest_item = self.item_from_index(dest_parent)
        if source_item is None or dest_item is None:
            return False

        child = source_item.child_at(source_row)
        if child is None:
            return False

        if child.is_ancestor_of(dest_item):
            return False

        if not self.beginMoveRows(source_parent, source_row, source_row,
                                  dest_parent, dest_row):
            return False
        source_item.remove_child(source_row)
        dest_item.insert_child(dest_row, child)
        self.endMoveRows()
        return True

    def mimeData(self, indexes: List[QModelIndex]) -> QMimeData:
        mime_data = QMimeData()
        encoded = QByteArray()
        stream = QDataStream(encoded, QIODevice.OpenModeFlag.WriteOnly)
        for idx in indexes:
            if idx.isValid():
                item: SceneItem = idx.internalPointer()
                parent = idx.parent()
                stream.writeInt32(item.row())
                if parent.isValid():
                    parent_item: SceneItem = parent.internalPointer()
                    stream.writeInt32(parent_item.item_id)
                else:
                    stream.writeInt32(-1)
        mime_data.setData(SCENE_ITEM_MIME, encoded)
        return mime_data

    def dropMimeData(
        self,
        data: QMimeData,
        action,
        row: int,
        column: int,
        parent: QModelIndex,
    ) -> bool:
        if not data.hasFormat(SCENE_ITEM_MIME):
            return False
        return True

    def canDropMimeData(
        self,
        data: QMimeData,
        action,
        row: int,
        column: int,
        parent: QModelIndex,
    ) -> bool:
        return data.hasFormat(SCENE_ITEM_MIME)

    def reset_model(self) -> None:
        self.beginResetModel()
        self._root = SceneItem(ShapeType.RECTANGLE, item_id=0)
        SceneItem.reset_id_counter()
        self.endResetModel()

    def all_items(self) -> List[SceneItem]:
        result = []
        for i in range(self._root.child_count()):
            child = self._root.child_at(i)
            if child:
                result.append(child)
                result.extend(child.all_descendants())
        return result

    def scene_bounds(self) -> tuple:
        """Return (min_x, min_y, max_x, max_y) of all shapes in absolute coords."""
        items = self.all_items()
        if not items:
            return (0, 0, 0, 0)
        min_x = min_y = float("inf")
        max_x = max_y = float("-inf")
        for item in items:
            abs_pos = item.absolute_position()
            props = item.properties
            sx = abs_pos.x()
            sy = abs_pos.y()
            if item.shape_type == ShapeType.EDGE:
                from ascii_diagram.model.shape_properties import EdgeData

                if isinstance(props, EdgeData):
                    ax = abs_pos.x() + props.start.x()
                    ay = abs_pos.y() + props.start.y()
                    bx = abs_pos.x() + props.end.x()
                    by = abs_pos.y() + props.end.y()
                    pts = [(ax, ay), (bx, by)]
                    for b in props.bends:
                        pts.append((abs_pos.x() + b.x(), abs_pos.y() + b.y()))
                    for px, py in pts:
                        min_x = min(min_x, px)
                        min_y = min(min_y, py)
                        max_x = max(max_x, px + 1)
                        max_y = max(max_y, py + 1)
            elif item.shape_type in (ShapeType.RECTANGLE, ShapeType.ELLIPSE,
                                     ShapeType.TEXT):
                from ascii_diagram.model.shape_properties import (
                    EllipseData,
                    RectData,
                    TextData,
                )

                if isinstance(props, (RectData, EllipseData, TextData)):
                    ex = sx + props.width
                    ey = sy + props.height
                    min_x = min(min_x, sx)
                    min_y = min(min_y, sy)
                    max_x = max(max_x, ex)
                    max_y = max(max_y, ey)

        if min_x == float("inf"):
            return (0, 0, 0, 0)
        return (int(min_x), int(min_y), int(max_x), int(max_y))


def find_item_by_id(model: SceneModel, parent: QModelIndex,
                    item_id: int) -> Optional[QModelIndex]:
    for row in range(model.rowCount(parent)):
        idx = model.index(row, 0, parent)
        if idx.data(ITEM_ID_ROLE) == item_id:
            return idx
        found = find_item_by_id(model, idx, item_id)
        if found is not None:
            return found
    return None


def decode_mime_items(data: QByteArray) -> List[Dict[str, int]]:
    """Decode MIME-encoded items. Returns list of {row, parent_id (-1 for root)}."""
    result = []
    stream = QDataStream(data, QIODevice.OpenModeFlag.ReadOnly)
    while not stream.atEnd():
        row = stream.readInt32()
        parent_id = stream.readInt32()
        result.append({"row": row, "parent_id": parent_id})
    return result

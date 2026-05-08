from __future__ import annotations

from typing import List, Optional

from PySide6.QtCore import QPoint

from ascii_diagram.model.enums import ShapeType
from ascii_diagram.model.shape_properties import (
    ShapeProperties,
    make_default_properties,
)


class SceneItem:
    _next_id: int = 1

    def __init__(
        self,
        shape_type: ShapeType,
        parent: Optional[SceneItem] = None,
        item_id: Optional[int] = None,
    ):
        self.shape_type = shape_type
        self._id = item_id if item_id is not None else SceneItem._next_id
        SceneItem._next_id = max(SceneItem._next_id, self._id + 1)
        self.properties: ShapeProperties = make_default_properties(
            shape_type.name)
        self._parent: Optional[SceneItem] = parent
        self._children: List[SceneItem] = []

    @property
    def item_id(self) -> int:
        return self._id

    def set_id(self, new_id: int) -> None:
        self._id = new_id
        SceneItem._next_id = max(SceneItem._next_id, new_id + 1)

    def parent(self) -> Optional[SceneItem]:
        return self._parent

    def set_parent(self, parent: Optional[SceneItem]) -> None:
        self._parent = parent

    def child_count(self) -> int:
        return len(self._children)

    def child_at(self, index: int) -> Optional[SceneItem]:
        if 0 <= index < len(self._children):
            return self._children[index]
        return None

    def row(self) -> int:
        if self._parent is not None:
            return self._parent._children.index(self)
        return 0

    def add_child(self, child: SceneItem) -> None:
        child.set_parent(self)
        self._children.append(child)

    def insert_child(self, index: int, child: SceneItem) -> None:
        child.set_parent(self)
        self._children.insert(index, child)

    def remove_child(self, index: int) -> Optional[SceneItem]:
        if 0 <= index < len(self._children):
            child = self._children.pop(index)
            child.set_parent(None)
            return child
        return None

    def take_child(self, child: SceneItem) -> bool:
        if child in self._children:
            self._children.remove(child)
            child.set_parent(None)
            return True
        return False

    def all_descendants(self) -> List[SceneItem]:
        result = []
        for child in self._children:
            result.append(child)
            result.extend(child.all_descendants())
        return result

    def absolute_position(self) -> QPoint:
        x, y = 0, 0
        node: Optional[SceneItem] = self
        while node is not None:
            x += node.properties.x
            y += node.properties.y
            node = node.parent()
        return QPoint(x, y)

    def is_ancestor_of(self, item: SceneItem) -> bool:
        current = item.parent()
        while current is not None:
            if current is self:
                return True
            current = current.parent()
        return False

    def display_name(self) -> str:
        return f"{self.shape_type.name} [{self._id}]"

    @staticmethod
    def reset_id_counter(start: int = 1) -> None:
        SceneItem._next_id = start

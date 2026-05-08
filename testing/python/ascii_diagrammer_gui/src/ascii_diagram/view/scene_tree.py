from __future__ import annotations

from typing import Optional

from PySide6.QtCore import QModelIndex, Qt, Signal
from PySide6.QtWidgets import QTreeView

from ascii_diagram.model.scene_model import SceneModel


class SceneTreeWidget(QTreeView):
    selection_changed_for_panel = Signal(QModelIndex)

    def __init__(self, model: SceneModel, parent=None):
        super().__init__(parent)
        self.setModel(model)
        self.setDragEnabled(True)
        self.setAcceptDrops(True)
        self.setDropIndicatorShown(True)
        self.setDragDropMode(QTreeView.DragDropMode.InternalMove)
        self.setHeaderHidden(True)
        self.setSelectionMode(QTreeView.SelectionMode.ExtendedSelection)
        self.selectionModel().currentChanged.connect(self._on_current_changed)

    def _on_current_changed(self, current: QModelIndex,
                            previous: QModelIndex) -> None:
        self.selection_changed_for_panel.emit(current)

    def delete_selected(self) -> None:
        model = self.model()
        if not isinstance(model, SceneModel):
            return
        indexes = self.selectionModel().selectedRows()
        for idx in sorted(indexes, key=lambda i: i.row(), reverse=True):
            model.remove_item(idx)

    def keyPressEvent(self, event) -> None:
        if event.key() == Qt.Key.Key_Delete:
            self.delete_selected()
        else:
            super().keyPressEvent(event)

import glom
from beartype import beartype
from beartype.typing import Any, Sequence, cast

from PyQt6.QtCore import QModelIndex, QObject, Qt

from index_service.gui.abstract_models.tree_column_model import AbstractTreeColumnModel
from index_service.gui.common.qt_model_roles import CustomModelRole
from index_service.gui.file_tree.base_tree_model import FileTreeNode
from index_service.gui.abstract_models.column_model import AbstractColumnItemModel, ColumnSpec
import logging

log = logging.getLogger(__name__)


@beartype
class FileTreeModel(AbstractTreeColumnModel[FileTreeNode]):

    def getNestedNodes(self, node: FileTreeNode) -> list[FileTreeNode]:
        return node.nested

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        match role:
            case CustomModelRole.HashRole.value:
                log.debug(self.node(index).hash)
                return glom.glom(self.node(index), "hash.hash", default=None)

            case CustomModelRole.PathRole.value:
                return self.node(index).path

            case _:
                return super().data(index, role)

import json
from pathlib import Path

from PyQt6.QtCore import QAbstractListModel, QObject, QModelIndex, Qt
from beartype import beartype
from beartype.typing import Literal, Annotated, Union
from pydantic import BaseModel, Field

from index_service.gui.common.qt_model_roles import CustomModelRole
from index_service.gui.file_tree.columns.file_tree_column import FileTreeNode
from index_service.services.pydantic_utils import model_from_json_data


class BaseAction(BaseModel, extra="forbid"):
    file: FileTreeNode


class TrashAction(BaseAction):
    kind: Literal["trash"] = "trash"


class MoveAction(BaseAction):
    kind: Literal["move"] = "move"
    dest: str


Action = Annotated[
    Union[MoveAction, TrashAction],
    Field(discriminator="kind"),
]


def load_actions(jsonl_path: Path) -> list[Action]:
    actions: list[Action] = []
    with jsonl_path.open("r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            data = json.loads(line)
            actions.append(model_from_json_data(data, Action))
    return actions


@beartype
class ActionProvider:

    def __init__(self) -> None:
        self.actions: list[BaseModel] = []

    def trash(self, file: FileTreeNode) -> None:
        self.actions.append(TrashAction(file=file))

    def move(self, file: FileTreeNode, dest: str) -> None:
        self.actions.append(MoveAction(file=file, dest=dest))


@beartype
class ActionListModel(QAbstractListModel):

    def __init__(self, actions: list[BaseAction], parent: QObject | None = None) -> None:
        super().__init__(parent)
        self._actions = actions

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self._actions)

    def data(self, index: QModelIndex,
             role: int = int(Qt.ItemDataRole.DisplayRole)) -> object:
        if not index.isValid():
            return None

        action = self._actions[index.row()]

        match role:
            case Qt.ItemDataRole.DisplayRole:
                match action:
                    case TrashAction():
                        return f"trash {action.file.path}"

                    case _:
                        return str(action)

            case CustomModelRole.HashRole.value:
                match action:
                    case TrashAction():
                        if action.file.hash:
                            result = action.file.hash.hash
                            return result

                        else:
                            return None

                    case _:
                        raise RuntimeError("Unknown action")

            case CustomModelRole.ActionRole.value:
                return action

        return None

    def roleNames(self) -> dict[int, bytes]:
        names = super().roleNames()
        names[self.ActionRole] = b"action"  # type: ignore
        return names  # type: ignore

    def actions(self) -> list[BaseAction]:
        return self._actions

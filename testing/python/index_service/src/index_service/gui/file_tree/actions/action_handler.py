from abc import ABC, abstractmethod

from index_service.gui.file_tree.actions.action_db import OperationRow
from index_service.gui.file_tree.actions.action_list_model import BaseAction


class ActionHandler(ABC):

    @abstractmethod
    def do_action(self, row: OperationRow, action: BaseAction) -> None:
        raise NotImplementedError

    @abstractmethod
    def undo_action(self, row: OperationRow, action: BaseAction) -> None:
        raise NotImplementedError

    @abstractmethod
    def get_hash(self, action: BaseAction) -> str:
        raise NotImplementedError

    @abstractmethod
    def verify_consistency_single(self, action: BaseAction) -> None:
        raise NotImplementedError

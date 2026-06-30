from typing import Protocol

from index_service.services.db import IndexDatabase
from index_service.services.types import FileHash
from PySide6.QtWidgets import QWidget
from abc import ABC, abstractmethod


class WidgetBuilder(ABC):

    asset_name: str

    @abstractmethod
    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        raise NotImplementedError()

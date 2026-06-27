from typing import Protocol

from index_service.services.db import IndexDatabase
from index_service.services.types import MD5
from PySide6.QtWidgets import QWidget
from abc import ABC, abstractmethod


class WidgetBuilder(ABC):

    asset_name: str

    @abstractmethod
    def build(self, db: IndexDatabase, md5: MD5) -> QWidget:
        raise NotImplementedError()

from typing import Protocol

from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer
from index_service.services.core.types import FileHash
from PySide6.QtWidgets import QWidget
from abc import ABC, abstractmethod


class WidgetBuilder(ABC):
    indexer: BaseIndexer

    def __init__(self, indexer: BaseIndexer) -> None:
        super().__init__()
        self.indexer = indexer

    @property
    def asset_name(self) -> str:
        return self.indexer.asset_name

    @abstractmethod
    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        raise NotImplementedError()

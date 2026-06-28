from __future__ import annotations
import logging
from typing import Any, Dict, List, Optional

from beartype import beartype
from PySide6.QtCore import (
    QAbstractListModel,
    QModelIndex,
    QObject,
    Qt,
)

from index_service.services.db import IndexDatabase
from index_service.services.types import MD5

log = logging.getLogger(__name__)


@beartype
class QueryResultModel(QAbstractListModel):
    Md5Role = Qt.ItemDataRole.UserRole + 1
    PathRole = Qt.ItemDataRole.UserRole + 2
    ExtraRole = Qt.ItemDataRole.UserRole + 3

    def __init__(self,
                 db: IndexDatabase,
                 batch_size: int,
                 parent: Optional[QObject] = None) -> None:
        super().__init__(parent)
        self._db = db
        self._cursor: Any = None
        self._rows: List[dict] = []
        self._paths: Dict[str, str] = {}
        self.batch_size = batch_size

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self._rows)

    def canFetchMore(self, parent: QModelIndex) -> bool:
        if parent.isValid():
            return False
        return self._cursor is not None

    def fetchMore(self, parent: QModelIndex) -> None:
        if parent.isValid() or self._cursor is None:
            return
        batch: List[dict] = []
        try:
            for _ in range(self.batch_size):
                batch.append(next(self._cursor))
        except StopIteration:
            self._cursor = None
        if not batch:
            return
        begin = len(self._rows)
        self.beginInsertRows(QModelIndex(), begin, begin + len(batch) - 1)
        self._rows.extend(batch)
        self.endInsertRows()

    def run_query(self, aql: str) -> int:
        self.beginResetModel()
        self._rows.clear()
        self._paths.clear()
        if self._cursor is not None:
            try:
                self._cursor.close()
            except Exception:
                pass
            self._cursor = None
        self._cursor = self._db._db.aql.execute(
            aql,
            batch_size=self.batch_size,
            count=True,
        )
        self.endResetModel()
        self.fetchMore(QModelIndex())
        return len(self._rows)

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        if not index.isValid():
            return None
        row = index.row()
        if row < 0 or row >= len(self._rows):
            return None
        doc = self._rows[row]
        md5 = doc.get("md5")
        if role == Qt.ItemDataRole.DisplayRole:
            return str(md5)
        if role == self.Md5Role:
            return md5

        if role == self.PathRole:
            if md5 not in self._paths:
                paths = [
                    self._db.get_path(p)
                    for p in self._db.get_all_refs(MD5(md5=md5))
                ]

                self._paths[md5] = str(paths[0]) if paths else ""

            return self._paths[md5]

        if role == self.ExtraRole:
            return {k: v for k, v in doc.items() if k != "md5"}
        return None

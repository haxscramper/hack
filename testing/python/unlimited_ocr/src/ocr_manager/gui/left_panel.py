import logging
from pathlib import Path
from typing import Optional, List, Any

from PyQt6.QtCore import Qt, QAbstractListModel
from PyQt6.QtWidgets import QWidget, QVBoxLayout, QListView, QLineEdit, QMenu
from PyQt6.QtGui import QAction
from PyQt6.QtCore import QSortFilterProxyModel
from sqlalchemy import select
from sqlalchemy.orm import sessionmaker

from src.ocr_manager.ocr_db import InputFileRecord


class PdfListModel(QAbstractListModel):

    def __init__(
        self,
        pdf_files: Optional[List[InputFileRecord]] = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.pdf_files: List[InputFileRecord] = pdf_files or []

    def rowCount(self, parent: Any = None) -> int:
        return len(self.pdf_files)

    def flags(self, index: Any):
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags
        return (Qt.ItemFlag.ItemIsEnabled
                | Qt.ItemFlag.ItemIsSelectable
                | Qt.ItemFlag.ItemIsUserCheckable)

    def data(self, index: Any, role: int = 0) -> Any:
        if not index.isValid() or not (0 <= index.row() < len(self.pdf_files)):
            return None

        record = self.pdf_files[index.row()]

        if role == Qt.ItemDataRole.DisplayRole:
            return Path(record.absolute_path).name
        elif role == Qt.ItemDataRole.ToolTipRole:
            return "Check to mark as OCR only"

        return None


class LeftPanel(QWidget):

    def __init__(
        self,
        session_factory: sessionmaker,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.session_factory = session_factory
        layout = QVBoxLayout(self)

        self.search_input = QLineEdit(self)
        self.search_input.setPlaceholderText("Search PDFs...")
        layout.addWidget(self.search_input)

        self.list_view = QListView(self)
        files = self._get_files_from_db()
        logging.info(f"LeftPanel: Found {len(files)} input files in DB.")
        self.model = PdfListModel(pdf_files=files)

        self.proxy_model = QSortFilterProxyModel(self)
        self.proxy_model.setSourceModel(self.model)
        self.proxy_model.setFilterCaseSensitivity(
            Qt.CaseSensitivity.CaseInsensitive)
        self.list_view.setModel(self.proxy_model)

        layout.addWidget(self.list_view)

        self.search_input.textChanged.connect(
            self.proxy_model.setFilterWildcard)

        self.list_view.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.list_view.customContextMenuRequested.connect(
            self._show_context_menu)

    def _show_context_menu(self, position):
        index = self.list_view.indexAt(position)
        if not index.isValid():
            return

        menu = QMenu()
        refresh_action = QAction("Refresh from DB", self)
        refresh_action.triggered.connect(self.refresh_files)
        menu.addAction(refresh_action)
        menu.exec_(self.list_view.viewport().mapToGlobal(position))

    def refresh_files(self):
        files = self._get_files_from_db()
        self.model.beginResetModel()
        self.model.pdf_files = files
        self.model.endResetModel()

    def _get_files_from_db(self) -> List[InputFileRecord]:
        if not self.session_factory:
            return []
        with self.session_factory() as session:
            records = list(
                session.scalars(
                    select(InputFileRecord).order_by(
                        InputFileRecord.absolute_path)))
            for r in records:
                session.expunge(r)
            return records

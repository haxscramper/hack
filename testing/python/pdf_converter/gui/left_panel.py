import logging
from pathlib import Path
from typing import Optional, List, Any

from PySide6.QtCore import Qt, QAbstractListModel, QModelIndex, QSortFilterProxyModel
from PySide6.QtWidgets import QWidget, QVBoxLayout, QListView, QLineEdit, QLabel

from config import AppConfig

class PdfListModel(QAbstractListModel):
    def __init__(self, pdf_files: Optional[List[Path]] = None, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.pdf_files: List[Path] = pdf_files or []

    def rowCount(self, parent: Optional[QModelIndex] = None) -> int:
        return len(self.pdf_files)

    def data(self, index: QModelIndex, role: int = 0) -> Any:
        if not index.isValid() or not (0 <= index.row() < len(self.pdf_files)):
            return None
            
        file_path = self.pdf_files[index.row()]
        
        if role == 0: # Qt.ItemDataRole.DisplayRole
            return file_path.name
        
        return None

class LeftPanel(QWidget):
    def __init__(self, config: Optional[AppConfig] = None, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.config = config
        layout = QVBoxLayout(self)
        
        # Search Box
        self.search_input = QLineEdit(self)
        self.search_input.setPlaceholderText("Search PDFs...")
        layout.addWidget(self.search_input)
        
        # List View
        self.list_view = QListView(self)
        files = self._get_files_from_config()
        logging.info(f"LeftPanel: Found {len(files)} PDFs in config.")
        self.model = PdfListModel(files)
        
        # Setup Filtering
        self.proxy_model = QSortFilterProxyModel(self)
        self.proxy_model.setSourceModel(self.model)
        self.proxy_model.setFilterCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)
        self.list_view.setModel(self.proxy_model)
        
        layout.addWidget(self.list_view)
        
        self.search_input.textChanged.connect(self.proxy_model.setFilterWildcard)

    def _get_files_from_config(self) -> List[Path]:
        pdf_files: List[Path] = []
        if self.config:
            for path in self.config.input_dirs:
                if path.is_dir():
                    pdf_files.extend(list(path.glob("*.pdf")))
                elif path.is_file() and path.suffix.lower() == '.pdf':
                    pdf_files.append(path)
        return pdf_files

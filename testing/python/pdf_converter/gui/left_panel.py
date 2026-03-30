from pathlib import Path
from PySide6.QtCore import Qt, QAbstractListModel, QModelIndex, QSortFilterProxyModel
from PySide6.QtWidgets import QWidget, QVBoxLayout, QListView, QLineEdit, QLabel

class PdfListModel(QAbstractListModel):
    def __init__(self, pdf_files=None, parent=None):
        super().__init__(parent)
        self.pdf_files = pdf_files or []

    def rowCount(self, parent=None):
        return len(self.pdf_files)

    def data(self, index, role=0):
        if not index.isValid() or not (0 <= index.row() < len(self.pdf_files)):
            return None
            
        file_path = self.pdf_files[index.row()]
        
        if role == 0: # DisplayRole
            return file_path.name
        
        return None

class LeftPanel(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        
        # Search Box
        self.search_input = QLineEdit(self)
        self.search_input.setPlaceholderText("Search PDFs...")
        layout.addWidget(self.search_input)
        
        # List View
        self.list_view = QListView(self)
        self.model = PdfListModel(self._get_dummy_files())
        
        # Setup Filtering
        self.proxy_model = QSortFilterProxyModel(self)
        self.proxy_model.setSourceModel(self.model)
        self.proxy_model.setFilterCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)
        self.list_view.setModel(self.proxy_model)
        
        layout.addWidget(self.list_view)
        
        self.search_input.textChanged.connect(self.proxy_model.setFilterWildcard)

    def _get_dummy_files(self):
        # Stub to get files; in a real scenario we'd query AppConfig
        return [Path("document1.pdf"), Path("another_doc.pdf")]

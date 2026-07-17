from PyQt6.QtPdf import QPdfDocument
from PyQt6.QtPdfWidgets import QPdfView
from PyQt6.QtWidgets import QWidget, QVBoxLayout
from beartype import beartype

from index_service.gui.collection_views.file_content_view.file_content_builder import FileContentViewBuilder


@beartype
class PdfFileContentViewBuilder(FileContentViewBuilder):

    def can_build(self, mime: str) -> bool:
        return mime == "application/pdf"

    def build(self, absolute_path: str) -> QWidget:
        container = QWidget()
        layout = QVBoxLayout(container)
        layout.setContentsMargins(0, 0, 0, 0)

        document = QPdfDocument(container)
        document.load(absolute_path)

        view = QPdfView(container)
        view.setDocument(document)
        layout.addWidget(view)

        container._pdf_document = document  # keep ref alive
        return container
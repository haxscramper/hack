from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.json_preview_widget import JsonPreviewWidget
from index_service.services.db import IndexDatabase
from index_service.services.types import FileHash
from PySide6.QtWidgets import QWidget


class JsonWidgetBuilder(WidgetBuilder):

    def __init__(self, collection_name: str) -> None:
        self._collection_name = collection_name

    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        doc = db._db.collection(self._collection_name).get(hash.hash)
        widget = JsonPreviewWidget()
        widget.set_doc(doc)
        return widget

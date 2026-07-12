import json

from arango import DocumentGetError

from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.json_preview_widget import JsonPreviewWidget
from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import FileHash
from PyQt6.QtWidgets import QWidget


class JsonWidgetBuilder(WidgetBuilder):

    def __init__(self, collection_name: str) -> None:
        self._collection_name = collection_name

    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        try:
            doc = db._db.collection(self._collection_name).get(hash.hash)

        except DocumentGetError as err:
            doc = str(json.dumps(dict(err=str(err))))

        widget = JsonPreviewWidget()
        widget.set_doc(doc)
        return widget

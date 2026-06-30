import json

from PySide6.QtWidgets import QAbstractItemView, QTableWidget, QTableWidgetItem, QWidget

from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.json_preview_widget import JsonPreviewWidget
from index_service.services.db import IndexDatabase
from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer, ComfyInputIndexerResult
from index_service.services.types import FileHash


class ComfyInputWidgetBuilder(WidgetBuilder):
    asset_name = ComfyInputIndexer.asset_name

    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        doc = db._db.collection("comfy_input").get(hash.hash)
        if doc is None:
            widget = JsonPreviewWidget()
            widget.set_doc(None)
            return widget
        result = ComfyInputIndexerResult.model_validate(doc["result"])
        table = QTableWidget(len(result.inputs), 2)
        table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        table.setHorizontalHeaderLabels(["node", "inputs"])
        for row, inp in enumerate(result.inputs):
            table.setItem(row, 0, QTableWidgetItem(inp.node))
            table.setItem(
                row, 1, QTableWidgetItem(json.dumps(inp.inputs, default=str)))
        table.horizontalHeader().setStretchLastSection(True)
        return table

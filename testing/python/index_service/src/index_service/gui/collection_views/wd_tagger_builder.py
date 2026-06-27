from PySide6.QtWidgets import QAbstractItemView, QTableWidget, QTableWidgetItem, QWidget

from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.json_preview_widget import JsonPreviewWidget
from index_service.services.db import IndexDatabase
from index_service.services.indexers.wd_indexer import WdTagIndexer
from index_service.services.resources.wd_tagger import WdTaggerResult
from index_service.services.types import MD5


class WdTaggerWidgetBuilder(WidgetBuilder):
    asset_name = WdTagIndexer.asset_name

    def build(self, db: IndexDatabase, md5: MD5) -> QWidget:
        doc = db._db.collection("wd_tags").get(md5.md5)
        if doc is None:
            widget = JsonPreviewWidget()
            widget.set_doc(None)
            return widget
        result = WdTaggerResult.model_validate(doc["result"])
        table = QTableWidget(len(result.tags), 3)
        table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        table.setHorizontalHeaderLabels(["category", "name", "probability"])
        for row, tag in enumerate(result.tags):
            table.setItem(row, 0, QTableWidgetItem(tag.category))
            table.setItem(row, 1, QTableWidgetItem(tag.name))
            table.setItem(row, 2, QTableWidgetItem(f"{tag.probability:.4f}"))
        table.horizontalHeader().setStretchLastSection(True)
        return table

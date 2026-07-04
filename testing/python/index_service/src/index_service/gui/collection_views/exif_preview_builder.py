import json
from pathlib import Path

from beartype.typing import Optional, cast
from PySide6.QtCore import Qt
from PySide6.QtGui import QPixmap
from PySide6.QtWidgets import (
    QAbstractItemView,
    QApplication,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QPushButton,
    QScrollArea,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.json_preview_widget import JsonPreviewWidget
from index_service.services.core.db import IndexDatabase
from index_service.services.indexers.exif_metadata import (
    ExifMetadataIndexer,
    ExifMetadataIndexerResult,
    ImageParams,
)
from index_service.services.pydantic_utils import model_to_json_data
from index_service.services.core.types import FileHash


class ExifPreviewrWidgetBuilder(WidgetBuilder):
    asset_name = ExifMetadataIndexer.asset_name

    def build(self, db: IndexDatabase, hash: FileHash) -> QWidget:
        result = cast(
            Optional[ExifMetadataIndexerResult],
            db.get_indexer_result(
                hash,
                ExifMetadataIndexer.asset_name,
                ExifMetadataIndexer.result_model,
            ),
        )

        if result is None:
            widget = JsonPreviewWidget()
            widget.set_doc(None)
            return widget

        elif not (result.file.parsed_prompt and result.file.parsed_negative_prompt):
            widget = JsonPreviewWidget()
            widget.set_doc(model_to_json_data(result))
            return widget

        else:
            params = result.file
            path = db.get_path(db.get_all_refs(hash)[0])
            return self._build_preview(params, path)

    def _build_preview(self, params: ImageParams, path: Path) -> QWidget:
        container = QWidget()
        outer = QVBoxLayout(container)
        outer.setContentsMargins(4, 4, 4, 4)
        outer.setSpacing(8)

        img_label = QLabel()
        pixmap = QPixmap(str(path.resolve()))
        if not pixmap.isNull():
            img_label.setPixmap(
                pixmap.scaledToWidth(300, Qt.TransformationMode.SmoothTransformation))

        img_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        outer.addWidget(img_label)

        outer.addWidget(
            QLabel("{}x{} {} on {}".format(
                *params.size,
                path.name,
                params.image_time,
            )))

        # --- table with data ---
        data_table = QTableWidget(4, 1)
        data_table.setHorizontalHeaderLabels(["value"])
        data_table.setVerticalHeaderLabels(
            ["PROMPT TEXT", "NEGATIVE PROMPT", "TAGS", "MODEL"])
        data_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        data_table.verticalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.ResizeToContents)

        for row, value in enumerate([
                params.prompt,
                params.negative_prompt,
                str(params.tags),
                params.model,
        ]):
            item = QTableWidgetItem(value)
            item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            data_table.setItem(row, 0, item)
        data_table.resizeRowsToContents()
        outer.addWidget(data_table)

        paste = json.dumps(params.get_tensor_art_prompt(), indent=2)
        button_row = QHBoxLayout()

        def copy_button(label: str, payload: str) -> QPushButton:
            btn = QPushButton(label)
            btn.clicked.connect(lambda: QApplication.clipboard().setText(payload))
            return btn

        button_row.addWidget(copy_button("Copy prompt", paste))
        button_row.addStretch(1)
        outer.addLayout(button_row)

        if params.generation_data:
            gen = QLabel(params.generation_data)
            gen.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
            gen.setWordWrap(True)
            outer.addWidget(gen)

        # --- loras table ---
        if params.loras:
            lora_table = QTableWidget(len(params.loras), 2)
            lora_table.setHorizontalHeaderLabels(["name", "weight"])
            lora_table.horizontalHeader().setSectionResizeMode(
                0, QHeaderView.ResizeMode.Stretch)
            lora_table.horizontalHeader().setSectionResizeMode(
                1, QHeaderView.ResizeMode.ResizeToContents)
            lora_table.verticalHeader().setVisible(False)
            for row, lora in enumerate(params.loras):
                name_item = QTableWidgetItem(lora.name)
                weight_item = QTableWidgetItem(str(lora.weight))
                for it in (name_item, weight_item):
                    it.setFlags(it.flags() & ~Qt.ItemFlag.ItemIsEditable)
                lora_table.setItem(row, 0, name_item)
                lora_table.setItem(row, 1, weight_item)
            lora_table.resizeRowsToContents()
            outer.addWidget(lora_table)

        outer.addStretch(1)

        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setWidget(container)
        return scroll

from __future__ import annotations

import logging
from typing import List, Optional, Sequence

from beartype import beartype
from PyQt6.QtCore import QSize, Qt
from PyQt6.QtGui import QPainter, QPen
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QListWidget,
    QListWidgetItem,
    QTabBar,
    QTabWidget,
    QVBoxLayout,
    QWidget,
)

from index_service.gui.collection_views.builder import WidgetBuilder
from index_service.gui.collection_views.json_preview_builder import JsonWidgetBuilder
from index_service.gui.collection_views.paths_preview_builder import PathsWidgetBuilder
from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)


class HorizontalTextTabBar(QTabBar):

    def tabSizeHint(self, index: int) -> QSize:
        font_metrics = self.fontMetrics()
        text = self.tabText(index)
        return QSize(
            font_metrics.horizontalAdvance(text) + 24,
            font_metrics.height() + 12,
        )

    def paintEvent(self, event) -> None:  # type: ignore[override]
        painter = QPainter(self)
        palette = self.palette()
        group = (palette.ColorGroup.Current
                 if self.isEnabled() else palette.ColorGroup.Disabled)

        for index in range(self.count()):
            rect = self.tabRect(index)
            selected = self.currentIndex() == index

            if selected:
                painter.fillRect(
                    rect,
                    palette.color(group, palette.ColorRole.Highlight),
                )
                text_color = palette.color(
                    group,
                    palette.ColorRole.HighlightedText,
                )
            else:
                painter.fillRect(
                    rect,
                    palette.color(group, palette.ColorRole.Window),
                )
                text_color = palette.color(
                    group,
                    palette.ColorRole.WindowText,
                )

            painter.setPen(QPen(palette.color(group, palette.ColorRole.Mid)))
            painter.drawRect(rect.adjusted(0, 0, -1, -1))

            painter.setPen(text_color)
            painter.drawText(
                rect,
                Qt.AlignmentFlag.AlignCenter,
                self.tabText(index),
            )


@beartype
class FilePreviewPane(QWidget):

    def __init__(
        self,
        db: IndexDatabase,
        collection_names: List[str],
        builders: Sequence[WidgetBuilder],
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)

        self._db = db
        self._collection_names = list(collection_names)
        self._widget_builders: dict[
            str, WidgetBuilder] = {  # type: ignore
                "paths": PathsWidgetBuilder(),
            }

        for name in self._collection_names:
            for builder in builders:
                if builder.asset_name == name:
                    self._widget_builders[name] = builder

            if name not in self._widget_builders:
                self._widget_builders[name] = JsonWidgetBuilder(name)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        self._tabs = QTabWidget()
        self._tabs.setTabBar(HorizontalTextTabBar())
        self._tabs.setTabPosition(QTabWidget.TabPosition.East)
        self._tabs.setMovable(False)
        self._tabs.currentChanged.connect(self._on_tab_changed)

        layout.addWidget(self._tabs)

    def show_hash(self, hash: FileHash) -> None:
        log.debug(f"Showing file hash {hash}")
        previous_index = self._tabs.currentIndex()
        self._tabs.clear()

        for name, builder in self._widget_builders.items():
            widget = builder.build(self._db, hash)
            self._tabs.addTab(widget, name)

        target_index = (previous_index if 0 <= previous_index < self._tabs.count() else 0)
        self._tabs.setCurrentIndex(target_index)

    def _on_tab_changed(self, index: int) -> None:
        if index >= 0:
            log.info("Tab changed to %d", index)

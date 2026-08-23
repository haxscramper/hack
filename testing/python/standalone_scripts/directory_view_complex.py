#!/usr/bin/env python

from __future__ import annotations

import argparse
import math
import os
import sys
from abc import ABC, abstractmethod
from collections import OrderedDict
from dataclasses import dataclass
from enum import Enum, auto
from typing import Hashable, TypeAlias

from PyQt6.QtCore import (
    QAbstractItemModel,
    QItemSelection,
    QItemSelectionModel,
    QModelIndex,
    QObject,
    QPoint,
    QRect,
    QRectF,
    QSize,
    QSizeF,
    QSortFilterProxyModel,
    QPersistentModelIndex,
    QTimer,
    Qt,
    pyqtSignal,
)
from PyQt6.QtGui import (
    QColor,
    QFileSystemModel,
    QImageReader,
    QPainter,
    QPalette,
    QPen,
    QPixmap,
    QPolygon,
    QRegion,
)
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
)

STABLE_ID_ROLE = int(Qt.ItemDataRole.UserRole) + 1
NODE_KIND_ROLE = int(Qt.ItemDataRole.UserRole) + 2

IMAGE_EXTENSIONS = {
    ".bmp",
    ".gif",
    ".ico",
    ".jpeg",
    ".jpg",
    ".png",
    ".tif",
    ".tiff",
    ".webp",
}

ItemId: TypeAlias = Hashable


class ItemKind(Enum):
    DIRECTORY = auto()
    FILE = auto()


@dataclass(frozen=True)
class LayoutConstraints:
    viewport_width: float
    zoom: float


@dataclass(frozen=True)
class TreeMetrics:
    content_margin: float = 8.0
    indentation: float = 18.0
    directory_height: float = 30.0
    directory_spacing: float = 4.0
    tile_width: float = 240.0
    tile_height: float = 120.0
    tile_spacing_x: float = 8.0
    tile_spacing_y: float = 8.0
    section_spacing: float = 10.0


@dataclass(frozen=True)
class LayoutItem:
    item_id: ItemId
    index: QPersistentModelIndex
    kind: ItemKind
    bounds: QRectF
    depth: int
    z_order: int = 0


@dataclass(frozen=True)
class DirectoryRunPlacement:
    parent: QPersistentModelIndex
    first_directory: int
    directory_count: int
    depth: int
    bounds: QRectF
    row_height: float
    row_spacing: float


@dataclass(frozen=True)
class FileGridPlacement:
    parent: QPersistentModelIndex
    first_file: int
    file_count: int
    depth: int
    bounds: QRectF
    columns: int
    tile_size: QSizeF
    spacing_x: float
    spacing_y: float


TreePlacement = DirectoryRunPlacement | FileGridPlacement


@dataclass
class RenderState:
    selected: bool
    current: bool
    hovered: bool
    expanded: bool


def persistent_to_index(index: QPersistentModelIndex) -> QModelIndex:
    return QModelIndex(index)


def scaled_rect(rect: QRectF) -> QRect:
    return rect.toAlignedRect()


def draw_triangle(
    painter: QPainter,
    box: QRect,
    expanded: bool,
    color: QColor,
) -> None:
    painter.save()
    painter.setPen(Qt.PenStyle.NoPen)
    painter.setBrush(color)

    cx = box.center().x()
    cy = box.center().y()

    if expanded:
        polygon = QPolygon([
            QPoint(cx - 4, cy - 2),
            QPoint(cx + 4, cy - 2),
            QPoint(cx, cy + 4),
        ])
    else:
        polygon = QPolygon([
            QPoint(cx - 2, cy - 4),
            QPoint(cx - 2, cy + 4),
            QPoint(cx + 4, cy),
        ])

    painter.drawPolygon(polygon)
    painter.restore()


def attribute_label(index: QModelIndex) -> str:
    model = index.model()
    header = model.headerData(
        index.column(),
        Qt.Orientation.Horizontal,
        Qt.ItemDataRole.DisplayRole,
    )
    value = index.data(Qt.ItemDataRole.DisplayRole)

    header_text = "" if header is None else str(header)
    value_text = "" if value is None else str(value)

    if not value_text:
        return ""

    if header_text:
        return f"{header_text}: {value_text}"

    return value_text


class FileSystemDataModel(QFileSystemModel):

    def data(
            self,
            index: QModelIndex,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ):
        if not index.isValid():
            return None

        column_zero = index.siblingAtColumn(0)
        info = self.fileInfo(column_zero)

        if role == STABLE_ID_ROLE:
            return os.path.normpath(os.path.abspath(info.absoluteFilePath()))

        if role == NODE_KIND_ROLE:
            if info.isDir():
                return ItemKind.DIRECTORY
            return ItemKind.FILE

        if role == int(Qt.ItemDataRole.DisplayRole):
            if index.column() == 2:
                created = info.birthTime()
                if created.isValid():
                    return created.toString("yyyy-MM-dd HH:mm:ss")
                return ""

            if index.column() == 3:
                modified = info.lastModified()
                if modified.isValid():
                    return modified.toString("yyyy-MM-dd HH:mm:ss")
                return ""

        if role == int(Qt.ItemDataRole.TextAlignmentRole):
            if index.column() == 1:
                return int(Qt.AlignmentFlag.AlignRight
                           | Qt.AlignmentFlag.AlignVCenter)

        return super().data(index, role)

    def headerData(
            self,
            section: int,
            orientation: Qt.Orientation,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ):
        if (orientation == Qt.Orientation.Horizontal
                and role == int(Qt.ItemDataRole.DisplayRole)):
            headers = ("Name", "Size", "Created", "Modified")
            if 0 <= section < len(headers):
                return headers[section]

        return super().headerData(section, orientation, role)


class DirectoryFirstProxyModel(QSortFilterProxyModel):

    def __init__(self, parent: QObject | None = None) -> None:
        super().__init__(parent)
        self.setDynamicSortFilter(True)
        self.setRecursiveFilteringEnabled(True)

    def lessThan(
        self,
        left: QModelIndex,
        right: QModelIndex,
    ) -> bool:
        left_kind = left.data(NODE_KIND_ROLE)
        right_kind = right.data(NODE_KIND_ROLE)

        if left_kind != right_kind:
            return left_kind == ItemKind.DIRECTORY

        left_text = str(left.data(Qt.ItemDataRole.DisplayRole) or "")
        right_text = str(right.data(Qt.ItemDataRole.DisplayRole) or "")

        return left_text.casefold() < right_text.casefold()


class PresentationState(QObject):
    expansionChanged = pyqtSignal(object, bool)
    reset = pyqtSignal()

    def __init__(self, parent: QObject | None = None) -> None:
        super().__init__(parent)
        self._expanded: set[ItemId] = set()

    def is_expanded(self, item_id: ItemId) -> bool:
        return item_id in self._expanded

    def set_expanded(
        self,
        item_id: ItemId,
        expanded: bool,
    ) -> None:
        if expanded:
            if item_id in self._expanded:
                return
            self._expanded.add(item_id)
        else:
            if item_id not in self._expanded:
                return
            self._expanded.remove(item_id)

        self.expansionChanged.emit(item_id, expanded)

    def toggle(self, item_id: ItemId) -> bool:
        expanded = not self.is_expanded(item_id)
        self.set_expanded(item_id, expanded)
        return expanded

    def expanded_ids(self) -> frozenset[ItemId]:
        return frozenset(self._expanded)

    def clear(self) -> None:
        if not self._expanded:
            return

        self._expanded.clear()
        self.reset.emit()


class TreePresentationAdapter(QObject):
    structureChanged = pyqtSignal()
    contentChanged = pyqtSignal()
    reset = pyqtSignal()

    def __init__(
        self,
        model: QAbstractItemModel,
        source_model: FileSystemDataModel,
        proxy_model: DirectoryFirstProxyModel,
        root: QModelIndex,
        parent: QObject | None = None,
    ) -> None:
        super().__init__(parent)

        self._model = model
        self._source_model = source_model
        self._proxy_model = proxy_model
        self._root = QPersistentModelIndex(root)

        model.rowsInserted.connect(self._emit_structure_changed)
        model.rowsRemoved.connect(self._emit_structure_changed)
        model.rowsMoved.connect(self._emit_structure_changed)
        model.layoutChanged.connect(self._emit_structure_changed)
        model.modelReset.connect(self._emit_reset)
        model.dataChanged.connect(self._emit_content_changed)

    @property
    def model(self) -> QAbstractItemModel:
        return self._model

    @property
    def root(self) -> QModelIndex:
        return persistent_to_index(self._root)

    def set_root(self, root: QModelIndex) -> None:
        self._root = QPersistentModelIndex(root)
        self.structureChanged.emit()

    def _emit_structure_changed(self, *args) -> None:
        self.structureChanged.emit()

    def _emit_content_changed(self, *args) -> None:
        self.contentChanged.emit()

    def _emit_reset(self, *args) -> None:
        self.reset.emit()

    def item_id(self, index: QModelIndex) -> ItemId:
        value = index.data(STABLE_ID_ROLE)
        if value is None:
            raise ValueError("Model index does not provide a stable ID")
        return value

    def kind(self, index: QModelIndex) -> ItemKind:
        value = index.data(NODE_KIND_ROLE)
        if not isinstance(value, ItemKind):
            raise ValueError("Model index does not provide its node kind")
        return value

    def index_for_id(self, item_id: ItemId) -> QModelIndex:
        source_index = self._source_model.index(str(item_id))
        if not source_index.isValid():
            return QModelIndex()

        return self._proxy_model.mapFromSource(source_index)

    def child_count(self, parent: QModelIndex) -> int:
        return self._model.rowCount(parent)

    def directory_count(self, parent: QModelIndex) -> int:
        low = 0
        high = self.child_count(parent)

        while low < high:
            middle = (low + high) // 2
            index = self._model.index(middle, 0, parent)

            if self.kind(index) == ItemKind.DIRECTORY:
                low = middle + 1
            else:
                high = middle

        return low

    def file_count(self, parent: QModelIndex) -> int:
        return self.child_count(parent) - self.directory_count(parent)

    def directory_index(
        self,
        parent: QModelIndex,
        position: int,
    ) -> QModelIndex:
        return self._model.index(position, 0, parent)

    def file_index(
        self,
        parent: QModelIndex,
        position: int,
    ) -> QModelIndex:
        row = self.directory_count(parent) + position
        return self._model.index(row, 0, parent)

    def fetch_more(self, index: QModelIndex) -> None:
        if self._model.canFetchMore(index):
            self._model.fetchMore(index)


class LayoutSnapshot(ABC):

    @property
    @abstractmethod
    def content_bounds(self) -> QRectF:
        raise NotImplementedError

    @abstractmethod
    def items_in_rect(self, rect: QRectF) -> list[LayoutItem]:
        raise NotImplementedError

    @abstractmethod
    def item_at(self, x: float, y: float) -> LayoutItem | None:
        raise NotImplementedError

    @abstractmethod
    def bounds_for_id(self, item_id: ItemId) -> QRectF | None:
        raise NotImplementedError


class TreeLayoutSnapshot(LayoutSnapshot):

    def __init__(
        self,
        adapter: TreePresentationAdapter,
        placements: tuple[TreePlacement, ...],
        content_bounds: QRectF,
    ) -> None:
        self._adapter = adapter
        self._placements = placements
        self._content_bounds = QRectF(content_bounds)

    @property
    def content_bounds(self) -> QRectF:
        return QRectF(self._content_bounds)

    def _layout_item(
        self,
        index: QModelIndex,
        kind: ItemKind,
        bounds: QRectF,
        depth: int,
    ) -> LayoutItem:
        return LayoutItem(
            item_id=self._adapter.item_id(index),
            index=QPersistentModelIndex(index),
            kind=kind,
            bounds=QRectF(bounds),
            depth=depth,
        )

    def _directory_bounds(
        self,
        placement: DirectoryRunPlacement,
        logical_position: int,
    ) -> QRectF:
        offset = logical_position - placement.first_directory
        y = placement.bounds.top() + offset * (placement.row_height +
                                               placement.row_spacing)

        return QRectF(
            placement.bounds.left(),
            y,
            placement.bounds.width(),
            placement.row_height,
        )

    def _file_bounds(
        self,
        placement: FileGridPlacement,
        logical_position: int,
    ) -> QRectF:
        offset = logical_position - placement.first_file
        grid_row, grid_column = divmod(offset, placement.columns)

        x = placement.bounds.left() + grid_column * (
            placement.tile_size.width() + placement.spacing_x)
        y = placement.bounds.top() + grid_row * (placement.tile_size.height() +
                                                 placement.spacing_y)

        return QRectF(
            x,
            y,
            placement.tile_size.width(),
            placement.tile_size.height(),
        )

    def _directory_items(
        self,
        placement: DirectoryRunPlacement,
        query: QRectF,
    ) -> list[LayoutItem]:
        if not placement.bounds.intersects(query):
            return []

        pitch = placement.row_height + placement.row_spacing
        relative_top = max(0.0, query.top() - placement.bounds.top())
        relative_bottom = min(
            placement.bounds.height(),
            query.bottom() - placement.bounds.top(),
        )

        first_offset = max(0, math.floor(relative_top / pitch))
        last_offset = min(
            placement.directory_count - 1,
            math.floor(max(0.0, relative_bottom) / pitch),
        )

        parent = persistent_to_index(placement.parent)
        result: list[LayoutItem] = []

        for offset in range(first_offset, last_offset + 1):
            logical_position = placement.first_directory + offset
            bounds = self._directory_bounds(placement, logical_position)

            if not bounds.intersects(query):
                continue

            index = self._adapter.directory_index(
                parent,
                logical_position,
            )

            if index.isValid():
                result.append(
                    self._layout_item(
                        index,
                        ItemKind.DIRECTORY,
                        bounds,
                        placement.depth,
                    ))

        return result

    def _file_items(
        self,
        placement: FileGridPlacement,
        query: QRectF,
    ) -> list[LayoutItem]:
        if not placement.bounds.intersects(query):
            return []

        row_pitch = placement.tile_size.height() + placement.spacing_y

        relative_top = max(0.0, query.top() - placement.bounds.top())
        relative_bottom = min(
            placement.bounds.height(),
            query.bottom() - placement.bounds.top(),
        )

        first_grid_row = max(0, math.floor(relative_top / row_pitch))
        last_grid_row = max(0,
                            math.floor(max(0.0, relative_bottom) / row_pitch))

        parent = persistent_to_index(placement.parent)
        result: list[LayoutItem] = []

        for grid_row in range(first_grid_row, last_grid_row + 1):
            first_offset = grid_row * placement.columns
            last_offset = min(
                placement.file_count - 1,
                first_offset + placement.columns - 1,
            )

            for offset in range(first_offset, last_offset + 1):
                logical_position = placement.first_file + offset
                bounds = self._file_bounds(placement, logical_position)

                if not bounds.intersects(query):
                    continue

                index = self._adapter.file_index(
                    parent,
                    logical_position,
                )

                if index.isValid():
                    result.append(
                        self._layout_item(
                            index,
                            ItemKind.FILE,
                            bounds,
                            placement.depth,
                        ))

        return result

    def items_in_rect(self, rect: QRectF) -> list[LayoutItem]:
        result: list[LayoutItem] = []

        for placement in self._placements:
            if not placement.bounds.intersects(rect):
                continue

            if isinstance(placement, DirectoryRunPlacement):
                result.extend(self._directory_items(placement, rect))
            else:
                result.extend(self._file_items(placement, rect))

        result.sort(key=lambda item: item.z_order)
        return result

    def item_at(self, x: float, y: float) -> LayoutItem | None:
        point_rect = QRectF(x, y, 0.001, 0.001)
        items = self.items_in_rect(point_rect)

        for item in reversed(items):
            if item.bounds.contains(x, y):
                return item

        return None

    def bounds_for_id(self, item_id: ItemId) -> QRectF | None:
        index = self._adapter.index_for_id(item_id)
        if not index.isValid():
            return None

        parent = index.parent()
        kind = self._adapter.kind(index)

        for placement in self._placements:
            placement_parent = persistent_to_index(placement.parent)

            if placement_parent != parent:
                continue

            if (kind == ItemKind.DIRECTORY
                    and isinstance(placement, DirectoryRunPlacement)):
                logical_position = index.row()
                first = placement.first_directory
                end = first + placement.directory_count

                if first <= logical_position < end:
                    return self._directory_bounds(
                        placement,
                        logical_position,
                    )

            if (kind == ItemKind.FILE
                    and isinstance(placement, FileGridPlacement)):
                directory_count = self._adapter.directory_count(parent)
                logical_position = index.row() - directory_count
                first = placement.first_file
                end = first + placement.file_count

                if first <= logical_position < end:
                    return self._file_bounds(
                        placement,
                        logical_position,
                    )

        return None


class TreeLayoutEngine:

    def __init__(
        self,
        adapter: TreePresentationAdapter,
        state: PresentationState,
        metrics: TreeMetrics,
    ) -> None:
        self._adapter = adapter
        self._state = state
        self._metrics = metrics
        self._constraints = LayoutConstraints(1.0, 1.0)
        self._placements: list[TreePlacement] = []

    def _scale(self, value: float) -> float:
        return value * self._constraints.zoom

    def _expanded_rows(
        self,
        parent: QModelIndex,
        directory_count: int,
    ) -> list[int]:
        rows: list[int] = []

        for item_id in self._state.expanded_ids():
            index = self._adapter.index_for_id(item_id)

            if not index.isValid():
                continue

            if index.parent() != parent:
                continue

            if self._adapter.kind(index) != ItemKind.DIRECTORY:
                continue

            if 0 <= index.row() < directory_count:
                rows.append(index.row())

        rows.sort()
        return rows

    def _append_directory_run(
        self,
        parent: QModelIndex,
        first: int,
        count: int,
        depth: int,
        x: float,
        y: float,
        width: float,
    ) -> float:
        if count <= 0:
            return y

        row_height = self._scale(self._metrics.directory_height)
        row_spacing = self._scale(self._metrics.directory_spacing)
        visible_height = count * row_height + max(0, count - 1) * row_spacing

        self._placements.append(
            DirectoryRunPlacement(
                parent=QPersistentModelIndex(parent),
                first_directory=first,
                directory_count=count,
                depth=depth,
                bounds=QRectF(x, y, width, visible_height),
                row_height=row_height,
                row_spacing=row_spacing,
            ))

        return y + count * (row_height + row_spacing)

    def _layout_parent(
        self,
        parent: QModelIndex,
        depth: int,
        y: float,
    ) -> float:
        viewport_width = max(1.0, self._constraints.viewport_width)
        margin = self._scale(self._metrics.content_margin)
        indentation = self._scale(self._metrics.indentation)

        x = margin + depth * indentation
        available_width = max(100.0, viewport_width - margin - x)

        directory_count = self._adapter.directory_count(parent)
        expanded_rows = self._expanded_rows(parent, directory_count)

        cursor = 0

        for expanded_row in expanded_rows:
            y = self._append_directory_run(
                parent,
                cursor,
                expanded_row - cursor,
                depth,
                x,
                y,
                available_width,
            )

            y = self._append_directory_run(
                parent,
                expanded_row,
                1,
                depth,
                x,
                y,
                available_width,
            )

            expanded_index = self._adapter.directory_index(
                parent,
                expanded_row,
            )
            self._adapter.fetch_more(expanded_index)

            y = self._layout_parent(
                expanded_index,
                depth + 1,
                y,
            )
            cursor = expanded_row + 1

        y = self._append_directory_run(
            parent,
            cursor,
            directory_count - cursor,
            depth,
            x,
            y,
            available_width,
        )

        file_count = self._adapter.file_count(parent)

        if file_count > 0:
            tile_width = self._scale(self._metrics.tile_width)
            tile_height = self._scale(self._metrics.tile_height)
            spacing_x = self._scale(self._metrics.tile_spacing_x)
            spacing_y = self._scale(self._metrics.tile_spacing_y)

            columns = max(
                1,
                int((available_width + spacing_x) // (tile_width + spacing_x)),
            )
            grid_rows = math.ceil(file_count / columns)
            grid_height = (grid_rows * tile_height +
                           max(0, grid_rows - 1) * spacing_y)

            self._placements.append(
                FileGridPlacement(
                    parent=QPersistentModelIndex(parent),
                    first_file=0,
                    file_count=file_count,
                    depth=depth,
                    bounds=QRectF(
                        x,
                        y,
                        available_width,
                        grid_height,
                    ),
                    columns=columns,
                    tile_size=QSizeF(tile_width, tile_height),
                    spacing_x=spacing_x,
                    spacing_y=spacing_y,
                ))

            y += grid_height
            y += self._scale(self._metrics.section_spacing)

        return y

    def build(
        self,
        constraints: LayoutConstraints,
    ) -> TreeLayoutSnapshot:
        self._constraints = constraints
        self._placements = []

        root = self._adapter.root
        margin = self._scale(self._metrics.content_margin)
        y = self._layout_parent(root, 0, margin)
        content_height = y + margin

        return TreeLayoutSnapshot(
            adapter=self._adapter,
            placements=tuple(self._placements),
            content_bounds=QRectF(
                0.0,
                0.0,
                max(1.0, constraints.viewport_width),
                content_height,
            ),
        )


class LayoutController(QObject):
    snapshotChanged = pyqtSignal(object)

    def __init__(
        self,
        engine: TreeLayoutEngine,
        parent: QObject | None = None,
    ) -> None:
        super().__init__(parent)

        self._engine = engine
        self._constraints = LayoutConstraints(1.0, 1.0)
        self._snapshot: LayoutSnapshot | None = None
        self._dirty = True

        self._timer = QTimer(self)
        self._timer.setSingleShot(True)
        self._timer.timeout.connect(self.rebuild)

    @property
    def snapshot(self) -> LayoutSnapshot | None:
        self.ensure_snapshot()
        return self._snapshot

    def set_constraints(
        self,
        constraints: LayoutConstraints,
    ) -> None:
        if constraints == self._constraints:
            return

        self._constraints = constraints
        self.invalidate()

    def invalidate(self) -> None:
        self._dirty = True

        if not self._timer.isActive():
            self._timer.start(0)

    def ensure_snapshot(self) -> None:
        if self._dirty:
            self.rebuild()

    def rebuild(self) -> None:
        if not self._dirty:
            return

        self._snapshot = self._engine.build(self._constraints)
        self._dirty = False
        self.snapshotChanged.emit(self._snapshot)


class TileDelegate(ABC):

    def __init__(self) -> None:
        self.request_update = lambda: None

    @abstractmethod
    def paint(
        self,
        painter: QPainter,
        primary: QModelIndex,
        area: QSize,
        attributes: list[QModelIndex],
        state: RenderState,
    ) -> None:
        raise NotImplementedError


class DirectoryDelegate(TileDelegate):

    def paint(
        self,
        painter: QPainter,
        primary: QModelIndex,
        area: QSize,
        attributes: list[QModelIndex],
        state: RenderState,
    ) -> None:
        palette = QApplication.palette()
        rect = QRect(QPoint(0, 0), area)

        painter.save()

        background = palette.color(QPalette.ColorRole.AlternateBase)

        if state.selected or state.current:
            background = palette.color(
                QPalette.ColorRole.Highlight).lighter(170)
        elif state.hovered:
            background = palette.color(
                QPalette.ColorRole.AlternateBase).lighter(110)

        painter.fillRect(rect, background)
        painter.setPen(QPen(palette.color(QPalette.ColorRole.Mid)))
        painter.drawLine(rect.bottomLeft(), rect.bottomRight())

        toggle = QRect(
            2,
            (area.height() - 16) // 2,
            16,
            16,
        )
        draw_triangle(
            painter,
            toggle,
            state.expanded,
            palette.color(QPalette.ColorRole.Text),
        )

        painter.setPen(palette.color(QPalette.ColorRole.Text))

        font = painter.font()
        font.setBold(True)
        painter.setFont(font)

        name = str(primary.data(Qt.ItemDataRole.DisplayRole) or "")
        labels = [attribute_label(attribute) for attribute in attributes]
        labels = [label for label in labels if label]

        text = name
        if labels:
            text += "      " + "   ".join(labels)

        text_rect = QRect(
            toggle.right() + 6,
            0,
            max(1,
                area.width() - toggle.right() - 10),
            area.height(),
        )

        painter.drawText(
            text_rect,
            Qt.AlignmentFlag.AlignVCenter
            | Qt.AlignmentFlag.AlignLeft,
            text,
        )

        painter.restore()


class FileTileDelegate(TileDelegate):

    def __init__(self, cache_limit: int = 256) -> None:
        super().__init__()
        self._cache_limit = cache_limit
        self._cache: OrderedDict[str, QPixmap | None] = OrderedDict()

    def _thumbnail(
        self,
        index: QModelIndex,
        target: QSize,
    ) -> QPixmap | None:
        path = str(index.data(STABLE_ID_ROLE) or "")
        extension = os.path.splitext(path)[1].lower()

        if extension not in IMAGE_EXTENSIONS:
            return None

        key = f"{path}:{target.width()}x{target.height()}"

        if key in self._cache:
            pixmap = self._cache.pop(key)
            self._cache[key] = pixmap
            return pixmap

        reader = QImageReader(path)
        reader.setAutoTransform(True)

        original_size = reader.size()
        if original_size.isValid():
            scaled_size = original_size.scaled(
                target,
                Qt.AspectRatioMode.KeepAspectRatio,
            )
            reader.setScaledSize(scaled_size)

        image = reader.read()
        pixmap = None if image.isNull() else QPixmap.fromImage(image)

        self._cache[key] = pixmap

        while len(self._cache) > self._cache_limit:
            self._cache.popitem(last=False)

        return pixmap

    def paint(
        self,
        painter: QPainter,
        primary: QModelIndex,
        area: QSize,
        attributes: list[QModelIndex],
        state: RenderState,
    ) -> None:
        palette = QApplication.palette()
        rect = QRect(QPoint(0, 0), area)

        painter.save()

        background = palette.color(QPalette.ColorRole.Base)

        if state.selected or state.current:
            background = palette.color(
                QPalette.ColorRole.Highlight).lighter(175)
        elif state.hovered:
            background = palette.color(QPalette.ColorRole.AlternateBase)

        painter.fillRect(rect, background)

        border_color = palette.color(QPalette.ColorRole.Mid)

        if state.current:
            border_color = palette.color(QPalette.ColorRole.Highlight)

        painter.setPen(QPen(border_color))
        painter.drawRect(rect.adjusted(0, 0, -1, -1))

        padding = max(4, round(area.height() * 0.05))
        preview_height = max(24, round(area.height() * 0.58))

        preview_rect = QRect(
            padding,
            padding,
            max(1,
                area.width() - 2 * padding),
            max(1, preview_height - padding),
        )

        preview_background = palette.color(QPalette.ColorRole.AlternateBase)
        painter.fillRect(preview_rect, preview_background)

        thumbnail = self._thumbnail(
            primary,
            preview_rect.size(),
        )

        if thumbnail is not None:
            target = QRect(QPoint(), thumbnail.size())
            target.moveCenter(preview_rect.center())
            painter.drawPixmap(target.topLeft(), thumbnail)
        else:
            extension = os.path.splitext(
                str(primary.data(Qt.ItemDataRole.DisplayRole) or ""))[1]
            extension_text = extension[1:].upper() if extension else "FILE"

            painter.setPen(palette.color(QPalette.ColorRole.PlaceholderText))
            painter.drawText(
                preview_rect,
                Qt.AlignmentFlag.AlignCenter,
                extension_text,
            )

        name = str(primary.data(Qt.ItemDataRole.DisplayRole) or "")
        labels = [attribute_label(attribute) for attribute in attributes]
        labels = [label for label in labels if label]

        text_rect = QRect(
            padding,
            preview_rect.bottom() + padding,
            max(1,
                area.width() - 2 * padding),
            max(1,
                area.height() - preview_rect.bottom() - 2 * padding),
        )

        painter.setPen(palette.color(QPalette.ColorRole.Text))

        font = painter.font()
        font.setBold(True)
        painter.setFont(font)

        name_rect = QRect(
            text_rect.left(),
            text_rect.top(),
            text_rect.width(),
            painter.fontMetrics().height(),
        )
        elided_name = painter.fontMetrics().elidedText(
            name,
            Qt.TextElideMode.ElideMiddle,
            name_rect.width(),
        )
        painter.drawText(
            name_rect,
            Qt.AlignmentFlag.AlignLeft
            | Qt.AlignmentFlag.AlignVCenter,
            elided_name,
        )

        font.setBold(False)
        font.setPointSizeF(max(7.0, font.pointSizeF() * 0.82))
        painter.setFont(font)
        painter.setPen(palette.color(QPalette.ColorRole.PlaceholderText))

        details_rect = QRect(
            text_rect.left(),
            name_rect.bottom() + 2,
            text_rect.width(),
            max(1,
                text_rect.bottom() - name_rect.bottom() - 2),
        )

        details = "\n".join(labels)
        painter.drawText(
            details_rect,
            Qt.AlignmentFlag.AlignLeft
            | Qt.AlignmentFlag.AlignTop,
            details,
        )

        painter.restore()


class MixedTreeTileView(QAbstractItemView):
    expanded = pyqtSignal(QModelIndex)
    collapsed = pyqtSignal(QModelIndex)

    def __init__(
        self,
        adapter: TreePresentationAdapter,
        state: PresentationState,
        controller: LayoutController,
        parent: QObject | None = None,
    ) -> None:
        super().__init__(parent)

        self._adapter = adapter
        self._state = state
        self._controller = controller

        self._zoom_factor = 1.0
        self._hovered_id: ItemId | None = None

        self._directory_delegate = DirectoryDelegate()
        self._file_delegate = FileTileDelegate()

        self._directory_delegate.request_update = self.viewport().update
        self._file_delegate.request_update = self.viewport().update

        self.setModel(adapter.model)
        super().setRootIndex(adapter.root)

        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)

        self.horizontalScrollBar().setRange(0, 0)

        self._adapter.structureChanged.connect(self._controller.invalidate)
        self._adapter.structureChanged.connect(self._update_constraints)
        self._adapter.contentChanged.connect(self.viewport().update)
        self._adapter.reset.connect(self._handle_model_reset)

        self._state.expansionChanged.connect(self._handle_expansion_changed)
        self._state.reset.connect(self._controller.invalidate)

        self._controller.snapshotChanged.connect(self._handle_snapshot_changed)

        selection_model = self.selectionModel()
        selection_model.selectionChanged.connect(self._update_viewport)
        selection_model.currentChanged.connect(self._update_viewport)

        self._update_constraints()

    def _update_viewport(self, *args) -> None:
        self.viewport().update()

    def _handle_model_reset(self) -> None:
        self._hovered_id = None
        self._controller.invalidate()
        self.viewport().update()

    def _handle_expansion_changed(
        self,
        item_id: ItemId,
        expanded: bool,
    ) -> None:
        index = self._adapter.index_for_id(item_id)

        if expanded and index.isValid():
            self._adapter.fetch_more(index)
            self.expanded.emit(index)
        elif index.isValid():
            self.collapsed.emit(index)

        self._controller.invalidate()

    def _handle_snapshot_changed(
        self,
        snapshot: LayoutSnapshot,
    ) -> None:
        page = max(1, self.viewport().height())
        content_height = math.ceil(snapshot.content_bounds.height())

        scroll_bar = self.verticalScrollBar()
        scroll_bar.setPageStep(page)
        scroll_bar.setRange(
            0,
            max(0, content_height - page),
        )

        self.viewport().update()

    def _update_constraints(self) -> None:
        self._controller.set_constraints(
            LayoutConstraints(
                viewport_width=max(1.0, float(self.viewport().width())),
                zoom=self._zoom_factor,
            ))

    def _snapshot(self) -> LayoutSnapshot | None:
        return self._controller.snapshot

    def _content_point(self, point: QPoint) -> QPoint:
        return QPoint(
            point.x() + self.horizontalOffset(),
            point.y() + self.verticalOffset(),
        )

    def _visible_content_rect(self) -> QRectF:
        return QRectF(
            float(self.horizontalOffset()),
            float(self.verticalOffset()),
            float(self.viewport().width()),
            float(self.viewport().height()),
        )

    def _attributes(
        self,
        primary: QModelIndex,
    ) -> list[QModelIndex]:
        model = self.model()
        parent = primary.parent()

        return [
            model.index(primary.row(), column, parent)
            for column in range(1, model.columnCount(parent))
        ]

    def paintEvent(self, event) -> None:
        painter = QPainter(self.viewport())
        palette = self.palette()

        painter.fillRect(
            self.viewport().rect(),
            palette.color(QPalette.ColorRole.Base),
        )

        snapshot = self._snapshot()
        if snapshot is None:
            return

        visible = self._visible_content_rect()
        items = snapshot.items_in_rect(visible)

        for item in items:
            index = persistent_to_index(item.index)
            if not index.isValid():
                continue

            bounds = item.bounds.translated(
                -self.horizontalOffset(),
                -self.verticalOffset(),
            )
            viewport_rect = bounds.toAlignedRect()

            if not viewport_rect.intersects(event.rect()):
                continue

            state = RenderState(
                selected=self.selectionModel().isSelected(index),
                current=index == self.currentIndex(),
                hovered=item.item_id == self._hovered_id,
                expanded=self._state.is_expanded(item.item_id),
            )

            if item.kind == ItemKind.DIRECTORY:
                delegate = self._directory_delegate
            else:
                delegate = self._file_delegate

            painter.save()
            painter.translate(viewport_rect.topLeft())
            delegate.paint(
                painter,
                index,
                viewport_rect.size(),
                self._attributes(index),
                state,
            )
            painter.restore()

    def resizeEvent(self, event) -> None:
        super().resizeEvent(event)
        self._update_constraints()

    def scrollContentsBy(self, dx: int, dy: int) -> None:
        self.viewport().update()

    def wheelEvent(self, event) -> None:
        if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            if event.angleDelta().y() > 0:
                self._zoom_factor = min(
                    20.0,
                    self._zoom_factor * 1.15,
                )
            else:
                self._zoom_factor = max(
                    0.2,
                    self._zoom_factor / 1.15,
                )

            self._update_constraints()
            event.accept()
            return

        super().wheelEvent(event)

    def mousePressEvent(self, event) -> None:
        index = self.indexAt(event.position().toPoint())

        if (index.isValid() and self._adapter.kind(index) == ItemKind.DIRECTORY
                and event.button() == Qt.MouseButton.LeftButton):
            item_id = self._adapter.item_id(index)
            self._state.toggle(item_id)

            if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
                command = QItemSelectionModel.SelectionFlag.Toggle
            else:
                command = (QItemSelectionModel.SelectionFlag.ClearAndSelect
                           | QItemSelectionModel.SelectionFlag.Rows)

            self.selectionModel().setCurrentIndex(index, command)
            event.accept()
            return

        super().mousePressEvent(event)

    def mouseMoveEvent(self, event) -> None:
        index = self.indexAt(event.position().toPoint())
        new_hovered = None

        if index.isValid():
            new_hovered = self._adapter.item_id(index)

        if new_hovered != self._hovered_id:
            self._hovered_id = new_hovered
            self.viewport().update()

        super().mouseMoveEvent(event)

    def leaveEvent(self, event) -> None:
        if self._hovered_id is not None:
            self._hovered_id = None
            self.viewport().update()

        super().leaveEvent(event)

    def visualRect(self, index: QModelIndex) -> QRect:
        if not index.isValid():
            return QRect()

        snapshot = self._snapshot()
        if snapshot is None:
            return QRect()

        item_id = self._adapter.item_id(index)
        bounds = snapshot.bounds_for_id(item_id)

        if bounds is None:
            return QRect()

        return bounds.translated(
            -self.horizontalOffset(),
            -self.verticalOffset(),
        ).toAlignedRect()

    def indexAt(self, point: QPoint) -> QModelIndex:
        snapshot = self._snapshot()
        if snapshot is None:
            return QModelIndex()

        content_point = self._content_point(point)
        item = snapshot.item_at(
            float(content_point.x()),
            float(content_point.y()),
        )

        if item is None:
            return QModelIndex()

        return persistent_to_index(item.index)

    def scrollTo(
        self,
        index: QModelIndex,
        hint: QAbstractItemView.ScrollHint = (
            QAbstractItemView.ScrollHint.EnsureVisible),
    ) -> None:
        if not index.isValid():
            return

        snapshot = self._snapshot()
        if snapshot is None:
            return

        bounds = snapshot.bounds_for_id(self._adapter.item_id(index))
        if bounds is None:
            return

        scroll_bar = self.verticalScrollBar()
        viewport_height = self.viewport().height()

        if hint == QAbstractItemView.ScrollHint.PositionAtTop:
            scroll_bar.setValue(round(bounds.top()))
            return

        if hint == QAbstractItemView.ScrollHint.PositionAtCenter:
            scroll_bar.setValue(
                round(bounds.center().y() - viewport_height / 2))
            return

        if hint == QAbstractItemView.ScrollHint.PositionAtBottom:
            scroll_bar.setValue(round(bounds.bottom() - viewport_height))
            return

        if bounds.top() < scroll_bar.value():
            scroll_bar.setValue(round(bounds.top()))
        elif bounds.bottom() > scroll_bar.value() + viewport_height:
            scroll_bar.setValue(round(bounds.bottom() - viewport_height))

    def horizontalOffset(self) -> int:
        return self.horizontalScrollBar().value()

    def verticalOffset(self) -> int:
        return self.verticalScrollBar().value()

    def isIndexHidden(self, index: QModelIndex) -> bool:
        if not index.isValid():
            return False

        root = self._adapter.root
        parent = index.parent()

        while parent.isValid() and parent != root:
            parent_id = self._adapter.item_id(parent)

            if not self._state.is_expanded(parent_id):
                return True

            parent = parent.parent()

        return False

    def moveCursor(
        self,
        action: QAbstractItemView.CursorAction,
        modifiers: Qt.KeyboardModifier,
    ) -> QModelIndex:
        snapshot = self._snapshot()
        if snapshot is None:
            return QModelIndex()

        visible_items = snapshot.items_in_rect(self._visible_content_rect())

        if not visible_items:
            return QModelIndex()

        current = self.currentIndex()

        if not current.isValid():
            return persistent_to_index(visible_items[0].index)

        current_rect = snapshot.bounds_for_id(self._adapter.item_id(current))

        if current_rect is None:
            return persistent_to_index(visible_items[0].index)

        current_center = current_rect.center()
        best_item: LayoutItem | None = None
        best_score: float | None = None

        for item in visible_items:
            index = persistent_to_index(item.index)

            if index == current:
                continue

            center = item.bounds.center()
            dx = center.x() - current_center.x()
            dy = center.y() - current_center.y()

            if action == QAbstractItemView.CursorAction.MoveDown:
                if dy <= 0:
                    continue
                score = dy * 1000.0 + abs(dx)
            elif action == QAbstractItemView.CursorAction.MoveUp:
                if dy >= 0:
                    continue
                score = -dy * 1000.0 + abs(dx)
            elif action == QAbstractItemView.CursorAction.MoveRight:
                if dx <= 0:
                    continue
                score = dx * 1000.0 + abs(dy)
            elif action == QAbstractItemView.CursorAction.MoveLeft:
                if dx >= 0:
                    continue
                score = -dx * 1000.0 + abs(dy)
            elif action == QAbstractItemView.CursorAction.MoveHome:
                return persistent_to_index(visible_items[0].index)
            elif action == QAbstractItemView.CursorAction.MoveEnd:
                return persistent_to_index(visible_items[-1].index)
            else:
                continue

            if best_score is None or score < best_score:
                best_score = score
                best_item = item

        if best_item is None:
            return current

        return persistent_to_index(best_item.index)

    def setSelection(
        self,
        rect: QRect,
        command: QItemSelectionModel.SelectionFlag,
    ) -> None:
        snapshot = self._snapshot()
        if snapshot is None:
            return

        content_rect = QRectF(rect).translated(
            self.horizontalOffset(),
            self.verticalOffset(),
        )

        items = snapshot.items_in_rect(content_rect)
        selection = QItemSelection()
        model = self.model()

        for item in items:
            index = persistent_to_index(item.index)

            if not index.isValid():
                continue

            parent = index.parent()
            left = model.index(index.row(), 0, parent)
            right = model.index(
                index.row(),
                model.columnCount(parent) - 1,
                parent,
            )
            selection.select(left, right)

        self.selectionModel().select(selection, command)

    def visualRegionForSelection(
        self,
        selection: QItemSelection,
    ) -> QRegion:
        region = QRegion()

        for index in selection.indexes():
            if index.column() != 0:
                continue

            rect = self.visualRect(index)
            if not rect.isNull():
                region += rect

        return region


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=
        "Display a directory using a virtualized mixed tree/tile view.")
    parser.add_argument(
        "directory",
        nargs="?",
        default=os.getcwd(),
        help="Directory to display",
    )
    return parser.parse_args()


def main() -> int:
    arguments = parse_arguments()
    directory = os.path.abspath(os.path.expanduser(arguments.directory))

    if not os.path.isdir(directory):
        raise NotADirectoryError(directory)

    application = QApplication(sys.argv)
    application.setApplicationName("Virtualized File Tree")

    source_model = FileSystemDataModel()
    source_model.setFilter(source_model.filter()
                           | source_model.filter().AllDirs
                           | source_model.filter().Files
                           | source_model.filter().NoDotAndDotDot)
    source_model.setRootPath(directory)

    proxy_model = DirectoryFirstProxyModel()
    proxy_model.setSourceModel(source_model)
    proxy_model.sort(0, Qt.SortOrder.AscendingOrder)

    source_root = source_model.index(directory)
    proxy_root = proxy_model.mapFromSource(source_root)

    state = PresentationState()

    adapter = TreePresentationAdapter(
        model=proxy_model,
        source_model=source_model,
        proxy_model=proxy_model,
        root=proxy_root,
    )

    metrics = TreeMetrics()
    engine = TreeLayoutEngine(adapter, state, metrics)
    controller = LayoutController(engine)

    view = MixedTreeTileView(
        adapter=adapter,
        state=state,
        controller=controller,
    )
    view.setWindowTitle(directory)
    view.resize(1200, 800)
    view.show()

    controller.invalidate()

    return application.exec()


if __name__ == "__main__":
    sys.exit(main())

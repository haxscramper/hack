from __future__ import annotations

import math
from dataclasses import dataclass

from PyQt6.QtCore import (
    QAbstractItemModel,
    QItemSelection,
    QItemSelectionModel,
    QModelIndex,
    QPersistentModelIndex,
    QPoint,
    QRect,
    QSize,
    QTimer,
    Qt,
    pyqtSignal,
)

from PyQt6.QtGui import (
    QColor,
    QPainter,
    QPalette,
    QPen,
    QPolygon,
    QRegion,
)

from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
)

import logging

from beartype import beartype

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)

log = logging.getLogger(__name__)

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


@dataclass
class RenderState:
    selected: bool
    current: bool
    hovered: bool
    expanded: bool


class TileDelegate:

    def __init__(self) -> None:
        self.request_update = lambda: None

    def paint(
        self,
        painter: QPainter,
        primary: QModelIndex,
        area: QSize,
        attributes: list[QModelIndex],
        state: RenderState,
    ) -> None:
        raise NotImplementedError

    def getItemBoundingBoxes(
        self,
        primary: QModelIndex,
        area: QSize,
        attributes: list[QModelIndex],
    ) -> list[QRect]:
        raise NotImplementedError


def _draw_triangle(painter: QPainter, box: QRect, expanded: bool, color: QColor) -> None:
    painter.save()
    painter.setPen(Qt.PenStyle.NoPen)
    painter.setBrush(color)
    cx, cy = box.center().x(), box.center().y()
    if expanded:
        poly = QPolygon([
            QPoint(cx - 4, cy - 2),
            QPoint(cx + 4, cy - 2),
            QPoint(cx, cy + 4),
        ])
    else:
        poly = QPolygon([
            QPoint(cx - 2, cy - 4),
            QPoint(cx - 2, cy + 4),
            QPoint(cx + 4, cy),
        ])
    painter.drawPolygon(poly)
    painter.restore()


def attr_label(index: QModelIndex) -> str:
    model = index.model()
    header = model.headerData(index.column(), Qt.Orientation.Horizontal,
                              Qt.ItemDataRole.DisplayRole)
    value = index.data(Qt.ItemDataRole.DisplayRole)
    header = "" if header is None else str(header)
    value = "" if value is None else str(value)
    return f"{header}: {value}" if header else value


class BaseDirDelegate(TileDelegate):

    def paint(self, painter, primary, area, attributes, state):
        pal = QApplication.palette()
        rect = QRect(QPoint(0, 0), area)
        painter.save()

        bg = pal.color(QPalette.ColorRole.AlternateBase)
        if state.selected or state.current:
            bg = pal.color(QPalette.ColorRole.Highlight).lighter(170)
        painter.fillRect(rect, bg)
        painter.setPen(QPen(pal.color(QPalette.ColorRole.Mid)))
        painter.drawLine(rect.bottomLeft(), rect.bottomRight())

        toggle = QRect(2, (area.height() - 16) // 2, 16, 16)
        _draw_triangle(painter, toggle, state.expanded,
                       pal.color(QPalette.ColorRole.Text))

        painter.setPen(pal.color(QPalette.ColorRole.Text))
        font = painter.font()
        font.setBold(True)
        painter.setFont(font)
        name = str(primary.data(Qt.ItemDataRole.DisplayRole) or "")
        cols = "   ".join(attr_label(a) for a in attributes)
        text = f"{name}      {cols}" if cols else name
        text_rect = QRect(toggle.right() + 6, 0,
                          area.width() - toggle.right() - 10, area.height())
        painter.drawText(text_rect,
                         Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft, text)
        painter.restore()

    def getItemBoundingBoxes(self, primary, area, attributes):
        toggle = QRect(2, (area.height() - 16) // 2, 16, 16)
        primary_box = QRect(toggle.right() + 6, 0, area.width() // 2, area.height())
        boxes = [primary_box]
        x = area.width() // 2
        step = (area.width() - x) // max(1, len(attributes))
        for i in range(len(attributes)):
            boxes.append(QRect(x + i * step, 0, step, area.height()))
        return boxes


@dataclass
class Hit:
    index: QModelIndex
    rect: QRect
    depth: int


@beartype
class MixedTreeTileView(QAbstractItemView):
    CONTENT_MARGIN = 8
    INDENT = 18
    TILE_SPACING_X = 8
    TILE_SPACING_Y = 8
    SECTION_SPACING = 10
    BASE_ITEM_W = 240
    BASE_ITEM_H = 96
    BASE_DIR_H = 30

    expanded = pyqtSignal(QModelIndex)
    collapsed = pyqtSignal(QModelIndex)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.zoom_factor = 1.0
        self._root_index = QModelIndex()
        self._expanded: set[QPersistentModelIndex] = set()
        self._hovered: QPersistentModelIndex | None = None

        self._dir_hits: list[Hit] = []
        self._item_hits: list[Hit] = []
        self._order: list[QModelIndex] = []
        self._content_height = 0
        self._layout_dirty = True

        self._dir_delegate: TileDelegate | None = None
        self._file_delegate: TileDelegate | None = None

        self._layout_timer = QTimer(self)
        self._layout_timer.setSingleShot(True)
        self._layout_timer.timeout.connect(self._apply_pending_layout)

        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setSelectionMode(QAbstractItemView.SelectionMode.ExtendedSelection)
        self.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.horizontalScrollBar().setRange(0, 0)

    def set_delegates(self, dir_delegate: TileDelegate,
                      file_delegate: TileDelegate) -> None:
        self._dir_delegate = dir_delegate
        self._file_delegate = file_delegate
        dir_delegate.request_update = self.viewport().update
        file_delegate.request_update = self.viewport().update

    @property
    def item_size(self) -> QSize:
        return QSize(round(self.BASE_ITEM_W * self.zoom_factor),
                     round(self.BASE_ITEM_H * self.zoom_factor))

    @property
    def dir_height(self) -> int:
        return round(self.BASE_DIR_H * self.zoom_factor)

    def visible_width(self) -> int:
        return max(1, self.viewport().width())

    def flow_columns(self, depth: int) -> int:
        avail = self.visible_width() - 2 * self.CONTENT_MARGIN - depth * self.INDENT
        iw = self.item_size.width()
        avail = max(avail, iw)
        return max(1, (avail + self.TILE_SPACING_X) // (iw + self.TILE_SPACING_X))

    def setModel(self, model: QAbstractItemModel) -> None:
        super().setModel(model)
        model.rowsInserted.connect(self._on_structure_changed)
        model.rowsRemoved.connect(self._on_structure_changed)
        model.modelReset.connect(self._on_model_reset)
        model.layoutChanged.connect(self._on_structure_changed)
        model.dataChanged.connect(self._on_data_changed)
        self.selectionModel().selectionChanged.connect(
            lambda *a: self.viewport().update())
        self.selectionModel().currentChanged.connect(lambda *a: self.viewport().update())
        self._update_scrollbars()

    def _on_model_reset(self, *args) -> None:
        # TODO: Finalze the simple behavior of fully resetting the model,
        # and then update implementation to have the model remember the
        # expansion regions when the results are reset.
        self._expanded.clear()
        self._hovered = None
        self._on_structure_changed()

    def setRootIndex(self, index: QModelIndex) -> None:
        self._root_index = index
        if index.isValid() and self.model() and self.model().canFetchMore(index):
            self.model().fetchMore(index)
        self._update_scrollbars()

    def rootIndex(self) -> QModelIndex:
        return QModelIndex(self._root_index)

    def _on_data_changed(self, *args) -> None:
        self._layout_dirty = True

        if not self._layout_timer.isActive():
            self._layout_timer.start(0)

    def _apply_pending_layout(self) -> None:
        self._update_scrollbars()
        self.viewport().update()

    def _on_structure_changed(self, *args) -> None:
        self._layout_dirty = True

        if not self._layout_timer.isActive():
            self._layout_timer.start(0)

    def _is_expanded(self, index: QModelIndex) -> bool:
        return QPersistentModelIndex(index) in self._expanded

    def _toggle_expand(self, index: QModelIndex) -> None:
        pidx = QPersistentModelIndex(index)

        if pidx in self._expanded:
            self._expanded.discard(pidx)
            self.collapsed.emit(index)
        else:
            self._expanded.add(pidx)
            if self.model().canFetchMore(index):
                self.model().fetchMore(index)
            self.expanded.emit(index)

        self._update_scrollbars()
        self.viewport().update()

    def _attrs(self, primary: QModelIndex) -> list[QModelIndex]:
        model = self.model()
        parent = primary.parent()
        row = primary.row()
        return [model.index(row, c, parent) for c in range(1, model.columnCount(parent))]

    def _ensure_layout(self) -> None:
        if not self._layout_dirty or self.model() is None:
            return
        self._dir_hits.clear()
        self._item_hits.clear()
        self._order.clear()
        y = self._layout_nested(self._root_index, 0, self.CONTENT_MARGIN)
        self._content_height = y + self.CONTENT_MARGIN
        self._layout_dirty = False

    def _layout_nested(self, parent: QModelIndex, depth: int, y: int) -> int:
        model = self.model()
        assert model
        n = model.rowCount(parent)
        x = self.CONTENT_MARGIN + depth * self.INDENT

        dirs: list[QModelIndex] = []
        files: list[QModelIndex] = []
        for r in range(n):
            idx = model.index(r, 0, parent)
            (dirs if model.hasChildren(idx) else files).append(idx)

        dir_w = max(100,
                    self.visible_width() - 2 * self.CONTENT_MARGIN - depth * self.INDENT)
        for d in dirs:
            rect = QRect(x, y, dir_w, self.dir_height)
            self._dir_hits.append(Hit(d, rect, depth))
            self._order.append(d)
            y += self.dir_height + 4
            if self._is_expanded(d):
                y = self._layout_nested(d, depth + 1, y)

        if files:
            cols = self.flow_columns(depth)
            iw = self.item_size.width()
            ih = self.item_size.height()
            for i, f in enumerate(files):
                rr, cc = divmod(i, cols)
                fx = x + cc * (iw + self.TILE_SPACING_X)
                fy = y + rr * (ih + self.TILE_SPACING_Y)
                rect = QRect(fx, fy, iw, ih)
                self._item_hits.append(Hit(f, rect, depth))
                self._order.append(f)
            rows = math.ceil(len(files) / cols)
            y += rows * ih + max(0, rows - 1) * self.TILE_SPACING_Y
            y += self.SECTION_SPACING
        return y

    def _update_scrollbars(self) -> None:
        self._layout_dirty = True
        self._ensure_layout()
        page = max(1, self.viewport().height())
        self.verticalScrollBar().setPageStep(page)
        self.verticalScrollBar().setRange(0, max(0, self._content_height - page))
        self.viewport().update()

    def paintEvent(self, event) -> None:
        if self.model() is None or self._dir_delegate is None:
            return
        self._ensure_layout()
        painter = QPainter(self.viewport())
        pal = self.palette()
        painter.fillRect(self.viewport().rect(), pal.color(QPalette.ColorRole.Base))

        offset = self.verticalOffset()
        visible = QRect(0, offset, self.viewport().width(), self.viewport().height())

        for hit, delegate in ([(h, self._dir_delegate) for h in self._dir_hits] +
                              [(h, self._file_delegate) for h in self._item_hits]):
            if not hit.rect.intersects(visible):
                continue
            state = RenderState(
                selected=self.selectionModel().isSelected(hit.index),
                current=(hit.index == self.currentIndex()),
                hovered=(self._hovered == QPersistentModelIndex(hit.index)),
                expanded=self._is_expanded(hit.index),
            )
            vp_rect = hit.rect.translated(0, -offset)
            painter.save()
            painter.translate(vp_rect.topLeft())
            delegate.paint(painter, hit.index, hit.rect.size(), self._attrs(hit.index),
                           state)
            painter.restore()

    def scrollContentsBy(self, dx: int, dy: int) -> None:
        self.viewport().update()

    def resizeEvent(self, event) -> None:
        super().resizeEvent(event)
        self._update_scrollbars()

    def wheelEvent(self, event) -> None:
        if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            if event.angleDelta().y() > 0:
                self.zoom_factor = min(self.zoom_factor * 1.15, 20.0)
            else:
                self.zoom_factor = max(self.zoom_factor / 1.15, 0.2)
            self._update_scrollbars()
            event.accept()
        else:
            super().wheelEvent(event)

    def mousePressEvent(self, event) -> None:
        idx = self.indexAt(event.position().toPoint())
        if (idx.isValid() and self.model().hasChildren(idx) and
                event.button() == Qt.MouseButton.LeftButton):
            self._toggle_expand(idx)
            self.selectionModel().setCurrentIndex(
                idx, QItemSelectionModel.SelectionFlag.NoUpdate)
            return
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event) -> None:
        idx = self.indexAt(event.position().toPoint())
        new = QPersistentModelIndex(idx) if idx.isValid() else None
        if new != self._hovered:
            self._hovered = new
            self.viewport().update()
        super().mouseMoveEvent(event)

    def leaveEvent(self, event) -> None:
        if self._hovered is not None:
            self._hovered = None
            self.viewport().update()
        super().leaveEvent(event)

    def _content_rect(self, index: QModelIndex) -> QRect:
        self._ensure_layout()
        for hit in self._dir_hits:
            if hit.index == index:
                return hit.rect
        for hit in self._item_hits:
            if hit.index == index:
                return hit.rect
        return QRect()

    def visualRect(self, index: QModelIndex) -> QRect:
        rect = self._content_rect(index)
        if rect.isNull():
            return QRect()
        return rect.translated(0, -self.verticalOffset())

    def indexAt(self, point: QPoint) -> QModelIndex:
        self._ensure_layout()
        p = QPoint(point.x(), point.y() + self.verticalOffset())
        for hit in self._item_hits:
            if hit.rect.contains(p):
                return hit.index
        for hit in self._dir_hits:
            if hit.rect.contains(p):
                return hit.index
        return QModelIndex()

    def scrollTo(self, index, hint=QAbstractItemView.ScrollHint.EnsureVisible):
        rect = self._content_rect(index)
        if rect.isNull():
            return
        sb = self.verticalScrollBar()
        vp_h = self.viewport().height()
        if rect.top() < sb.value():
            sb.setValue(rect.top())
        elif rect.bottom() > sb.value() + vp_h:
            sb.setValue(rect.bottom() - vp_h)

    def horizontalOffset(self) -> int:
        return self.horizontalScrollBar().value()

    def verticalOffset(self) -> int:
        return self.verticalScrollBar().value()

    def isIndexHidden(self, index: QModelIndex) -> bool:
        if not index.isValid():
            return False
        p = index.parent()
        while p.isValid() and p != self._root_index:
            if not self._is_expanded(p):
                return True
            p = p.parent()
        return False

    def moveCursor(self, action, modifiers) -> QModelIndex:
        self._ensure_layout()
        if not self._order:
            return QModelIndex()
        current = self.currentIndex()
        CA = QAbstractItemView.CursorAction
        if not current.isValid():
            return self._order[0]

        if action in (CA.MoveHome,):
            return self._order[0]
        if action in (CA.MoveEnd,):
            return self._order[-1]
        if action in (CA.MoveNext, CA.MovePrevious):
            step = 1 if action == CA.MoveNext else -1
            try:
                i = self._order.index(current)
            except ValueError:
                return self._order[0]
            return self._order[max(0, min(len(self._order) - 1, i + step))]

        cur_rect = self._content_rect(current)
        if cur_rect.isNull():
            return self._order[0]
        result = self._geometric_neighbor(current, cur_rect, action)
        return result if result.isValid() else current

    def _geometric_neighbor(self, current, cur_rect, action) -> QModelIndex:
        CA = QAbstractItemView.CursorAction
        cx, cy = cur_rect.center().x(), cur_rect.center().y()
        best = QModelIndex()
        best_score = None
        for hit in self._dir_hits + self._item_hits:
            if hit.index == current:
                continue
            r = hit.rect
            rx, ry = r.center().x(), r.center().y()
            if action == CA.MoveDown and ry > cy:
                score = (ry - cy) * 1000 + abs(rx - cx)
            elif action == CA.MoveUp and ry < cy:
                score = (cy - ry) * 1000 + abs(rx - cx)
            elif action == CA.MoveRight and rx > cx:
                score = (rx - cx) * 1000 + abs(ry - cy)
            elif action == CA.MoveLeft and rx < cx:
                score = (cx - rx) * 1000 + abs(ry - cy)
            else:
                continue
            if best_score is None or score < best_score:
                best_score, best = score, hit.index
        return best

    def setSelection(self, rect: QRect, command) -> None:
        self._ensure_layout()
        content = rect.translated(0, self.verticalOffset())
        selection = QItemSelection()
        model = self.model()
        for hit in self._item_hits:
            if hit.rect.intersects(content):
                row, parent = hit.index.row(), hit.index.parent()
                left = model.index(row, 0, parent)
                right = model.index(row, model.columnCount(parent) - 1, parent)
                selection.select(left, right)
        self.selectionModel().select(selection, command)

    def visualRegionForSelection(self, selection: QItemSelection) -> QRegion:
        region = QRegion()
        for index in selection.indexes():
            if index.column() == 0:
                r = self.visualRect(index)
                if not r.isNull():
                    region += r
        return region

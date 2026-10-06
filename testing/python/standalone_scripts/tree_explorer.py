# https://github.com/freakinfrick/treebeard Sol 6.1 conversion to qt
# https://x.com/FreakinFrick/status/2104935423252500967

from __future__ import annotations

import math
import sys
from dataclasses import dataclass, field
from time import perf_counter

from beartype import beartype
from beartype.typing import Any, Callable
from PyQt6.QtCore import (
    QAbstractItemModel,
    QItemSelection,
    QItemSelectionModel,
    QModelIndex,
    QPersistentModelIndex,
    QPoint,
    QPointF,
    QRect,
    QRectF,
    Qt,
    QTimer,
)
from PyQt6.QtGui import (
    QColor,
    QFont,
    QKeyEvent,
    QMouseEvent,
    QPainter,
    QPainterPath,
    QPen,
    QRegion,
    QResizeEvent,
    QStandardItem,
    QStandardItemModel,
    QWheelEvent,
)
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
    QDialog,
    QLabel,
    QMainWindow,
    QVBoxLayout,
    QWidget,
)


@beartype
@dataclass
class Spring:
    value: float = 0.0
    velocity: float = 0.0

    def advance(self, target: float, duration: float, elapsed: float) -> bool:
        omega = 2.0 / duration
        amount = omega * elapsed
        decay = 1.0 / (1.0 + amount + 0.48 * amount**2 + 0.235 * amount**3)
        displacement = self.value - target
        adjustment = (self.velocity + omega * displacement) * elapsed
        self.velocity = (self.velocity - omega * adjustment) * decay
        result = target + (displacement + adjustment) * decay

        if (self.value < target) == (target < result):
            result = target
            self.velocity = 0.0

        self.value = result

        if abs(self.value - target) < 0.05 and abs(self.velocity) < 0.5:
            self.value = target
            self.velocity = 0.0
            return False

        return True


@beartype
@dataclass
class Placement:
    index: QPersistentModelIndex
    label: str
    width: float
    x: float
    y: float


@beartype
@dataclass
class Branch:
    parent: QPersistentModelIndex
    nested: list[QPersistentModelIndex]
    track: int


@beartype
@dataclass
class AnimatedItem:
    index: QPersistentModelIndex
    label: str
    width: float
    x: Spring
    y: Spring
    target_x: float
    target_y: float
    opacity: float = 0.0
    departing: bool = False


@beartype
@dataclass
class Column:
    entries: list[QPersistentModelIndex] = field(default_factory=list)
    left: float = 0.0
    right: float = 0.0


@beartype
class HorizontalTreeView(QAbstractItemView):

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self.expanded: set[QPersistentModelIndex] = set()
        self.remembered: dict[QPersistentModelIndex,
                              QPersistentModelIndex] = {}
        self.items: dict[QPersistentModelIndex, AnimatedItem] = {}
        self.placements: dict[QPersistentModelIndex, Placement] = {}
        self.branches: list[Branch] = []
        self.columns: list[Column] = []
        self.spine: set[QPersistentModelIndex] = set()
        self.route: set[QPersistentModelIndex] = set()
        self.anchor = QPersistentModelIndex()
        self.camera = Spring()
        self.camera_target = 0.0
        self.camera_hold: float | None = None
        self.connections: list[tuple[Any, Callable[..., None]]] = []
        self.last_frame = perf_counter()

        self.row_pitch = 34.0
        self.column_gap = 38.0
        self.branch_length = 16.0
        self.track_pitch = 10.0
        self.maximum_name_width = 250.0
        self.label_padding = 10.0
        self.label_height = 28.0

        self.background = QColor("#0b0d14")
        self.foreground = QColor("#eef0fa")
        self.muted = QColor("#8794c4")
        self.connector = QColor("#394a80")
        self.active_connector = QColor("#718bda")
        self.route_connector = QColor("#a8bbff")
        self.selection_background = QColor("#252e51")
        self.panel_background = QColor("#121724")

        font = QFont("monospace")
        font.setStyleHint(QFont.StyleHint.Monospace)
        font.setPointSize(11)
        self.setFont(font)

        self.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self.setSelectionBehavior(
            QAbstractItemView.SelectionBehavior.SelectRows)
        self.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.viewport().setMouseTracking(True)
        self.viewport().setAutoFillBackground(False)

        self.timer = QTimer(self)
        self.timer.setInterval(16)
        self.timer.timeout.connect(self.advance_animation)

    def model_index(self, index: QPersistentModelIndex) -> QModelIndex:
        return QModelIndex(index)

    def root_key(self) -> QPersistentModelIndex:
        return QPersistentModelIndex(self.rootIndex())

    def nested_indices(
            self,
            parent: QPersistentModelIndex) -> list[QPersistentModelIndex]:
        model = self.model()

        if model is None:
            return []

        index = self.model_index(parent)
        return [
            QPersistentModelIndex(model.index(row, 0, index))
            for row in range(model.rowCount(index))
        ]

    def label_for(self, index: QPersistentModelIndex) -> str:
        if index == self.root_key() and not index.isValid():
            model = self.model()
            return model.objectName(
            ) or "Model" if model is not None else "Model"

        value = index.data(Qt.ItemDataRole.DisplayRole)
        return "" if value is None else str(value)

    def path_to(self,
                index: QPersistentModelIndex) -> list[QPersistentModelIndex]:
        root = self.root_key()
        path: list[QPersistentModelIndex] = []
        current = index

        while current != root and current.isValid():
            path.append(current)
            current = QPersistentModelIndex(current.parent())

        if current != root:
            return []

        path.append(root)
        path.reverse()
        return path

    def is_expanded(self, index: QPersistentModelIndex) -> bool:
        return index == self.root_key() or index in self.expanded

    def visible_outline(self) -> list[QPersistentModelIndex]:
        output: list[QPersistentModelIndex] = []
        pending = list(reversed(self.nested_indices(self.root_key())))

        while pending:
            index = pending.pop()
            output.append(index)

            if self.is_expanded(index):
                pending.extend(reversed(self.nested_indices(index)))

        return output

    def cursor_spine(self) -> list[QPersistentModelIndex]:
        root = self.root_key()
        path = self.path_to(self.anchor) if self.anchor.isValid() else [root]

        if not path:
            path = [root]

        current = path[-1]

        while self.is_expanded(current):
            nested = self.nested_indices(current)

            if not nested:
                break

            remembered = self.remembered.get(current)
            current = remembered if remembered in nested else nested[0]
            path.append(current)

        return path

    def setModel(self, model: QAbstractItemModel | None) -> None:
        for signal, callback in self.connections:
            signal.disconnect(callback)

        self.connections.clear()
        self.expanded.clear()
        self.remembered.clear()
        self.items.clear()
        self.anchor = QPersistentModelIndex()
        self.camera_hold = None

        super().setModel(model)

        if model is not None:
            for signal in (
                    model.rowsInserted,
                    model.rowsRemoved,
                    model.rowsMoved,
                    model.columnsInserted,
                    model.columnsRemoved,
                    model.columnsMoved,
                    model.layoutChanged,
                    model.dataChanged,
            ):
                callback = self.model_changed
                signal.connect(callback)
                self.connections.append((signal, callback))

        self.select_first()
        self.rebuild_layout()

    def setRootIndex(self, index: QModelIndex) -> None:
        super().setRootIndex(index)
        self.anchor = QPersistentModelIndex()
        self.camera_hold = None
        self.select_first()
        self.rebuild_layout()

    def reset(self) -> None:
        self.expanded.clear()
        self.remembered.clear()
        self.items.clear()
        self.anchor = QPersistentModelIndex()
        self.camera_hold = None
        super().reset()
        self.select_first()
        self.rebuild_layout()

    def select_first(self) -> None:
        model = self.model()

        if model is not None and 0 < model.rowCount(self.rootIndex()):
            self.select_index(model.index(0, 0, self.rootIndex()))

    def model_changed(self, *arguments: Any) -> None:
        self.expanded = {index for index in self.expanded if index.isValid()}
        self.remembered = {
            parent: index
            for parent, index in self.remembered.items()
            if (parent == self.root_key() or parent.isValid()) and
            index.isValid() and QPersistentModelIndex(index.parent()) == parent
        }

        if not self.currentIndex().isValid():
            self.select_first()

        self.rebuild_layout()

    def currentChanged(self, current: QModelIndex,
                       previous: QModelIndex) -> None:
        super().currentChanged(current, previous)
        self.anchor = QPersistentModelIndex(current)

        if current.isValid():
            parent = QPersistentModelIndex(current.parent())
            self.remembered[parent] = self.anchor

            for index in self.path_to(self.anchor)[:-1]:
                if index != self.root_key():
                    self.expanded.add(index)

        self.rebuild_layout()

    def select_index(self, index: QModelIndex) -> None:
        selection = self.selectionModel()

        if selection is None or not index.isValid():
            return

        selection.setCurrentIndex(
            index,
            QItemSelectionModel.SelectionFlag.ClearAndSelect
            | QItemSelectionModel.SelectionFlag.Rows,
        )

    def rebuild_layout(self) -> None:
        root = self.root_key()
        spine = self.cursor_spine()
        self.spine = set(spine)
        self.route = set(
            self.path_to(QPersistentModelIndex(self.currentIndex())))
        metrics = self.fontMetrics()
        placements: dict[QPersistentModelIndex, Placement] = {}
        branches: list[Branch] = []
        columns: list[Column] = []
        level: list[tuple[QPersistentModelIndex, float]] = [(root, 0.0)]
        x = 0.0
        depth = 0

        while level:
            column = Column()

            for index, y in level:
                label = metrics.elidedText(
                    self.label_for(index),
                    Qt.TextElideMode.ElideRight,
                    int(self.maximum_name_width),
                )
                width = float(metrics.horizontalAdvance(label))
                width += self.label_padding * 2.0
                placements[index] = Placement(
                    index=index,
                    label=label,
                    width=width,
                    x=x,
                    y=y * self.row_pitch,
                )
                column.entries.append(index)

            column_width = max(placements[index].width for index, y in level)
            parents = [(index, y, self.nested_indices(index))
                       for index, y in level if self.is_expanded(index)]
            parents = [(index, y, nested) for index, y, nested in parents
                       if nested]

            if not parents:
                column.left = x - self.column_gap / 2.0
                column.right = x + column_width + self.column_gap
                columns.append(column)
                break

            starts = [0.0] * len(parents)
            pinned: int | None = None

            if depth + 1 < len(spine):
                for position, (index, y, nested) in enumerate(parents):
                    if index == spine[depth]:
                        pinned = position
                        starts[position] = -float(
                            nested.index(spine[depth + 1]))
                        break

            if pinned is not None:
                next_start = starts[pinned]

                for position in reversed(range(pinned)):
                    index, y, nested = parents[position]
                    ideal = y - (len(nested) - 1) / 2.0
                    starts[position] = min(ideal,
                                           next_start - len(nested) - 1.0)
                    next_start = starts[position]

                previous_end = starts[pinned] + len(parents[pinned][2]) - 1.0
                below = pinned + 1
            else:
                previous_end = -math.inf
                below = 0

            for position in range(below, len(parents)):
                index, y, nested = parents[position]
                ideal = y - (len(nested) - 1) / 2.0
                starts[position] = max(ideal, previous_end + 2.0)
                previous_end = starts[position] + len(nested) - 1.0

            capacity = max(2, int(column_width / 3.0 / self.track_pitch))
            tracks = [0] * len(parents)
            raised = [
                position for position, (index, y, nested) in enumerate(parents)
                if starts[position] + len(nested) - 1 < y
            ]
            hanging = [
                position for position, (index, y, nested) in enumerate(parents)
                if y < starts[position]
            ]

            for track, position in enumerate(reversed(raised)):
                tracks[position] = min(track, capacity - 1)

            for track, position in enumerate(hanging):
                tracks[position] = min(track, capacity - 1)

            lanes = max(tracks) + 1
            next_x = (x + column_width + self.column_gap +
                      lanes * self.track_pitch + self.branch_length)
            column.left = x - self.column_gap / 2.0
            column.right = next_x - self.column_gap / 2.0
            columns.append(column)

            next_level: list[tuple[QPersistentModelIndex, float]] = []

            for position, (index, y, nested) in enumerate(parents):
                branches.append(
                    Branch(
                        parent=index,
                        nested=nested,
                        track=tracks[position],
                    ))
                next_level.extend((entry, starts[position] + offset)
                                  for offset, entry in enumerate(nested))

            next_level.sort(key=lambda entry: entry[1])
            level = next_level
            x = next_x
            depth += 1

        self.placements = placements
        self.branches = branches
        self.columns = columns

        for index, placement in placements.items():
            animated = self.items.get(index)

            if animated is None:
                parent = QPersistentModelIndex(index.parent())
                origin = self.items.get(parent) if index != root else None
                origin_x = (origin.x.value + origin.width
                            if origin is not None else placement.x)
                origin_y = origin.y.value if origin is not None else placement.y
                animated = AnimatedItem(
                    index=index,
                    label=placement.label,
                    width=placement.width,
                    x=Spring(value=origin_x),
                    y=Spring(value=origin_y),
                    target_x=placement.x,
                    target_y=placement.y,
                )
                self.items[index] = animated

            animated.label = placement.label
            animated.width = placement.width
            animated.target_x = placement.x
            animated.target_y = placement.y
            animated.departing = False

        for index, animated in self.items.items():
            if index in placements:
                continue

            animated.departing = True
            parent = QPersistentModelIndex(
                index.parent()) if index.isValid() else root

            while parent not in placements and parent.isValid():
                parent = QPersistentModelIndex(parent.parent())

            destination = placements.get(parent, placements[root])
            animated.target_x = destination.x + destination.width
            animated.target_y = destination.y

        focus = placements.get(self.anchor, placements[root])
        desired = focus.x - self.viewport().width() * 0.38

        if self.camera_hold is not None:
            desired = min(
                max(
                    self.camera_hold,
                    focus.x + focus.width + 20.0 - self.viewport().width(),
                ),
                focus.x - 20.0,
            )
            self.camera_hold = desired

        self.camera_target = desired
        self.start_animation()

    def start_animation(self) -> None:
        if not self.timer.isActive():
            self.last_frame = perf_counter()
            self.timer.start()

        self.viewport().update()

    def advance_animation(self) -> None:
        now = perf_counter()
        elapsed = min(now - self.last_frame, 0.05)
        self.last_frame = now
        moving = self.camera.advance(self.camera_target, 0.16, elapsed)
        departed: list[QPersistentModelIndex] = []

        for index, animated in self.items.items():
            moving |= animated.x.advance(animated.target_x, 0.11, elapsed)
            moving |= animated.y.advance(animated.target_y, 0.11, elapsed)
            target_opacity = 0.0 if animated.departing else 1.0
            duration = 0.05 if animated.departing else 0.07
            animated.opacity += (target_opacity - animated.opacity) * (
                1.0 - math.exp(-elapsed / duration))

            if abs(animated.opacity - target_opacity) < 0.01:
                animated.opacity = target_opacity
            else:
                moving = True

            if animated.departing and animated.opacity < 0.01:
                departed.append(index)

        for index in departed:
            del self.items[index]

        self.viewport().update()

        if not moving:
            self.timer.stop()

    def screen_rect(self, animated: AnimatedItem) -> QRectF:
        return QRectF(
            animated.x.value - self.camera.value,
            animated.y.value + self.selection_line() - self.label_height / 2.0,
            animated.width,
            self.label_height,
        )

    def selection_line(self) -> float:
        return (self.viewport().height() - 30.0) / 2.0

    def paintEvent(self, event: Any) -> None:
        painter = QPainter(self.viewport())
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.fillRect(self.viewport().rect(), self.background)
        painter.setFont(self.font())

        selected = QPersistentModelIndex(self.currentIndex())
        placement = self.placements.get(selected)

        if placement is not None:
            pill = QRectF(
                placement.x - self.camera.value - 5.0,
                self.selection_line() - self.label_height / 2.0 - 2.0,
                placement.width + 10.0,
                self.label_height + 4.0,
            )
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(self.selection_background)
            painter.drawRoundedRect(pill, 7.0, 7.0)

        for branch in self.branches:
            self.paint_branch(painter, branch)

        ordered = sorted(
            self.items.values(),
            key=lambda animated: not animated.departing,
        )
        viewport_rect = QRectF(self.viewport().rect())

        for animated in ordered:
            rect = self.screen_rect(animated)

            if not rect.intersects(viewport_rect):
                continue

            painter.setOpacity(animated.opacity)
            on_route = animated.index in self.route
            color = self.foreground if on_route else self.muted

            if animated.departing:
                color = self.muted

            font = QFont(self.font())
            font.setBold(on_route or bool(self.nested_indices(animated.index)))
            painter.setFont(font)
            painter.setPen(color)
            painter.drawText(
                rect.adjusted(self.label_padding, 0.0, -self.label_padding,
                              0.0),
                Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter,
                animated.label,
            )

            if (not animated.departing and not self.is_expanded(animated.index)
                    and self.nested_indices(animated.index)):
                painter.setPen(QPen(color, 1.8, Qt.PenStyle.SolidLine))
                center = rect.center().y()
                x = rect.right() + 4.0
                marker = QPainterPath(QPointF(x, center - 4.0))
                marker.lineTo(x + 4.0, center)
                marker.lineTo(x, center + 4.0)
                painter.drawPath(marker)

        painter.setOpacity(1.0)
        self.paint_status(painter)
        painter.end()

    def paint_branch(self, painter: QPainter, branch: Branch) -> None:
        parent = self.items.get(branch.parent)

        if parent is None:
            return

        nested = [
            self.items[index] for index in branch.nested if index in self.items
        ]

        if not nested:
            return

        parent_rect = self.screen_rect(parent)
        rects = [self.screen_rect(animated) for animated in nested]
        start = parent_rect.right()
        bar = min(rect.left() for rect in rects) - self.branch_length

        if bar < start:
            return

        top = min(rect.center().y() for rect in rects)
        bottom = max(rect.center().y() for rect in rects)
        parent_y = parent_rect.center().y()
        join_y = top if bottom < parent_y else min(max(parent_y, top), bottom)
        track = max(
            start,
            bar - (branch.track + 1) * self.track_pitch,
        )

        path = QPainterPath(QPointF(bar, top))
        path.lineTo(bar, bottom)

        for rect in rects:
            path.moveTo(bar, rect.center().y())
            path.lineTo(rect.left() - 2.0, rect.center().y())

        path.moveTo(start, parent_y)

        if join_y == parent_y:
            path.lineTo(bar, parent_y)
        else:
            path.lineTo(track, parent_y)
            path.lineTo(track, join_y)
            path.lineTo(bar, join_y)

        active = branch.parent in self.spine
        color = self.active_connector if active else self.connector
        painter.setOpacity(parent.opacity)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.setPen(
            QPen(
                color,
                1.5,
                Qt.PenStyle.SolidLine,
                Qt.PenCapStyle.RoundCap,
                Qt.PenJoinStyle.RoundJoin,
            ))
        painter.drawPath(path)

        routed = next(
            (rect for animated, rect in zip(nested, rects)
             if animated.index in self.route),
            None,
        )

        if branch.parent in self.route and routed is not None:
            route = QPainterPath(QPointF(start, parent_y))

            if join_y == parent_y:
                route.lineTo(bar, parent_y)
            else:
                route.lineTo(track, parent_y)
                route.lineTo(track, join_y)
                route.lineTo(bar, join_y)

            route.lineTo(bar, routed.center().y())
            route.lineTo(routed.left() - 2.0, routed.center().y())
            painter.setPen(
                QPen(
                    self.route_connector,
                    2.8,
                    Qt.PenStyle.SolidLine,
                    Qt.PenCapStyle.RoundCap,
                    Qt.PenJoinStyle.RoundJoin,
                ))
            painter.drawPath(route)

    def paint_status(self, painter: QPainter) -> None:
        area = QRectF(
            0.0,
            float(self.viewport().height() - 30),
            float(self.viewport().width()),
            30.0,
        )
        painter.fillRect(area, self.panel_background)
        painter.setFont(self.font())
        painter.setPen(self.muted)

        path = self.path_to(QPersistentModelIndex(self.currentIndex()))
        breadcrumb = " › ".join(self.label_for(index) for index in path)
        hints = "hjkl / arrows · space fold · C collapse · e expand · ? help"
        metrics = self.fontMetrics()
        hints_width = metrics.horizontalAdvance(hints) + 20
        available = max(0, int(area.width()) - hints_width - 24)

        painter.drawText(
            area.adjusted(12.0, 0.0, -float(hints_width), 0.0),
            Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter,
            metrics.elidedText(breadcrumb, Qt.TextElideMode.ElideLeft,
                               available),
        )

        if hints_width + 100 < area.width():
            painter.drawText(
                area.adjusted(0.0, 0.0, -12.0, 0.0),
                Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
                hints,
            )

    def open_current(self) -> None:
        index = QPersistentModelIndex(self.currentIndex())

        if not index.isValid():
            return

        model = self.model()

        if model is None:
            return

        ordinary = self.model_index(index)

        if model.canFetchMore(ordinary):
            model.fetchMore(ordinary)

        nested = self.nested_indices(index)

        if not nested:
            self.preview_current()
            return

        self.expanded.add(index)
        remembered = self.remembered.get(index)
        destination = remembered if remembered in nested else nested[0]
        self.select_index(self.model_index(destination))
        self.rebuild_layout()

    def toggle_current(self) -> None:
        index = QPersistentModelIndex(self.currentIndex())

        if not index.isValid():
            return

        if index in self.expanded:
            self.expanded.remove(index)
        else:
            model = self.model()

            if model is not None and model.canFetchMore(self.currentIndex()):
                model.fetchMore(self.currentIndex())

            if self.nested_indices(index):
                self.expanded.add(index)

        self.rebuild_layout()

    def collapse_current(self) -> None:
        self.expanded.discard(QPersistentModelIndex(self.currentIndex()))
        self.rebuild_layout()

    def collapse_others(self) -> None:
        path = self.path_to(QPersistentModelIndex(self.currentIndex()))
        self.expanded.intersection_update(path[:-1])
        self.rebuild_layout()

    def expand_current(self) -> None:
        index = QPersistentModelIndex(self.currentIndex())

        if not index.isValid():
            return

        if not self.nested_indices(index):
            index = QPersistentModelIndex(index.parent())

        pending = [index]
        opened = 0

        while pending and opened < 400:
            current = pending.pop(0)
            nested = self.nested_indices(current)

            if nested:
                self.expanded.add(current)
                opened += 1
                pending.extend(nested)

        self.rebuild_layout()

    def return_to_parent(self) -> None:
        index = self.currentIndex().parent()

        if index.isValid() and index != self.rootIndex():
            self.select_index(index)

    def step(self, amount: int, column_only: bool = False) -> None:
        current = QPersistentModelIndex(self.currentIndex())

        if column_only:
            entries = next(
                (column.entries
                 for column in self.columns if current in column.entries),
                [],
            )
        else:
            entries = self.visible_outline()

        if current not in entries:
            return

        position = min(
            max(entries.index(current) + amount, 0),
            len(entries) - 1,
        )
        self.select_index(self.model_index(entries[position]))

    def step_end(self, last: bool) -> None:
        current = self.currentIndex()

        if not current.isValid():
            return

        nested = self.nested_indices(QPersistentModelIndex(current.parent()))

        if nested:
            self.select_index(
                self.model_index(nested[-1] if last else nested[0]))

    def preview_current(self) -> None:
        current = QPersistentModelIndex(self.currentIndex())

        if not current.isValid():
            return

        self.activated.emit(self.currentIndex())
        dialog = QDialog(self)
        dialog.setWindowTitle(self.label_for(current))
        dialog.resize(500, 240)
        layout = QVBoxLayout(dialog)
        label = QLabel(self.label_for(current), dialog)
        label.setTextFormat(Qt.TextFormat.PlainText)
        label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        label.setWordWrap(True)
        label.setFont(self.font())
        layout.addWidget(label)
        dialog.exec()

    def show_help(self) -> None:
        dialog = QDialog(self)
        dialog.setWindowTitle("Horizontal tree navigation")
        layout = QVBoxLayout(dialog)
        label = QLabel(
            "j / k: move through the open tree\n"
            "↑ / ↓: move within the current column\n"
            "J / K, Page Down / Page Up: jump ten entries\n"
            "g / G, Home / End: first / last sibling\n"
            "l / → / Enter: expand and enter, or preview\n"
            "h / ←: return to the parent\n"
            "Space / Tab: fold or unfold without entering\n"
            "c: fold the selected item\n"
            "C: fold everything outside the selection path\n"
            "e: expand the selected branch, up to 400 containers\n"
            "Click: select; click the selection again to open\n"
            "Wheel: navigate the column under the pointer\n"
            "Escape: return keyboard focus to the containing window",
            dialog,
        )
        label.setTextFormat(Qt.TextFormat.PlainText)
        layout.addWidget(label)
        dialog.exec()

    def keyPressEvent(self, event: QKeyEvent) -> None:
        self.camera_hold = None
        text = event.text()

        match text:
            case "j":
                self.step(1)
            case "k":
                self.step(-1)
            case "J":
                self.step(10)
            case "K":
                self.step(-10)
            case "g":
                self.step_end(False)
            case "G":
                self.step_end(True)
            case "l":
                self.open_current()
            case "h":
                self.return_to_parent()
            case "c":
                self.collapse_current()
            case "C":
                self.collapse_others()
            case "e":
                self.expand_current()
            case "?":
                self.show_help()
            case _:
                match event.key():
                    case Qt.Key.Key_Down:
                        self.step(1, column_only=True)
                    case Qt.Key.Key_Up:
                        self.step(-1, column_only=True)
                    case Qt.Key.Key_PageDown:
                        self.step(10, column_only=True)
                    case Qt.Key.Key_PageUp:
                        self.step(-10, column_only=True)
                    case Qt.Key.Key_Home:
                        self.step_end(False)
                    case Qt.Key.Key_End:
                        self.step_end(True)
                    case Qt.Key.Key_Right | Qt.Key.Key_Return | Qt.Key.Key_Enter:
                        self.open_current()
                    case Qt.Key.Key_Left:
                        self.return_to_parent()
                    case Qt.Key.Key_Space | Qt.Key.Key_Tab:
                        self.toggle_current()
                    case Qt.Key.Key_Escape:
                        self.clearFocus()
                    case _:
                        super().keyPressEvent(event)
                        return

        event.accept()

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        self.setFocus(Qt.FocusReason.MouseFocusReason)
        self.camera_hold = None
        index = self.indexAt(event.position().toPoint())

        if index.isValid():
            self.clicked.emit(index)

            if index == self.currentIndex():
                self.open_current()
            else:
                self.select_index(index)

        event.accept()

    def mouseDoubleClickEvent(self, event: QMouseEvent) -> None:
        event.accept()

    def wheelEvent(self, event: QWheelEvent) -> None:
        vertical = event.angleDelta().y()

        if vertical == 0:
            vertical = event.pixelDelta().y()

        if vertical == 0:
            event.ignore()
            return

        world_x = event.position().x() + self.camera.value
        column = next(
            (column for column in self.columns
             if column.left <= world_x < column.right),
            None,
        )

        if column is None:
            event.accept()
            return

        entries = [
            index for index in column.entries if index != self.root_key()
        ]

        if not entries:
            event.accept()
            return

        current = QPersistentModelIndex(self.currentIndex())
        self.camera_hold = self.camera_target

        if current not in entries:
            hit = QPersistentModelIndex(
                self.indexAt(event.position().toPoint()))
            destination = next(
                (index for index in entries if index in self.spine),
                min(
                    entries,
                    key=lambda index: abs(
                        self.screen_rect(self.items[index]).center().y() -
                        event.position().y()),
                ),
            )

            if hit in entries:
                destination = hit

            self.select_index(self.model_index(destination))
        else:
            self.step(-1 if 0 < vertical else 1, column_only=True)

        event.accept()

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self.rebuild_layout()

    def visualRect(self, index: QModelIndex) -> QRect:
        animated = self.items.get(QPersistentModelIndex(index))

        if animated is None or animated.departing:
            return QRect()

        return self.screen_rect(animated).toAlignedRect()

    def indexAt(self, point: QPoint) -> QModelIndex:
        if self.viewport().height() - 30 <= point.y():
            return QModelIndex()

        root = self.root_key()

        for animated in reversed(list(self.items.values())):
            if (animated.index != root and not animated.departing
                    and self.screen_rect(animated).contains(QPointF(point))):
                return self.model_index(animated.index)

        return QModelIndex()

    def scrollTo(
        self,
        index: QModelIndex,
        hint: QAbstractItemView.ScrollHint = (
            QAbstractItemView.ScrollHint.EnsureVisible),
    ) -> None:
        if not index.isValid():
            return

        persistent = QPersistentModelIndex(index)
        path = self.path_to(persistent)

        if not path:
            return

        for ancestor in path[:-1]:
            if ancestor != self.root_key():
                self.expanded.add(ancestor)

        self.anchor = persistent
        self.rebuild_layout()

    def horizontalOffset(self) -> int:
        return int(self.camera.value)

    def verticalOffset(self) -> int:
        return -int(self.selection_line())

    def isIndexHidden(self, index: QModelIndex) -> bool:
        return QPersistentModelIndex(index) not in self.placements

    def setSelection(
        self,
        rect: QRect,
        command: QItemSelectionModel.SelectionFlag,
    ) -> None:
        selection_model = self.selectionModel()

        if selection_model is None:
            return

        selection = QItemSelection()

        for index, animated in self.items.items():
            if (index != self.root_key() and not animated.departing
                    and self.screen_rect(animated).intersects(QRectF(rect))):
                ordinary = self.model_index(index)
                selection.select(ordinary, ordinary)

        selection_model.select(selection, command)

    def visualRegionForSelection(self, selection: QItemSelection) -> QRegion:
        region = QRegion()

        for index in selection.indexes():
            region = region.united(QRegion(self.visualRect(index)))

        return region

    def moveCursor(
        self,
        action: QAbstractItemView.CursorAction,
        modifiers: Qt.KeyboardModifier,
    ) -> QModelIndex:
        match action:
            case QAbstractItemView.CursorAction.MoveDown:
                self.step(1, column_only=True)
            case QAbstractItemView.CursorAction.MoveUp:
                self.step(-1, column_only=True)
            case QAbstractItemView.CursorAction.MoveNext:
                self.step(1)
            case QAbstractItemView.CursorAction.MovePrevious:
                self.step(-1)
            case QAbstractItemView.CursorAction.MovePageDown:
                self.step(10, column_only=True)
            case QAbstractItemView.CursorAction.MovePageUp:
                self.step(-10, column_only=True)
            case QAbstractItemView.CursorAction.MoveHome:
                self.step_end(False)
            case QAbstractItemView.CursorAction.MoveEnd:
                self.step_end(True)
            case QAbstractItemView.CursorAction.MoveLeft:
                self.return_to_parent()
            case QAbstractItemView.CursorAction.MoveRight:
                self.open_current()
            case _:
                pass

        return self.currentIndex()

    def updateGeometries(self) -> None:
        self.viewport().update()


@beartype
def demonstration_model() -> QStandardItemModel:
    model = QStandardItemModel()
    model.setObjectName("Workspace")
    model.setHorizontalHeaderLabels(["Item"])

    for name in ("Documents", "Research", "Projects", "Archive"):
        section = QStandardItem(name)
        section.setEditable(False)
        model.appendRow(section)

        for number in range(1, 5):
            group = QStandardItem(f"{name.lower()}-{number}")
            group.setEditable(False)
            section.appendRow(group)

            for entry in range(1, number + 3):
                item = QStandardItem(f"Entry {entry}")
                item.setEditable(False)
                group.appendRow(item)

                if entry == 2:
                    current = item

                    for depth in range(1, 6):
                        nested = QStandardItem(f"Level {depth}")
                        nested.setEditable(False)
                        current.appendRow(nested)
                        current.appendRow(
                            QStandardItem(f"Notes at level {depth}"))
                        current = nested

        section.appendRow(QStandardItem("Overview"))
        section.appendRow(QStandardItem("Summary"))

    return model


@beartype
def main() -> int:
    application = QApplication(sys.argv)
    window = QMainWindow()
    window.setWindowTitle("Horizontal model explorer")
    window.resize(1200, 760)

    view = HorizontalTreeView(window)
    model = demonstration_model()
    model.setParent(view)
    view.setModel(model)
    window.setCentralWidget(view)
    window.setVisible(True)
    view.setFocus()

    return application.exec()


if __name__ == "__main__":
    sys.exit(main())

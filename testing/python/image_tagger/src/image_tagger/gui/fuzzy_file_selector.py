#!/usr/bin/env python
from __future__ import annotations

import os
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

from rapidfuzz import fuzz
from PySide6.QtCore import QAbstractListModel
from PySide6.QtCore import QModelIndex
from PySide6.QtCore import QRect
from PySide6.QtCore import QSize
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor
from PySide6.QtGui import QFont
from PySide6.QtGui import QFontMetrics
from PySide6.QtGui import QKeySequence
from PySide6.QtGui import QPainter
from PySide6.QtGui import QShortcut
from PySide6.QtWidgets import QApplication
from PySide6.QtWidgets import QDialog
from PySide6.QtWidgets import QHBoxLayout
from PySide6.QtWidgets import QLabel
from PySide6.QtWidgets import QLineEdit
from PySide6.QtWidgets import QListView
from PySide6.QtWidgets import QMainWindow
from PySide6.QtWidgets import QPushButton
from PySide6.QtWidgets import QStyledItemDelegate
from PySide6.QtWidgets import QStyleOptionViewItem
from PySide6.QtWidgets import QVBoxLayout
from PySide6.QtWidgets import QWidget
from PySide6.QtWidgets import QStyle

PATH_ROLE = Qt.ItemDataRole.UserRole + 1
SPANS_ROLE = Qt.ItemDataRole.UserRole + 2
PINNED_ROLE = Qt.ItemDataRole.UserRole + 3
RECENT_ROLE = Qt.ItemDataRole.UserRole + 4
SCORE_ROLE = Qt.ItemDataRole.UserRole + 5


@dataclass(slots=True)
class Entry:
    path: Path
    rel_text: str
    full_text: str
    components: list[str]
    pinned: bool
    recent_rank: int


@dataclass(slots=True)
class MatchResult:
    entry: Entry
    score: float
    spans: list[tuple[int, int]]


def normalize_text(text: str) -> str:
    return text.casefold()


def fuzzy_positions(needle: str, haystack: str) -> list[int] | None:
    if not needle:
        return []

    n = normalize_text(needle)
    h = normalize_text(haystack)

    positions: list[int] = []
    start = 0
    for ch in n:
        idx = h.find(ch, start)
        if idx == -1:
            return None
        positions.append(idx)
        start = idx + 1
    return positions


def positions_to_spans(positions: Iterable[int]) -> list[tuple[int, int]]:
    sorted_positions = sorted(set(positions))
    if not sorted_positions:
        return []

    spans: list[tuple[int, int]] = []
    start = sorted_positions[0]
    prev = sorted_positions[0]

    for pos in sorted_positions[1:]:
        if pos == prev + 1:
            prev = pos
            continue
        spans.append((start, prev + 1))
        start = pos
        prev = pos

    spans.append((start, prev + 1))
    return spans


def component_offsets(rel_text: str) -> list[tuple[str, int]]:
    parts = rel_text.split("/")
    offsets: list[tuple[str, int]] = []
    cursor = 0
    for part in parts:
        offsets.append((part, cursor))
        cursor += len(part) + 1
    return offsets


def score_component_token(token: str,
                          component: str) -> tuple[float, list[int]] | None:
    positions = fuzzy_positions(token, component)
    if positions is None:
        return None

    subseq_bonus = 100.0
    fuzzy_bonus = float(
        fuzz.ratio(normalize_text(token), normalize_text(component)))
    prefix_bonus = (25.0 if normalize_text(component).startswith(
        normalize_text(token)) else 0.0)
    exact_bonus = 40.0 if normalize_text(component) == normalize_text(
        token) else 0.0

    compactness = 0.0
    if positions:
        span_len = positions[-1] - positions[0] + 1
        compactness = max(0.0, 20.0 - (span_len - len(token)) * 3.0)

    score = subseq_bonus + fuzzy_bonus * 0.35 + prefix_bonus + exact_bonus + compactness
    return score, positions


def score_entry(query: str, entry: Entry) -> MatchResult | None:
    tokens = [part for part in query.strip().split() if part]
    if not tokens:
        base_score = 0.0
        if entry.pinned:
            base_score += 10000.0
        if entry.recent_rank > 0:
            base_score += 500.0 - entry.recent_rank * 25.0
        return MatchResult(entry=entry, score=base_score, spans=[])

    offsets = component_offsets(entry.rel_text)
    used_indices: set[int] = set()
    all_positions: list[int] = []
    total_score = 0.0

    for token in tokens:
        best_component_index = -1
        best_component_score = -1.0
        best_positions: list[int] = []

        for idx, (component, offset) in enumerate(offsets):
            component_match = score_component_token(token, component)
            if component_match is None:
                continue

            component_score, rel_positions = component_match

            if idx not in used_indices:
                component_score += 15.0

            if idx == len(offsets) - 1:
                component_score += 20.0

            if component_score > best_component_score:
                best_component_index = idx
                best_component_score = component_score
                best_positions = [offset + pos for pos in rel_positions]

        if best_component_index == -1:
            full_positions = fuzzy_positions(token, entry.rel_text)
            if full_positions is None:
                return None

            full_ratio = float(
                fuzz.partial_ratio(normalize_text(token),
                                   normalize_text(entry.rel_text)))
            total_score += 40.0 + full_ratio * 0.25
            all_positions.extend(full_positions)
            continue

        used_indices.add(best_component_index)
        total_score += best_component_score
        all_positions.extend(best_positions)

    basename = entry.components[-1]
    basename_ratio = float(
        fuzz.partial_ratio(normalize_text(query), normalize_text(basename)))
    path_ratio = float(
        fuzz.partial_ratio(
            normalize_text(query.replace(" ", "")),
            normalize_text(entry.rel_text.replace("/", "")),
        ))
    total_score += basename_ratio * 0.2 + path_ratio * 0.15

    if entry.pinned:
        total_score += 10000.0

    if entry.recent_rank > 0:
        total_score += 500.0 - entry.recent_rank * 25.0

    total_score -= len(entry.components) * 2.0
    total_score -= len(entry.rel_text) * 0.03

    spans = positions_to_spans(all_positions)
    return MatchResult(entry=entry, score=total_score, spans=spans)


class MatchModel(QAbstractListModel):

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._items: list[MatchResult] = []

    def set_items(self, items: list[MatchResult]) -> None:
        self.beginResetModel()
        self._items = items
        self.endResetModel()

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid():
            return 0
        return len(self._items)

    def data(self,
             index: QModelIndex,
             role: int = Qt.ItemDataRole.DisplayRole) -> object:
        if not index.isValid():
            return None

        item = self._items[index.row()]
        if role == Qt.ItemDataRole.DisplayRole:
            return item.entry.rel_text
        if role == PATH_ROLE:
            return str(item.entry.path)
        if role == SPANS_ROLE:
            return item.spans
        if role == PINNED_ROLE:
            return item.entry.pinned
        if role == RECENT_ROLE:
            return item.entry.recent_rank > 0
        if role == SCORE_ROLE:
            return item.score
        return None

    def item_at(self, row: int) -> MatchResult:
        return self._items[row]


class MatchDelegate(QStyledItemDelegate):

    def sizeHint(self, option: QStyleOptionViewItem,
                 index: QModelIndex) -> QSize:
        return QSize(option.rect.width(), 28)

    def paint(self, painter: QPainter, option: QStyleOptionViewItem,
              index: QModelIndex) -> None:
        painter.save()

        rect = option.rect
        selected = bool(option.state & QStyle.StateFlag.State_Selected)

        if selected:
            painter.fillRect(rect, option.palette.highlight())
            text_color = option.palette.highlightedText().color()
            sub_color = QColor(text_color)
            sub_color.setAlpha(180)
            match_color = QColor(255, 215, 0)
        else:
            painter.fillRect(rect, option.palette.base())
            text_color = option.palette.text().color()
            sub_color = option.palette.mid().color()
            match_color = QColor(220, 160, 20)

        rel_text = str(index.data(Qt.ItemDataRole.DisplayRole))
        path_text = str(index.data(PATH_ROLE))
        spans = index.data(SPANS_ROLE) or []
        pinned = bool(index.data(PINNED_ROLE))
        recent = bool(index.data(RECENT_ROLE))

        left = rect.left() + 10
        top = rect.top()
        width = rect.width() - 20
        height = rect.height()

        badge_parts: list[str] = []
        if pinned:
            badge_parts.append("PIN")
        if recent:
            badge_parts.append("RECENT")
        badge_text = "  ".join(badge_parts)

        main_font = option.font
        bold_font = QFont(main_font)
        bold_font.setBold(True)

        meta_font = QFont(main_font)
        meta_font.setPointSize(max(8, meta_font.pointSize() - 1))

        fm = QFontMetrics(main_font)
        meta_fm = QFontMetrics(meta_font)

        if badge_text:
            badge_width = meta_fm.horizontalAdvance(badge_text)
        else:
            badge_width = 0

        text_right = left + width - badge_width - 10
        baseline = top + height // 2 + fm.ascent() // 2 - 2

        painter.setClipRect(rect)

        x = left
        cursor = 0
        span_iter = iter(spans)
        current_span = next(span_iter, None)

        while cursor < len(rel_text):
            if current_span is not None and cursor == current_span[0]:
                segment = rel_text[current_span[0]:current_span[1]]
                painter.setFont(bold_font)
                painter.setPen(match_color)
                seg_width = QFontMetrics(bold_font).horizontalAdvance(segment)
                if x + seg_width <= text_right:
                    painter.drawText(x, baseline, segment)
                x += seg_width
                cursor = current_span[1]
                current_span = next(span_iter, None)
            else:
                next_stop = (current_span[0]
                             if current_span is not None else len(rel_text))
                segment = rel_text[cursor:next_stop]
                painter.setFont(main_font)
                painter.setPen(text_color)
                seg_width = fm.horizontalAdvance(segment)
                if x + seg_width <= text_right:
                    painter.drawText(x, baseline, segment)
                x += seg_width
                cursor = next_stop

        painter.setFont(meta_font)
        painter.setPen(sub_color)
        sub_text = path_text
        sub_y = rect.bottom() - 6
        elided_sub = meta_fm.elidedText(sub_text, Qt.TextElideMode.ElideMiddle,
                                        width)
        painter.drawText(
            QRect(left, top + 2, width, height),
            Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignBottom,
            elided_sub,
        )

        if badge_text:
            painter.setFont(meta_font)
            painter.setPen(match_color if selected else QColor(90, 140, 220))
            badge_rect = QRect(text_right + 5, top, badge_width + 5, height)
            painter.drawText(
                badge_rect,
                Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignRight,
                badge_text,
            )

        painter.restore()


class PaletteDialog(QDialog):

    def __init__(self,
                 entries: list[Entry],
                 parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.entries = entries
        self.model = MatchModel(self)
        self.selected_path: Path | None = None

        self.setWindowTitle("Command Palette")
        self.setModal(True)
        self.resize(900, 520)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(10, 10, 10, 10)

        self.input = QLineEdit(self)
        self.input.setPlaceholderText(
            "Type path query, e.g. 'doc note', 'wo', 'rep py' ...")
        layout.addWidget(self.input)

        self.list = QListView(self)
        self.list.setModel(self.model)
        self.list.setItemDelegate(MatchDelegate(self.list))
        self.list.setUniformItemSizes(True)
        self.list.setEditTriggers(QListView.EditTrigger.NoEditTriggers)
        self.list.setSelectionBehavior(QListView.SelectionBehavior.SelectRows)
        self.list.setSelectionMode(QListView.SelectionMode.SingleSelection)
        layout.addWidget(self.list)

        self.path_label = QLabel(self)
        self.path_label.setTextInteractionFlags(
            Qt.TextInteractionFlag.TextSelectableByMouse)
        layout.addWidget(self.path_label)

        self.input.textChanged.connect(self.refresh)
        self.list.activated.connect(self.accept_current)
        self.list.clicked.connect(self.update_path_label)

        self.refresh("")
        self.input.setFocus()

    def refresh(self, query: str) -> None:
        matches: list[MatchResult] = []
        for entry in self.entries:
            result = score_entry(query, entry)
            if result is not None:
                matches.append(result)

        matches.sort(key=lambda item: (
            -item.score,
            item.entry.rel_text,
        ))
        matches = matches[:100]
        self.model.set_items(matches)

        if matches:
            self.list.setCurrentIndex(self.model.index(0, 0))
            self.update_path_label(self.model.index(0, 0))
        else:
            self.path_label.setText("No matches")

    def update_path_label(self, index: QModelIndex) -> None:
        if not index.isValid():
            self.path_label.setText("")
            return
        self.path_label.setText(str(index.data(PATH_ROLE)))

    def keyPressEvent(self, event) -> None:
        key = event.key()

        if key == Qt.Key.Key_Down:
            current = self.list.currentIndex().row()
            next_row = min(current + 1, self.model.rowCount() - 1)
            if next_row >= 0:
                index = self.model.index(next_row, 0)
                self.list.setCurrentIndex(index)
                self.update_path_label(index)
            return

        if key == Qt.Key.Key_Up:
            current = self.list.currentIndex().row()
            next_row = max(current - 1, 0)
            if self.model.rowCount() > 0:
                index = self.model.index(next_row, 0)
                self.list.setCurrentIndex(index)
                self.update_path_label(index)
            return

        if key in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            self.accept_current()
            return

        if key == Qt.Key.Key_Escape:
            self.reject()
            return

        super().keyPressEvent(event)

    def accept_current(self) -> None:
        index = self.list.currentIndex()
        if not index.isValid():
            return
        self.selected_path = Path(str(index.data(PATH_ROLE)))
        self.accept()


def build_directory_entries(
    root: Path,
    pinned_paths: set[str],
    recent_paths: list[str],
) -> list[Entry]:
    """Build palette entries for all directories under root."""
    entries: list[Entry] = []
    recent_map = {path: idx + 1 for idx, path in enumerate(recent_paths)}

    def add_dir(path: Path) -> None:
        try:
            rel = path.relative_to(root)
            rel_text = str(rel).replace(os.sep, "/")
        except ValueError:
            rel_text = path.name or str(path)

        path_str = str(path)
        entries.append(
            Entry(
                path=path,
                rel_text=rel_text,
                full_text=path_str,
                components=rel_text.split("/"),
                pinned=path_str in pinned_paths,
                recent_rank=recent_map.get(path_str, 0),
            ))

    if root.is_dir():
        add_dir(root)
        try:
            for item in sorted(root.rglob("*")):
                if item.is_dir():
                    add_dir(item)
        except (PermissionError, OSError):
            pass

    return entries


class MainWindow(QMainWindow):

    def __init__(self, entries: list[Entry]) -> None:
        super().__init__()
        self.entries = entries

        self.setWindowTitle("PySide6 Command Palette POC")
        self.resize(900, 500)

        central = QWidget(self)
        self.setCentralWidget(central)

        layout = QVBoxLayout(central)

        info = QLabel(self)
        info.setText(
            "Press Ctrl+P to open the palette.\n"
            "Examples: 'doc', 'wor', 'temp inter', 'rep', 'not', 'tor', 'pdf'."
        )
        layout.addWidget(info)

        self.result_label = QLabel("Selected path: <none>", self)
        self.result_label.setTextInteractionFlags(
            Qt.TextInteractionFlag.TextSelectableByMouse)
        layout.addWidget(self.result_label)

        open_button = QPushButton("Open Palette", self)
        open_button.clicked.connect(self.open_palette)
        layout.addWidget(open_button)

        layout.addStretch()

        shortcut = QShortcut(QKeySequence("Ctrl+P"), self)
        shortcut.activated.connect(self.open_palette)

    def open_palette(self) -> None:
        dialog = PaletteDialog(self.entries, self)
        if dialog.exec():
            assert dialog.selected_path is not None
            self.result_label.setText(f"Selected path: {dialog.selected_path}")


def build_entries(root: Path) -> list[Entry]:
    names = sorted([item.name for item in root.iterdir() if item.is_dir()])

    pinned_names = {
        "documents",
        "notes",
        "repos",
        "workspace",
    }

    recent_order = [
        "workspace",
        "repos",
        "notes",
        "temporary_interchange",
        "documents",
        "downloads",
    ]

    recent_map = {name: idx + 1 for idx, name in enumerate(recent_order)}

    entries: list[Entry] = []
    for name in names:
        path = root / name
        rel_text = name
        entries.append(
            Entry(
                path=path,
                rel_text=rel_text,
                full_text=str(path),
                components=rel_text.split("/"),
                pinned=name in pinned_names,
                recent_rank=recent_map.get(name, 0),
            ))

    nested_examples = [
        root / "documents" / "work" / "reports",
        root / "documents" / "personal" / "letters",
        root / "notes" / "projects" / "ideas",
        root / "notes" / "daily" / "journal",
        root / "repos" / "python" / "pyside_palette",
        root / "repos" / "cpp" / "render_lab",
        root / "workspace" / "active" / "client_a",
        root / "workspace" / "active" / "prototype_ui",
        root / "temporary_interchange" / "incoming",
        root / "temporary_interchange" / "outgoing",
        root / "pdf" / "manuals",
        root / "images" / "screenshots",
        root / "videos" / "editing",
        root / "music" / "albums",
        root / "archive" / "2023",
        root / "archive" / "2024",
    ]

    extra_pinned = {
        str(root / "workspace" / "active" / "prototype_ui"),
        str(root / "repos" / "python" / "pyside_palette"),
    }

    extra_recent = [
        str(root / "workspace" / "active" / "prototype_ui"),
        str(root / "repos" / "python" / "pyside_palette"),
        str(root / "documents" / "work" / "reports"),
        str(root / "temporary_interchange" / "incoming"),
    ]
    extra_recent_map = {name: idx + 1 for idx, name in enumerate(extra_recent)}

    for path in nested_examples:
        rel_text = str(path.relative_to(root)).replace(os.sep, "/")
        entries.append(
            Entry(
                path=path,
                rel_text=rel_text,
                full_text=str(path),
                components=rel_text.split("/"),
                pinned=str(path) in extra_pinned,
                recent_rank=extra_recent_map.get(str(path), 0),
            ))

    return entries


def main() -> int:
    root = Path.home() / "defaultdirs"
    if not root.is_dir():
        raise RuntimeError(f"Expected directory does not exist: {root}")

    app = QApplication(sys.argv)
    entries = build_entries(root)
    window = MainWindow(entries)
    window.show()
    return app.exec()


if __name__ == "__main__":
    raise SystemExit(main())

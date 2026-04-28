from __future__ import annotations

from beartype import beartype
from pathlib import Path
from PySide6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QHBoxLayout,
    QPushButton,
    QDoubleSpinBox,
    QLineEdit,
    QCompleter,
    QLabel,
    QScrollArea,
    QFrame,
    QSizePolicy,
)
from PySide6.QtCore import Qt, Signal, QStringListModel
from sqlalchemy.orm import Session
from sqlalchemy import select

from image_tagger.db.models import ProbabilisticTag


@beartype
class WeightedTagRow(QWidget):
    removed = Signal(object)

    def __init__(self, session: Session, parent=None):
        super().__init__(parent)
        self.session = session

        layout = QHBoxLayout(self)
        layout.setContentsMargins(4, 2, 4, 2)
        layout.setSpacing(4)

        self.tag_input = QLineEdit()
        self.tag_input.setPlaceholderText("tag name")
        self.tag_input.setMinimumWidth(120)

        self._completer = QCompleter()
        self._completer.setCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)
        self._completer.setFilterMode(Qt.MatchFlag.MatchContains)
        self.tag_input.setCompleter(self._completer)

        self.weight_spin = QDoubleSpinBox()
        self.weight_spin.setRange(-9999.0, 9999.0)
        self.weight_spin.setValue(1.0)
        self.weight_spin.setDecimals(2)
        self.weight_spin.setSingleStep(0.1)
        self.weight_spin.setMinimumWidth(80)

        self.remove_btn = QPushButton("x")
        self.remove_btn.setMaximumWidth(28)
        self.remove_btn.setToolTip("Remove this tag")
        self.remove_btn.clicked.connect(self._on_remove)

        layout.addWidget(self.tag_input)
        layout.addWidget(self.weight_spin)
        layout.addWidget(self.remove_btn)

        self._load_tag_names()

    def _load_tag_names(self):
        names = sorted(r[0] for r in self.session.execute(
            select(ProbabilisticTag.name).distinct()).all() if r[0])
        self._completer.setModel(QStringListModel(names))

    def _on_remove(self):
        self.removed.emit(self)

    def get_tag_name(self) -> str:
        return self.tag_input.text().strip()

    def get_weight(self) -> float:
        return self.weight_spin.value()

    def set_values(self, tag_name: str, weight: float) -> None:
        self.tag_input.setText(tag_name)
        self.weight_spin.setValue(weight)


@beartype
class WeightedTagSortWidget(QWidget):
    sortRequested = Signal()

    def __init__(self, session: Session, parent=None):
        super().__init__(parent)
        self.session = session

        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(4)

        header = QLabel("Weighted Tag Sort")
        header.setStyleSheet("font-weight: bold;")
        layout.addWidget(header)

        self.rows_container = QWidget()
        self.rows_layout = QVBoxLayout(self.rows_container)
        self.rows_layout.setContentsMargins(0, 0, 0, 0)
        self.rows_layout.setSpacing(2)
        self.rows_layout.addStretch(1)

        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setWidget(self.rows_container)
        layout.addWidget(scroll)

        btn_layout = QHBoxLayout()

        self.add_btn = QPushButton("+")
        self.add_btn.setToolTip("Add a new weighted tag")
        self.add_btn.clicked.connect(lambda: self.add_row())

        self.sort_btn = QPushButton("RE-sort")
        self.sort_btn.setToolTip("Rebuild index and resort mixed view")
        self.sort_btn.clicked.connect(self.sortRequested.emit)

        btn_layout.addWidget(self.add_btn)
        btn_layout.addWidget(self.sort_btn)
        btn_layout.addStretch(1)

        layout.addLayout(btn_layout)

        self._rows: list[WeightedTagRow] = []

    def add_row(self,
                tag_name: str = "",
                weight: float = 1.0) -> WeightedTagRow:
        row = WeightedTagRow(self.session, self.rows_container)
        row.set_values(tag_name, weight)
        row.removed.connect(self._remove_row)
        self.rows_layout.insertWidget(len(self._rows), row)
        self._rows.append(row)
        return row

    def _remove_row(self, row: WeightedTagRow):
        if row in self._rows:
            self._rows.remove(row)
            row.deleteLater()

    def get_weighted_tags(self) -> dict[str, float]:
        result: dict[str, float] = {}
        for row in self._rows:
            name = row.get_tag_name()
            if name:
                result[name] = row.get_weight()
        return result

    def get_tag_id_weights(self) -> dict[int, float]:
        result: dict[int, float] = {}
        for row in self._rows:
            name = row.get_tag_name()
            if not name:
                continue
            tag = self.session.scalar(
                select(ProbabilisticTag).where(ProbabilisticTag.name == name))
            if tag is not None:
                result[tag.id] = row.get_weight()
        return result

    def set_entries(self, entries: list[tuple[str, float]]) -> None:
        for row in list(self._rows):
            self._remove_row(row)
        for tag_name, weight in entries:
            self.add_row(tag_name, weight)

    def get_entries(self) -> list[tuple[str, float]]:
        return [(row.get_tag_name(), row.get_weight()) for row in self._rows
                if row.get_tag_name()]

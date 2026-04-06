from __future__ import annotations

from PySide6.QtCore import Signal, Qt
from PySide6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QHBoxLayout,
    QLabel,
    QTableWidget,
    QTableWidgetItem,
    QPushButton,
    QLineEdit,
    QDoubleSpinBox,
    QTextEdit,
    QScrollArea,
    QFrame,
)

from gui.flow_layout import FlowLayout


class TagWidget(QFrame):
    addSearchRequested = Signal(str, str)

    def __init__(self, category: str, name: str, parent=None):
        super().__init__(parent)
        self.category = category
        self.name_ = name

        self.setFrameShape(QFrame.Shape.StyledPanel)
        self.setStyleSheet(
            "TagWidget { background-color: #e0e0e0; border-radius: 4px; border: 1px solid #ccc; }"
        )

        layout = QHBoxLayout(self)
        layout.setContentsMargins(6, 2, 6, 2)
        layout.setSpacing(4)

        self.label = QLabel(f"{category}:{name}")
        self.label.setStyleSheet("border: none; background: transparent;")
        layout.addWidget(self.label)

        self.add_btn = QPushButton("+")
        self.add_btn.setFixedSize(16, 16)
        self.add_btn.setStyleSheet(
            "QPushButton { color: white; background-color: #4CAF50; border: none; border-radius: 8px; font-weight: bold; padding-bottom: 2px; }"
            "QPushButton:hover { background-color: #45a049; }"
        )
        self.add_btn.hide()
        self.add_btn.clicked.connect(self._on_add_clicked)
        layout.addWidget(self.add_btn)

        self.close_btn = QPushButton("x")
        self.close_btn.setFixedSize(16, 16)
        self.close_btn.setStyleSheet(
            "QPushButton { color: white; background-color: #ff4444; border: none; border-radius: 8px; font-weight: bold; padding-bottom: 2px; }"
            "QPushButton:hover { background-color: #cc0000; }"
        )
        self.close_btn.hide()
        layout.addWidget(self.close_btn)

        self.setAttribute(Qt.WidgetAttribute.WA_Hover)

    def _on_add_clicked(self):
        self.addSearchRequested.emit(self.category, self.name_)

    def enterEvent(self, event):
        self.add_btn.show()
        self.close_btn.show()
        super().enterEvent(event)

    def leaveEvent(self, event):
        self.add_btn.hide()
        self.close_btn.hide()
        super().leaveEvent(event)


class RegularTagsContainer(QWidget):
    tagRemoveRequested = Signal(str, str)
    tagAddSearchRequested = Signal(str, str)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.root_layout = QVBoxLayout(self)
        self.flow_host = QWidget()
        self.flow = FlowLayout(self.flow_host)
        self.flow_host.setLayout(self.flow)
        self.root_layout.addWidget(self.flow_host)

    def clear_tags(self):
        while self.flow.count():
            item = self.flow.takeAt(0)
            if item and item.widget():
                item.widget().deleteLater()

    def set_tags(self, tags: list[tuple[str, str]]):
        self.clear_tags()
        for category, name in tags:
            tag_widget = TagWidget(category, name)
            tag_widget.close_btn.clicked.connect(
                lambda checked=False, c=category, n=name: self.tagRemoveRequested.emit(
                    c, n
                )
            )
            tag_widget.addSearchRequested.connect(
                lambda c, n: self.tagAddSearchRequested.emit(c, n)
            )
            self.flow.addWidget(tag_widget)


class CenterPanel(QWidget):
    probabilisticTagAdded = Signal(str, float)
    probabilisticTagDeleted = Signal(str)
    regularTagAdded = Signal(str, str)
    regularTagDeleted = Signal(str, str)
    descriptionSaved = Signal(str)

    probTagSearchRequested = Signal(str, str)
    regTagSearchRequested = Signal(str, str)

    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)

        layout.addWidget(QLabel("Probabilistic tags"))
        self.prob_table = QTableWidget(0, 3)
        self.prob_table.setHorizontalHeaderLabels(["+", "Tag", "Probability"])
        self.prob_table.horizontalHeader().setStretchLastSection(True)
        self.prob_table.setColumnWidth(0, 30)
        layout.addWidget(self.prob_table)

        prob_add_row = QHBoxLayout()
        self.prob_name_edit = QLineEdit()
        self.prob_name_edit.setPlaceholderText("tag name")
        self.prob_value_spin = QDoubleSpinBox()
        self.prob_value_spin.setRange(0.0, 1.0)
        self.prob_value_spin.setSingleStep(0.01)
        self.prob_value_spin.setValue(0.5)
        self.prob_add_btn = QPushButton("Add probabilistic tag")
        prob_add_row.addWidget(self.prob_name_edit)
        prob_add_row.addWidget(self.prob_value_spin)
        prob_add_row.addWidget(self.prob_add_btn)
        layout.addLayout(prob_add_row)

        layout.addWidget(QLabel("Regular tags"))
        self.regular_tags = RegularTagsContainer()
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setWidget(self.regular_tags)
        layout.addWidget(scroll)

        reg_add_row = QHBoxLayout()
        self.reg_category_edit = QLineEdit()
        self.reg_category_edit.setPlaceholderText("category")
        self.reg_name_edit = QLineEdit()
        self.reg_name_edit.setPlaceholderText("tag")
        self.reg_add_btn = QPushButton("Add regular tag")
        reg_add_row.addWidget(self.reg_category_edit)
        reg_add_row.addWidget(self.reg_name_edit)
        reg_add_row.addWidget(self.reg_add_btn)
        layout.addLayout(reg_add_row)

        layout.addWidget(QLabel("Description"))
        self.description_edit = QTextEdit()
        layout.addWidget(self.description_edit)

        self.save_description_btn = QPushButton("Save description")
        layout.addWidget(self.save_description_btn)

        self.prob_add_btn.clicked.connect(self._on_prob_add)
        self.reg_add_btn.clicked.connect(self._on_reg_add)
        self.save_description_btn.clicked.connect(self._on_save_desc)
        self.regular_tags.tagRemoveRequested.connect(self.regularTagDeleted)
        self.regular_tags.tagAddSearchRequested.connect(self.regTagSearchRequested)

    def _on_prob_add(self):
        name = self.prob_name_edit.text().strip()
        if name:
            self.probabilisticTagAdded.emit(name, self.prob_value_spin.value())
            self.prob_name_edit.clear()

    def _on_reg_add(self):
        category = self.reg_category_edit.text().strip()
        name = self.reg_name_edit.text().strip()
        if category and name:
            self.regularTagAdded.emit(category, name)
            self.reg_name_edit.clear()

    def _on_save_desc(self):
        self.descriptionSaved.emit(self.description_edit.toPlainText())

    def set_probabilistic_tags(self, tags: list[tuple[str, str, float]]):
        self.prob_table.setRowCount(len(tags))
        for row, (category, tag_name, probability) in enumerate(tags):
            add_btn = QPushButton("+")
            add_btn.setStyleSheet(
                "QPushButton { color: white; background-color: #4CAF50; border: none; font-weight: bold; margin: 2px; }"
                "QPushButton:hover { background-color: #45a049; }"
            )
            add_btn.clicked.connect(
                lambda checked=False,
                c=category,
                t=tag_name: self.probTagSearchRequested.emit(c, t)
            )

            self.prob_table.setCellWidget(row, 0, add_btn)
            self.prob_table.setItem(row, 1, QTableWidgetItem(tag_name))
            self.prob_table.setItem(row, 2, QTableWidgetItem(f"{probability:.4f}"))

    def set_regular_tags(self, tags: list[tuple[str, str]]):
        self.regular_tags.set_tags(tags)

    def set_description(self, text: str):
        self.description_edit.setPlainText(text)

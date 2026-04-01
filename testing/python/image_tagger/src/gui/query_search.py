from __future__ import annotations
from typing import Optional

from PySide6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QScrollArea,
    QPushButton,
    QLabel,
    QFrame,
    QComboBox,
    QDoubleSpinBox,
    QLineEdit,
    QHBoxLayout,
    QFormLayout,
    QSplitter,
    QAbstractItemView,
    QCompleter,
)
from PySide6.QtCore import Qt, Signal, QStringListModel
from pathlib import Path
import os

from gui.image_list_widget import ImageListWidget
from sqlalchemy.orm import Session
from sqlalchemy import select
from db.models import (
    ImageEntry,
    ProbabilisticTag,
    ImageProbabilisticTag,
    RegularTag,
    ImageRegularTag,
    ImageDescription,
)


class ImageThumbnailList(ImageListWidget):
    """Displays image thumbnails in an icon-mode list."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.list_view.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection
        )

    def set_images(self, images: list):
        """Set the images to display in the thumbnail list."""
        from pathlib import Path

        # Accept list or sequence of paths
        image_paths = [Path(p) if isinstance(p, str) else p for p in images]
        self.model.set_images(image_paths)
        self.list_view.clearSelection()

    def get_selected_images(self) -> list[Path]:
        """Return the selected images as a list of Path objects."""
        indexes = self.list_view.selectedIndexes()
        return [self.model.images[idx.row()] for idx in indexes]

    def set_selection(self, paths: set[Path]) -> None:
        """Set the selection to the given set of file paths."""
        from PySide6.QtCore import QItemSelectionModel

        self.list_view.clearSelection()
        selection_model = self.list_view.selectionModel()

        for path in paths:
            # Convert to string for comparison if needed
            path_str = str(path)
            for idx, img_path in enumerate(self.model.images):
                if img_path == path or str(img_path) == path_str:
                    index = self.model.index(idx, 0)
                    selection_model.select(
                        index, QItemSelectionModel.SelectionFlag.Select
                    )
                    break


class TagCompleter(QLineEdit):
    """A line edit with auto-completion for tag names."""

    def __init__(self, suggestions: list[str], parent=None):
        super().__init__(parent)

        self._completer = QCompleter(suggestions)
        self._completer.setCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)
        self._completer.setFilterMode(Qt.MatchFlag.MatchContains)
        self.setCompleter(self._completer)

    def set_suggestions(self, suggestions: list[str]):
        self._completer.setModel(QStringListModel(suggestions))


class ConditionWidget(QFrame):
    """Widget for a single filter condition."""

    changed = Signal()
    remove_requested = Signal()

    def __init__(self, session: Session, parent=None):
        super().__init__(parent)
        self.session = session
        self.setFrameStyle(QFrame.Shape.StyledPanel | QFrame.Shadow.Raised)
        self._build_ui()

    def _build_ui(self):
        layout = QFormLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)

        self.condition_type = QComboBox()
        self.condition_type.addItems(
            [
                "has probabilistic tag",
                "has regular tag",
                "has description containing",
            ]
        )
        self.condition_type.currentIndexChanged.connect(self._on_type_changed)
        layout.addRow("type:", self.condition_type)

        self.category_combo = QComboBox()
        self.category_combo.setEditable(True)
        self.category_combo.setMinimumWidth(100)
        self.category_combo.currentTextChanged.connect(self._on_category_changed)
        layout.addRow("category:", self.category_combo)
        self.category_label = layout.labelForField(self.category_combo)

        self.tag_input = TagCompleter([])
        self.tag_input.setMinimumWidth(200)
        self.tag_input.textChanged.connect(lambda: self.changed.emit())
        layout.addRow("tag:", self.tag_input)
        self.tag_label = layout.labelForField(self.tag_input)

        self.prob_spin = QDoubleSpinBox()
        self.prob_spin.setRange(0.0, 1.0)
        self.prob_spin.setSingleStep(0.05)
        self.prob_spin.setValue(0.5)
        self.prob_spin.setDecimals(2)
        self.prob_spin.valueChanged.connect(lambda: self.changed.emit())
        layout.addRow("confidence ≥", self.prob_spin)
        self.prob_label = layout.labelForField(self.prob_spin)

        self.desc_input = QLineEdit()
        self.desc_input.setMinimumWidth(200)
        self.desc_input.textChanged.connect(lambda: self.changed.emit())
        layout.addRow("text:", self.desc_input)
        self.desc_label = layout.labelForField(self.desc_input)

        remove_btn = QPushButton("✕")
        remove_btn.setFixedWidth(30)
        remove_btn.clicked.connect(self.remove_requested.emit)
        layout.addRow(remove_btn)

        self._load_suggestions()
        self._on_type_changed(0)

    def _load_suggestions(self):
        prob_cats = sorted(
            set(
                r[0]
                for r in self.session.execute(
                    select(ProbabilisticTag.category).distinct()
                ).all()
            )
        )
        reg_cats = sorted(
            set(
                r[0]
                for r in self.session.execute(
                    select(RegularTag.category).distinct()
                ).all()
            )
        )
        self._prob_categories = prob_cats
        self._reg_categories = reg_cats
        self._update_categories()

    def _update_categories(self):
        idx = self.condition_type.currentIndex()
        self.category_combo.clear()
        if idx == 0:
            self.category_combo.addItems(self._prob_categories)
        elif idx == 1:
            self.category_combo.addItems(self._reg_categories)

    def _on_type_changed(self, idx):
        is_prob = idx == 0
        is_reg = idx == 1
        is_desc = idx == 2

        self.category_label.setVisible(is_prob or is_reg)
        self.category_combo.setVisible(is_prob or is_reg)
        self.tag_label.setVisible(is_prob or is_reg)
        self.tag_input.setVisible(is_prob or is_reg)
        self.prob_label.setVisible(is_prob)
        self.prob_spin.setVisible(is_prob)
        self.desc_label.setVisible(is_desc)
        self.desc_input.setVisible(is_desc)

        if is_prob or is_reg:
            self.category_label.show()
            self.tag_label.show()
        else:
            self.category_label.hide()
            self.tag_label.hide()
        if is_prob:
            self.prob_label.show()
        else:
            self.prob_label.hide()
        if is_desc:
            self.desc_label.show()
        else:
            self.desc_label.hide()

        self._update_categories()
        self._on_category_changed()
        self.changed.emit()

    def _on_category_changed(self):
        idx = self.condition_type.currentIndex()
        cat = self.category_combo.currentText()
        if idx == 0:
            names = sorted(
                r[0]
                for r in self.session.execute(
                    select(ProbabilisticTag.name).where(
                        ProbabilisticTag.category == cat
                    )
                ).all()
            )
        elif idx == 1:
            names = sorted(
                r[0]
                for r in self.session.execute(
                    select(RegularTag.name).where(RegularTag.category == cat)
                ).all()
            )
        else:
            names = []
        self.tag_input.set_suggestions(names)
        self.changed.emit()

    def to_filter_spec(self) -> Optional[dict]:
        idx = self.condition_type.currentIndex()
        if idx == 0:
            tag_name = self.tag_input.text().strip()
            if not tag_name:
                return None
            return {
                "type": "probabilistic_tag",
                "category": self.category_combo.currentText(),
                "name": tag_name,
                "min_probability": self.prob_spin.value(),
            }
        elif idx == 1:
            tag_name = self.tag_input.text().strip()
            if not tag_name:
                return None
            return {
                "type": "regular_tag",
                "category": self.category_combo.currentText(),
                "name": tag_name,
            }
        elif idx == 2:
            text = self.desc_input.text().strip()
            if not text:
                return None
            return {
                "type": "description",
                "text": text,
            }
        return None


class ExpressionNode(QFrame):
    """A node in the boolean expression tree. Can be a condition or a logical group."""

    changed = Signal()
    remove_requested = Signal()

    def __init__(self, session: Session, is_root=False, parent=None):
        super().__init__(parent)
        self.session = session
        self.is_root = is_root
        self.children: list[ExpressionNode] = []
        self.condition: Optional[ConditionWidget] = None
        self.is_group = True

        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)
        self._build_ui()

    def _build_ui(self):
        self.main_layout = QVBoxLayout(self)
        self.main_layout.setContentsMargins(4, 4, 4, 4)
        self.main_layout.setSpacing(2)

        header = QVBoxLayout()
        header.setContentsMargins(0, 0, 0, 0)
        h_row = QHBoxLayout()
        h_row.setContentsMargins(0, 0, 0, 0)
        h_row.setSpacing(4)

        self.operator_combo = QComboBox()
        self.operator_combo.addItems(["AND", "OR", "NOT"])
        self.operator_combo.currentIndexChanged.connect(self._on_operator_changed)
        h_row.addWidget(QLabel("Operator:"))
        h_row.addWidget(self.operator_combo)

        add_cond_btn = QPushButton("+ Condition")
        add_cond_btn.clicked.connect(self._add_condition_child)
        h_row.addWidget(add_cond_btn)

        add_group_btn = QPushButton("+ Group")
        add_group_btn.clicked.connect(self._add_group_child)
        h_row.addWidget(add_group_btn)

        h_row.addStretch()

        if not self.is_root:
            remove_btn = QPushButton("✕ Remove Group")
            remove_btn.clicked.connect(self.remove_requested.emit)
            h_row.addWidget(remove_btn)

        header.addLayout(h_row)
        self.main_layout.addLayout(header)

        self.children_layout = QVBoxLayout()
        self.children_layout.setContentsMargins(8, 0, 0, 0)
        self.children_layout.setSpacing(2)
        self.main_layout.addLayout(self.children_layout)

    def _on_operator_changed(self):
        self.changed.emit()

    def _add_condition_child(self):
        node = ExpressionNode(self.session, is_root=False)
        node.is_group = False
        node._convert_to_condition()
        node.changed.connect(self.changed.emit)
        node.remove_requested.connect(lambda n=node: self._remove_child(n))
        self.children.append(node)
        self.children_layout.addWidget(node)
        self.changed.emit()

    def _add_group_child(self):
        node = ExpressionNode(self.session, is_root=False)
        node.changed.connect(self.changed.emit)
        node.remove_requested.connect(lambda n=node: self._remove_child(n))
        self.children.append(node)
        self.children_layout.addWidget(node)
        self.changed.emit()

    def _remove_child(self, child: "ExpressionNode"):
        if child in self.children:
            self.children.remove(child)
            self.children_layout.removeWidget(child)
            child.deleteLater()
            self.changed.emit()

    def _convert_to_condition(self):
        while self.main_layout.count():
            item = self.main_layout.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
            elif item.layout():
                self._clear_layout(item.layout())

        self.condition = ConditionWidget(self.session)
        self.condition.changed.connect(self.changed.emit)
        self.condition.remove_requested.connect(self.remove_requested.emit)
        self.main_layout.addWidget(self.condition)

    def _clear_layout(self, layout):
        while layout.count():
            item = layout.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
            elif item.layout():
                self._clear_layout(item.layout())

    def to_filter_spec(self) -> Optional[dict]:
        if not self.is_group and self.condition:
            return self.condition.to_filter_spec()

        op = self.operator_combo.currentText()
        child_specs = []
        for child in self.children:
            spec = child.to_filter_spec()
            if spec is not None:
                child_specs.append(spec)

        if not child_specs:
            return None

        if op == "NOT":
            return {"type": "not", "child": child_specs[0]}
        elif op == "AND":
            if len(child_specs) == 1:
                return child_specs[0]
            return {"type": "and", "children": child_specs}
        elif op == "OR":
            if len(child_specs) == 1:
                return child_specs[0]
            return {"type": "or", "children": child_specs}
        return None


def build_query(session: Session, spec: dict):
    t = spec["type"]

    if t == "probabilistic_tag":
        tag = session.execute(
            select(ProbabilisticTag.id).where(
                ProbabilisticTag.category == spec["category"],
                ProbabilisticTag.name == spec["name"],
            )
        ).scalar()
        if tag is None:
            return select(ImageEntry.id).where(False)
        return select(ImageProbabilisticTag.image_id).where(
            ImageProbabilisticTag.tag_id == tag,
            ImageProbabilisticTag.probability >= spec["min_probability"],
        )

    elif t == "regular_tag":
        tag = session.execute(
            select(RegularTag.id).where(
                RegularTag.category == spec["category"],
                RegularTag.name == spec["name"],
            )
        ).scalar()
        if tag is None:
            return select(ImageEntry.id).where(False)
        return select(ImageRegularTag.image_id).where(ImageRegularTag.tag_id == tag)

    elif t == "description":
        return select(ImageDescription.image_id).where(
            ImageDescription.description.contains(spec["text"])
        )

    elif t == "and":
        subqueries = [build_query(session, c) for c in spec["children"]]
        if not subqueries:
            return select(ImageEntry.id).where(False)
        result = subqueries[0].subquery()
        for sq in subqueries[1:]:
            sq_sub = sq.subquery()
            result = (
                select(result.c.image_id).where(
                    result.c.image_id.in_(select(sq_sub.c.image_id))
                )
            ).subquery()
        return select(result.c.image_id)

    elif t == "or":
        subqueries = [build_query(session, c) for c in spec["children"]]
        if not subqueries:
            return select(ImageEntry.id).where(False)
        result = subqueries[0].subquery()
        for sq in subqueries[1:]:
            sq_sub = sq.subquery()
            result = (
                select(result.c.image_id).union(select(sq_sub.c.image_id))
            ).subquery()
        return select(result.c.image_id)

    elif t == "not":
        child_query = build_query(session, spec["child"]).subquery()
        return select(ImageEntry.id).where(
            ImageEntry.id.notin_(select(child_query.c.image_id))
        )

    raise ValueError(f"Unknown filter spec type: {t}")


class SearchTab(QWidget):
    """Tab with boolean expression builder for filtering images."""

    results_found = Signal(list)

    def __init__(self, session: Session, base_dir: str, parent=None):
        super().__init__(parent)
        self.session = session
        self.base_dir = base_dir

        layout = QVBoxLayout(self)

        splitter = QSplitter(Qt.Orientation.Vertical)

        # Expression builder area
        expr_container = QWidget()
        expr_layout = QVBoxLayout(expr_container)
        expr_layout.setContentsMargins(0, 0, 0, 0)

        scroll = QScrollArea()
        scroll.setWidgetResizable(True)

        self.root_node = ExpressionNode(session, is_root=True)
        # self.root_node.changed.connect(self._on_expression_changed)
        scroll.setWidget(self.root_node)
        expr_layout.addWidget(scroll)

        search_btn = QPushButton("Search")
        search_btn.clicked.connect(self._execute_search)
        expr_layout.addWidget(search_btn)

        self.status_label = QLabel("")
        expr_layout.addWidget(self.status_label)

        splitter.addWidget(expr_container)

        # Results area
        self.thumbnail_list = ImageThumbnailList()
        splitter.addWidget(self.thumbnail_list)

        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 2)
        layout.addWidget(splitter)

    def _on_expression_changed(self):
        pass

    def _execute_search(self):
        spec = self.root_node.to_filter_spec()
        if spec is None:
            self.status_label.setText("No valid filter conditions specified.")
            self.thumbnail_list.set_images([])
            return

        query = build_query(self.session, spec)
        image_ids = [r[0] for r in self.session.execute(query).all()]

        if not image_ids:
            self.status_label.setText("No images found.")
            self.thumbnail_list.set_images([])
            return

        images = self.session.execute(
            select(ImageEntry.relative_path).where(ImageEntry.id.in_(image_ids))
        ).all()

        paths = []
        for (rel_path,) in images:
            full_path = os.path.join(self.base_dir, rel_path)
            if os.path.isfile(full_path):
                paths.append(full_path)

        self.status_label.setText(f"Found {len(paths)} images.")
        self.thumbnail_list.set_images(paths)

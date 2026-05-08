from __future__ import annotations
import logging

from typing import Dict, Optional, cast

from PySide6.QtCore import QModelIndex, QPoint, Qt, Signal
from PySide6.QtWidgets import (
    QComboBox,
    QFormLayout,
    QLabel,
    QLineEdit,
    QSpinBox,
    QStackedWidget,
    QVBoxLayout,
    QWidget,
)

from ascii_diagram.model.enums import (
    EdgeType,
    HJustify,
    OverlapMode,
    ShapeType,
    VAlign,
)
from ascii_diagram.model.scene_model import SceneModel, PROPERTIES_ROLE
from ascii_diagram.model.shape_properties import (
    EdgeData,
    EllipseData,
    RectData,
    TextData,
)


class PropertiesPanel(QWidget):
    property_changed = Signal()

    def __init__(self, model: SceneModel, parent=None):
        super().__init__(parent)
        self._model = model
        self._current_index: Optional[QModelIndex] = None
        self._current_shape_type: Optional[ShapeType] = None
        self._widgets: Dict[str, QWidget] = {}
        self._ignore_updates = False

        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)

        self._no_selection_label = QLabel("No shape selected")
        layout.addWidget(self._no_selection_label)

        self._stack = QStackedWidget()
        layout.addWidget(self._stack)

        self._rect_form = self._make_rect_form()
        self._edge_form = self._make_edge_form()
        self._ellipse_form = self._make_ellipse_form()
        self._text_form = self._make_text_form()

        self._stack.addWidget(self._rect_form)
        self._stack.addWidget(self._edge_form)
        self._stack.addWidget(self._ellipse_form)
        self._stack.addWidget(self._text_form)

        self._no_selection_label.show()
        self._stack.hide()

    def set_current_index(self, index: QModelIndex) -> None:
        self._current_index = index
        if not index.isValid():
            self._no_selection_label.show()
            self._stack.hide()
            self._current_shape_type = None
            return

        item = self._model.item_from_index(index)
        if item is None:
            self._no_selection_label.show()
            self._stack.hide()
            self._current_shape_type = None
            return

        self._current_shape_type = item.shape_type

        self._no_selection_label.hide()
        self._stack.show()

        if item.shape_type == ShapeType.RECTANGLE:
            self._stack.setCurrentWidget(self._rect_form)
            assert isinstance(item.properties, RectData)
            self._populate_rect(item.properties)
        elif item.shape_type == ShapeType.EDGE:
            self._stack.setCurrentWidget(self._edge_form)
            assert isinstance(item.properties, EdgeData)
            self._populate_edge(item.properties)
        elif item.shape_type == ShapeType.ELLIPSE:
            self._stack.setCurrentWidget(self._ellipse_form)
            assert isinstance(item.properties, EllipseData)
            self._populate_ellipse(item.properties)
        elif item.shape_type == ShapeType.TEXT:
            self._stack.setCurrentWidget(self._text_form)
            assert isinstance(item.properties, TextData)
            self._populate_text(item.properties)

    def _make_position_group(self, parent: QFormLayout) -> None:
        x_spin = QSpinBox()
        x_spin.setRange(-9999, 9999)
        y_spin = QSpinBox()
        y_spin.setRange(-9999, 9999)
        self._widgets["x"] = x_spin
        self._widgets["y"] = y_spin
        parent.addRow("X", x_spin)
        parent.addRow("Y", y_spin)
        x_spin.valueChanged.connect(self._on_property_edit)
        y_spin.valueChanged.connect(self._on_property_edit)

    def _make_overlap_combo(self, parent: QFormLayout) -> None:
        combo = QComboBox()
        combo.addItems(["OVERWRITE", "EMPTY_ONLY", "MERGE"])
        self._widgets["overlap_mode"] = combo
        parent.addRow("Overlap", combo)
        combo.currentIndexChanged.connect(self._on_property_edit)

    def _make_rect_form(self) -> QWidget:
        w = QWidget()
        form = QFormLayout(w)
        self._make_position_group(form)
        w_spin = QSpinBox()
        w_spin.setRange(0, 9999)
        w_spin.valueChanged.connect(self._on_property_edit)
        h_spin = QSpinBox()
        h_spin.setRange(0, 9999)
        h_spin.valueChanged.connect(self._on_property_edit)
        self._widgets["width"] = w_spin
        self._widgets["height"] = h_spin
        form.addRow("Width", w_spin)
        form.addRow("Height", h_spin)

        for key, label in [
            ("corner_ch", "Corner"),
            ("top_ch", "Top"),
            ("bottom_ch", "Bottom"),
            ("left_ch", "Left"),
            ("right_ch", "Right"),
        ]:
            le = QLineEdit()
            le.setMaxLength(1)
            self._widgets[key] = le
            form.addRow(label, le)
            le.textChanged.connect(self._on_property_edit)
        self._make_overlap_combo(form)
        return w

    def _make_edge_form(self) -> QWidget:
        w = QWidget()
        form = QFormLayout(w)
        self._make_position_group(form)

        for key, label in [
            ("start_x", "Start X"),
            ("start_y", "Start Y"),
            ("end_x", "End X"),
            ("end_y", "End Y"),
        ]:
            spin = QSpinBox()
            spin.setRange(-9999, 9999)
            self._widgets[key] = spin
            form.addRow(label, spin)
            spin.valueChanged.connect(self._on_property_edit)

        type_combo = QComboBox()
        type_combo.addItems(["POLYLINE", "SPLINE", "ORTHOGONAL"])
        self._widgets["edge_type"] = type_combo
        form.addRow("Type", type_combo)
        type_combo.currentIndexChanged.connect(self._on_property_edit)

        for key, label in [
            ("h_char", "H char"),
            ("v_char", "V char"),
            ("start_arrow", "Start arrow"),
            ("end_arrow", "End arrow"),
            ("bend_char", "Bend char"),
        ]:
            le = QLineEdit()
            le.setMaxLength(1)
            self._widgets[key] = le
            form.addRow(label, le)
            le.textChanged.connect(self._on_property_edit)

        self._make_overlap_combo(form)
        return w

    def _make_ellipse_form(self) -> QWidget:
        w = QWidget()
        form = QFormLayout(w)
        self._make_position_group(form)

        w_spin = QSpinBox()
        w_spin.setRange(0, 9999)
        h_spin = QSpinBox()
        h_spin.setRange(0, 9999)
        self._widgets["width"] = w_spin
        self._widgets["height"] = h_spin
        form.addRow("Width", w_spin)
        form.addRow("Height", h_spin)

        le = QLineEdit()
        le.setMaxLength(1)
        self._widgets["char"] = le
        form.addRow("Char", le)
        le.textChanged.connect(self._on_property_edit)

        self._make_overlap_combo(form)
        return w

    def _make_text_form(self) -> QWidget:
        w = QWidget()
        form = QFormLayout(w)
        self._make_position_group(form)

        w_spin = QSpinBox()
        w_spin.setRange(0, 9999)
        h_spin = QSpinBox()
        h_spin.setRange(0, 9999)
        self._widgets["width"] = w_spin
        self._widgets["height"] = h_spin
        form.addRow("Width", w_spin)
        form.addRow("Height", h_spin)

        text_le = QLineEdit()
        self._widgets["text"] = text_le
        form.addRow("Text", text_le)
        text_le.textChanged.connect(self._on_property_edit)

        h_combo = QComboBox()
        h_combo.addItems(["LEFT", "CENTER", "RIGHT"])
        self._widgets["h_justify"] = h_combo
        form.addRow("H Justify", h_combo)
        h_combo.currentIndexChanged.connect(self._on_property_edit)

        v_combo = QComboBox()
        v_combo.addItems(["TOP", "CENTER", "BOTTOM"])
        self._widgets["v_align"] = v_combo
        form.addRow("V Align", v_combo)
        v_combo.currentIndexChanged.connect(self._on_property_edit)

        self._make_overlap_combo(form)
        return w

    def _populate_rect(self, props: RectData) -> None:
        self._ignore_updates = True
        x_spin = cast(QSpinBox, self._widgets["x"])
        y_spin = cast(QSpinBox, self._widgets["y"])
        w_spin = cast(QSpinBox, self._widgets["width"])
        h_spin = cast(QSpinBox, self._widgets["height"])
        x_spin.setValue(props.x)
        y_spin.setValue(props.y)
        w_spin.setValue(props.width)
        h_spin.setValue(props.height)
        cast(QLineEdit, self._widgets["corner_ch"]).setText(props.corner_ch)
        cast(QLineEdit, self._widgets["top_ch"]).setText(props.top_ch)
        cast(QLineEdit, self._widgets["bottom_ch"]).setText(props.bottom_ch)
        cast(QLineEdit, self._widgets["left_ch"]).setText(props.left_ch)
        cast(QLineEdit, self._widgets["right_ch"]).setText(props.right_ch)
        mode_idx = ["OVERWRITE", "EMPTY_ONLY",
                    "MERGE"].index(props.overlap_mode.name)
        cast(QComboBox,
             self._widgets["overlap_mode"]).setCurrentIndex(mode_idx)
        self._ignore_updates = False

    def _populate_edge(self, props: EdgeData) -> None:
        self._ignore_updates = True
        cast(QSpinBox, self._widgets["x"]).setValue(props.x)
        cast(QSpinBox, self._widgets["y"]).setValue(props.y)
        cast(QSpinBox, self._widgets["start_x"]).setValue(props.start.x())
        cast(QSpinBox, self._widgets["start_y"]).setValue(props.start.y())
        cast(QSpinBox, self._widgets["end_x"]).setValue(props.end.x())
        cast(QSpinBox, self._widgets["end_y"]).setValue(props.end.y())
        type_idx = ["POLYLINE", "SPLINE",
                    "ORTHOGONAL"].index(props.edge_type.name)
        cast(QComboBox, self._widgets["edge_type"]).setCurrentIndex(type_idx)
        cast(QLineEdit, self._widgets["h_char"]).setText(props.h_char)
        cast(QLineEdit, self._widgets["v_char"]).setText(props.v_char)
        cast(QLineEdit,
             self._widgets["start_arrow"]).setText(props.start_arrow)
        cast(QLineEdit, self._widgets["end_arrow"]).setText(props.end_arrow)
        cast(QLineEdit, self._widgets["bend_char"]).setText(props.bend_char)
        mode_idx = ["OVERWRITE", "EMPTY_ONLY",
                    "MERGE"].index(props.overlap_mode.name)
        cast(QComboBox,
             self._widgets["overlap_mode"]).setCurrentIndex(mode_idx)
        self._ignore_updates = False

    def _populate_ellipse(self, props: EllipseData) -> None:
        self._ignore_updates = True
        cast(QSpinBox, self._widgets["x"]).setValue(props.x)
        cast(QSpinBox, self._widgets["y"]).setValue(props.y)
        cast(QSpinBox, self._widgets["width"]).setValue(props.width)
        cast(QSpinBox, self._widgets["height"]).setValue(props.height)
        cast(QLineEdit, self._widgets["char"]).setText(props.char)
        mode_idx = ["OVERWRITE", "EMPTY_ONLY",
                    "MERGE"].index(props.overlap_mode.name)
        cast(QComboBox,
             self._widgets["overlap_mode"]).setCurrentIndex(mode_idx)
        self._ignore_updates = False

    def _populate_text(self, props: TextData) -> None:
        self._ignore_updates = True
        cast(QSpinBox, self._widgets["x"]).setValue(props.x)
        cast(QSpinBox, self._widgets["y"]).setValue(props.y)
        cast(QSpinBox, self._widgets["width"]).setValue(props.width)
        cast(QSpinBox, self._widgets["height"]).setValue(props.height)
        cast(QLineEdit, self._widgets["text"]).setText(props.text)
        h_idx = ["LEFT", "CENTER", "RIGHT"].index(props.h_justify.name)
        cast(QComboBox, self._widgets["h_justify"]).setCurrentIndex(h_idx)
        v_idx = ["TOP", "CENTER", "BOTTOM"].index(props.v_align.name)
        cast(QComboBox, self._widgets["v_align"]).setCurrentIndex(v_idx)
        mode_idx = ["OVERWRITE", "EMPTY_ONLY",
                    "MERGE"].index(props.overlap_mode.name)
        cast(QComboBox,
             self._widgets["overlap_mode"]).setCurrentIndex(mode_idx)
        self._ignore_updates = False

    def _on_property_edit(self) -> None:
        if self._ignore_updates:
            return
        if self._current_index is None or not self._current_index.isValid():
            return
        if self._current_shape_type is None:
            return

        item = self._model.item_from_index(self._current_index)
        assert item is not None, f"Could not select item for current index {self._current_index}"

        props = item.properties
        self._read_common(props)

        if isinstance(props, RectData):
            self._read_rect(props)
        elif isinstance(props, EdgeData):
            self._read_edge(props)
        elif isinstance(props, EllipseData):
            self._read_ellipse(props)
        elif isinstance(props, TextData):
            self._read_text(props)

        item.properties = props
        self._model.setData(self._current_index, props, PROPERTIES_ROLE)
        self.property_changed.emit()

    def _read_common(self, props) -> None:
        props.x = cast(QSpinBox, self._widgets["x"]).value()
        props.y = cast(QSpinBox, self._widgets["y"]).value()
        mode_name = cast(QComboBox,
                         self._widgets["overlap_mode"]).currentText()
        props.overlap_mode = OverlapMode[mode_name]

    def _read_rect(self, props: RectData) -> None:
        props.width = cast(QSpinBox, self._widgets["width"]).value()
        props.height = cast(QSpinBox, self._widgets["height"]).value()
        props.corner_ch = cast(QLineEdit, self._widgets["corner_ch"]).text()
        props.top_ch = cast(QLineEdit, self._widgets["top_ch"]).text()
        props.bottom_ch = cast(QLineEdit, self._widgets["bottom_ch"]).text()
        props.left_ch = cast(QLineEdit, self._widgets["left_ch"]).text()
        props.right_ch = cast(QLineEdit, self._widgets["right_ch"]).text()

    def _read_edge(self, props: EdgeData) -> None:
        props.start.setX(cast(QSpinBox, self._widgets["start_x"]).value())
        props.start.setY(cast(QSpinBox, self._widgets["start_y"]).value())
        props.end.setX(cast(QSpinBox, self._widgets["end_x"]).value())
        props.end.setY(cast(QSpinBox, self._widgets["end_y"]).value())
        type_name = cast(QComboBox, self._widgets["edge_type"]).currentText()
        props.edge_type = EdgeType[type_name]
        props.h_char = cast(QLineEdit, self._widgets["h_char"]).text()
        props.v_char = cast(QLineEdit, self._widgets["v_char"]).text()
        props.start_arrow = cast(QLineEdit,
                                 self._widgets["start_arrow"]).text()
        props.end_arrow = cast(QLineEdit, self._widgets["end_arrow"]).text()
        props.bend_char = cast(QLineEdit, self._widgets["bend_char"]).text()

    def _read_ellipse(self, props: EllipseData) -> None:
        props.width = cast(QSpinBox, self._widgets["width"]).value()
        props.height = cast(QSpinBox, self._widgets["height"]).value()
        props.char = cast(QLineEdit, self._widgets["char"]).text()

    def _read_text(self, props: TextData) -> None:
        props.width = cast(QSpinBox, self._widgets["width"]).value()
        props.height = cast(QSpinBox, self._widgets["height"]).value()
        props.text = cast(QLineEdit, self._widgets["text"]).text()
        h_name = cast(QComboBox, self._widgets["h_justify"]).currentText()
        props.h_justify = HJustify[h_name]
        v_name = cast(QComboBox, self._widgets["v_align"]).currentText()
        props.v_align = VAlign[v_name]

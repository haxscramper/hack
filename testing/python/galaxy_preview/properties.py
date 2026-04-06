from PySide6 import QtWidgets, QtCore, QtGui
from models import (
    Star,
    Planet,
    ImageOverlay,
    Shape,
    GalacticEntry,
    EntryType,
    GalacticMap,
)
import math


class PropertiesPanel(QtWidgets.QWidget):
    entry_changed = QtCore.Signal(str)  # Emits the ID of the changed entry

    def __init__(self, parent=None):
        super().__init__(parent)
        self.layout = QtWidgets.QVBoxLayout(self)
        self.current_entry = None
        self.galactic_map = None

        # Name
        self.layout.addWidget(QtWidgets.QLabel("Name:"))
        self.name_edit = QtWidgets.QLineEdit()
        self.name_edit.textChanged.connect(self._on_field_changed)
        self.layout.addWidget(self.name_edit)

        # Short Description (Plain Text)
        self.layout.addWidget(QtWidgets.QLabel("Short Description:"))
        self.short_desc_edit = QtWidgets.QPlainTextEdit()
        self.short_desc_edit.textChanged.connect(self._on_field_changed)
        self.layout.addWidget(self.short_desc_edit)

        # Detailed Description (Plain Text)
        self.layout.addWidget(QtWidgets.QLabel("Detailed Description:"))
        self.detailed_desc_edit = QtWidgets.QPlainTextEdit()
        self.detailed_desc_edit.textChanged.connect(self._on_field_changed)
        self.layout.addWidget(self.detailed_desc_edit)

        # Toolbar for rich text
        rt_toolbar = QtWidgets.QToolBar()
        # Note: B/I actions are disabled as we switched to plain text
        self.layout.addWidget(rt_toolbar)

        # Visibility
        self.visible_check = QtWidgets.QCheckBox("Visible")
        self.visible_check.toggled.connect(self._on_field_changed)
        self.layout.addWidget(self.visible_check)

        self.show_name_check = QtWidgets.QCheckBox("Show Name in 3D")
        self.show_name_check.toggled.connect(self._on_field_changed)
        self.layout.addWidget(self.show_name_check)

        self.show_desc_check = QtWidgets.QCheckBox("Show Short Desc in 3D")
        self.show_desc_check.toggled.connect(self._on_field_changed)
        self.layout.addWidget(self.show_desc_check)

        # Color
        self.layout.addWidget(QtWidgets.QLabel("Color:"))
        self.color_button = QtWidgets.QPushButton("Choose Color")
        self.color_button.clicked.connect(self._choose_color)
        self.layout.addWidget(self.color_button)

        # Specific fields based on type
        self.specific_fields_group = QtWidgets.QGroupBox("Specific Properties")
        self.specific_layout = QtWidgets.QFormLayout(self.specific_fields_group)
        self.layout.addWidget(self.specific_fields_group)

        self.layout.addStretch()

        self.setEnabled(False)
    def _on_field_changed(self):
        if not self.current_entry:
            return

        self.current_entry.name = self.name_edit.text()
        self.current_entry.short_description = self.short_desc_edit.toPlainText()
        self.current_entry.detailed_description = self.detailed_desc_edit.toPlainText()
        self.current_entry.visible = self.visible_check.isChecked()
        self.current_entry.show_name = self.show_name_check.isChecked()
        self.current_entry.show_short_desc = self.show_desc_check.isChecked()
        self.entry_changed.emit(self.current_entry.id)

    def _add_hms_dms_fields(self, entry, is_relative=False):
        if is_relative:
            ra_h_attr = "rel_ra_h"
            ra_m_attr = "rel_ra_m"
            ra_s_attr = "rel_ra_s"
            dec_d_attr = "rel_dec_d"
            dec_m_attr = "rel_dec_m"
            dec_s_attr = "rel_dec_s"
            dist_attr = "rel_distance"
            prefix = "Rel "
        else:
            ra_h_attr = "ra_h"
            ra_m_attr = "ra_m"
            ra_s_attr = "ra_s"
            dec_d_attr = "dec_d"
            dec_m_attr = "dec_m"
            dec_s_attr = "dec_s"
            dist_attr = "distance"
            prefix = ""

        ra_h = getattr(entry, ra_h_attr)
        ra_m = getattr(entry, ra_m_attr)
        ra_s = getattr(entry, ra_s_attr)
        dec_d = getattr(entry, dec_d_attr)
        dec_m = getattr(entry, dec_m_attr)
        dec_s = getattr(entry, dec_s_attr)
        dist = getattr(entry, dist_attr)

        ra_layout = QtWidgets.QHBoxLayout()
        ra_h_spin = QtWidgets.QSpinBox()
        ra_h_spin.setRange(0, 23)
        ra_h_spin.setValue(ra_h)
        ra_h_spin.setSuffix("h ")
        ra_h_spin.valueChanged.connect(lambda v: setattr(entry, ra_h_attr, v))
        ra_h_spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        ra_layout.addWidget(ra_h_spin)

        ra_m_spin = QtWidgets.QSpinBox()
        ra_m_spin.setRange(0, 59)
        ra_m_spin.setValue(ra_m)
        ra_m_spin.setSuffix("m ")
        ra_m_spin.valueChanged.connect(lambda v: setattr(entry, ra_m_attr, v))
        ra_m_spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        ra_layout.addWidget(ra_m_spin)

        ra_s_spin = QtWidgets.QDoubleSpinBox()
        ra_s_spin.setRange(0.0, 59.99)
        ra_s_spin.setDecimals(2)
        ra_s_spin.setSingleStep(0.1)
        ra_s_spin.setValue(ra_s)
        ra_s_spin.setSuffix("s")
        ra_s_spin.valueChanged.connect(lambda v: setattr(entry, ra_s_attr, v))
        ra_s_spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        ra_layout.addWidget(ra_s_spin)

        ra_label = QtWidgets.QLabel(f"{prefix}RA:")
        self.specific_layout.addRow(ra_label, ra_layout)

        dec_layout = QtWidgets.QHBoxLayout()
        dec_d_spin = QtWidgets.QSpinBox()
        dec_d_spin.setRange(-90, 90)
        dec_d_spin.setValue(dec_d)
        dec_d_spin.setSuffix("° ")
        dec_d_spin.valueChanged.connect(lambda v: setattr(entry, dec_d_attr, v))
        dec_d_spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        dec_layout.addWidget(dec_d_spin)

        dec_m_spin = QtWidgets.QSpinBox()
        dec_m_spin.setRange(0, 59)
        dec_m_spin.setValue(dec_m)
        dec_m_spin.setSuffix("' ")
        dec_m_spin.valueChanged.connect(lambda v: setattr(entry, dec_m_attr, v))
        dec_m_spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        dec_layout.addWidget(dec_m_spin)

        dec_s_spin = QtWidgets.QDoubleSpinBox()
        dec_s_spin.setRange(0.0, 59.99)
        dec_s_spin.setDecimals(2)
        dec_s_spin.setSingleStep(0.1)
        dec_s_spin.setValue(dec_s)
        dec_s_spin.setSuffix('"')
        dec_s_spin.valueChanged.connect(lambda v: setattr(entry, dec_s_attr, v))
        dec_s_spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        dec_layout.addWidget(dec_s_spin)

        dec_label = QtWidgets.QLabel(f"{prefix}Dec:")
        self.specific_layout.addRow(dec_label, dec_layout)

        dist_spin = QtWidgets.QDoubleSpinBox()
        dist_spin.setRange(0.0, 1000000.0)
        dist_spin.setDecimals(2)
        dist_spin.setSingleStep(1.0)
        dist_spin.setValue(dist)
        dist_spin.setSuffix(" pc")
        dist_spin.valueChanged.connect(lambda v: setattr(entry, dist_attr, v))
        dist_spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        self.specific_layout.addRow(f"{prefix}Dist:", dist_spin)

    def set_entry(self, entry: GalacticEntry, galactic_map: GalacticMap | None = None):
        self.current_entry = entry
        self.galactic_map = galactic_map
        if not entry:
            self.setEnabled(False)
            return

        self.setEnabled(True)
        # Block signals to avoid triggering updates while setting fields
        self.name_edit.blockSignals(True)
        self.short_desc_edit.blockSignals(True)
        self.detailed_desc_edit.blockSignals(True)
        self.visible_check.blockSignals(True)
        self.show_name_check.blockSignals(True)
        self.show_desc_check.blockSignals(True)

        self.name_edit.setText(entry.name)
        self.short_desc_edit.setPlainText(entry.short_description)
        self.detailed_desc_edit.setPlainText(entry.detailed_description)
        self.visible_check.setChecked(entry.visible)
        self.show_name_check.setChecked(entry.show_name)
        self.show_desc_check.setChecked(entry.show_short_desc)

        self.name_edit.blockSignals(False)
        self.short_desc_edit.blockSignals(False)
        self.detailed_desc_edit.blockSignals(False)
        self.visible_check.blockSignals(False)
        self.show_name_check.blockSignals(False)
        self.show_desc_check.blockSignals(False)

        # Update color button
        self.color_button.setStyleSheet(f"background-color: {entry.color};")

        # Clear and rebuild specific fields
        while self.specific_layout.count():
            item = self.specific_layout.takeAt(0)
            if item.widget():
                item.widget().deleteLater()

        if isinstance(entry, Star):
            self._add_hms_dms_fields(entry, is_relative=False)
            self._add_float_field(
                "Radius", entry.radius, lambda v: setattr(entry, "radius", v)
            )

        elif isinstance(entry, Planet):
            self._add_hms_dms_fields(entry, is_relative=True)
            self._add_float_field(
                "Radius", entry.radius, lambda v: setattr(entry, "radius", v)
            )

        elif isinstance(entry, ImageOverlay):
            file_btn = QtWidgets.QPushButton("Select Image")
            file_btn.clicked.connect(self._select_image)
            self.specific_layout.addRow("File:", file_btn)
            self._add_hms_dms_fields(entry, is_relative=False)
            self._add_float_field(
                "Width", entry.width, lambda v: setattr(entry, "width", v)
            )
            self._add_float_field(
                "Height", entry.height, lambda v: setattr(entry, "height", v)
            )

    def _select_image(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, "Select Image", "", "Images (*.png *.jpg *.jpeg *.bmp)"
        )
        if path:
            self.current_entry.file_path = path
            self.entry_changed.emit(self.current_entry.id)

    def _add_float_field(
        self, label, value, setter, min_val=-100000.0, max_val=100000.0
    ):
        spin = QtWidgets.QDoubleSpinBox()
        spin.setRange(min_val, max_val)
        spin.setDecimals(2)
        spin.setSingleStep(0.1)
        spin.setValue(value)
        spin.valueChanged.connect(setter)
        spin.valueChanged.connect(
            lambda _: self.entry_changed.emit(self.current_entry.id)
        )
        self.specific_layout.addRow(label, spin)

    def _on_field_changed(self):
        if not self.current_entry:
            return

        self.current_entry.name = self.name_edit.text()
        self.current_entry.short_description = self.short_desc_edit.toPlainText()
        self.current_entry.detailed_description = self.detailed_desc_edit.toPlainText()
        self.current_entry.visible = self.visible_check.isChecked()
        self.current_entry.show_name = self.show_name_check.isChecked()
        self.current_entry.show_short_desc = self.show_desc_check.isChecked()
        self.entry_changed.emit(self.current_entry.id)

    def _choose_color(self):
        color = QtWidgets.QColorDialog.getColor(
            QtGui.QColor(self.current_entry.color), self
        )
        if color.isValid():
            self.current_entry.color = color.name()
            self.color_button.setStyleSheet(f"background-color: {color.name()};")
            self.entry_changed.emit(self.current_entry.id)

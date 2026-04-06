from PySide6 import QtWidgets, QtCore, QtGui
from models import Star, Planet, ImageOverlay, Shape, GalacticEntry, EntryType, GalacticMap
import math

class PropertiesPanel(QtWidgets.QWidget):
    entry_changed = QtCore.Signal(str) # Emits the ID of the changed entry

    def __init__(self, parent=None):
        super().__init__(parent)
        self.layout = QtWidgets.QVBoxLayout(self)
        self.current_entry = None
        self.galactic_map = None
        
        # Coordinates Display
        self.coord_label = QtWidgets.QLabel("")
        self.coord_label.setStyleSheet("font-weight: bold; color: #555;")
        self.layout.addWidget(self.coord_label)

        # Name
        self.layout.addWidget(QtWidgets.QLabel("Name:"))
        self.name_edit = QtWidgets.QLineEdit()
        self.name_edit.textChanged.connect(self._on_field_changed)
        self.layout.addWidget(self.name_edit)

        # Toolbar for rich text
        rt_toolbar = QtWidgets.QToolBar()
        bold_action = rt_toolbar.addAction("B")
        bold_action.setFont(QtGui.QFont("Arial", 10, QtGui.QFont.Bold))
        bold_action.triggered.connect(self._toggle_bold)
        italic_action = rt_toolbar.addAction("I")
        italic_action.setFont(QtGui.QFont("Arial", 10, -1, True))
        italic_action.triggered.connect(self._toggle_italic)
        self.layout.addWidget(rt_toolbar)
        
        # Short Description (Rich Text)
        self.layout.addWidget(QtWidgets.QLabel("Short Description:"))
        self.short_desc_edit = QtWidgets.QTextEdit()
        self.short_desc_edit.textChanged.connect(self._on_field_changed)
        self.layout.addWidget(self.short_desc_edit)
        
        # Detailed Description (Rich Text)
        self.layout.addWidget(QtWidgets.QLabel("Detailed Description:"))
        self.detailed_desc_edit = QtWidgets.QTextEdit()
        self.detailed_desc_edit.textChanged.connect(self._on_field_changed)
        self.layout.addWidget(self.detailed_desc_edit)
        
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

    def _toggle_bold(self):
        editor = self.focusWidget()
        if isinstance(editor, QtWidgets.QTextEdit):
            fmt = editor.currentCharFormat()
            fmt.setFontWeight(QtGui.QFont.Normal if fmt.fontWeight() == QtGui.QFont.Bold else QtGui.QFont.Bold)
            editor.setCurrentCharFormat(fmt)

    def _toggle_italic(self):
        editor = self.focusWidget()
        if isinstance(editor, QtWidgets.QTextEdit):
            fmt = editor.currentCharFormat()
            fmt.setFontItalic(not fmt.fontItalic())
            editor.setCurrentCharFormat(fmt)

    def set_entry(self, entry: GalacticEntry, galactic_map: GalacticMap = None):
        self.current_entry = entry
        self.galactic_map = galactic_map
        if not entry:
            self.setEnabled(False)
            self.coord_label.setText("")
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
        self.short_desc_edit.setHtml(entry.short_description)
        self.detailed_desc_edit.setHtml(entry.detailed_description)
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
            self._add_float_field("X", entry.x, lambda v: setattr(entry, 'x', v))
            self._add_float_field("Y", entry.y, lambda v: setattr(entry, 'y', v))
            self._add_float_field("Z", entry.z, lambda v: setattr(entry, 'z', v))
            self._add_float_field("Radius", entry.radius, lambda v: setattr(entry, 'radius', v))
            
        elif isinstance(entry, Planet):
            self._add_float_field("Rel X", entry.rel_x, lambda v: setattr(entry, 'rel_x', v))
            self._add_float_field("Rel Y", entry.rel_y, lambda v: setattr(entry, 'rel_y', v))
            self._add_float_field("Rel Z", entry.rel_z, lambda v: setattr(entry, 'rel_z', v))
            self._add_float_field("Radius", entry.radius, lambda v: setattr(entry, 'radius', v))

        elif isinstance(entry, ImageOverlay):
            file_btn = QtWidgets.QPushButton("Select Image")
            file_btn.clicked.connect(self._select_image)
            self.specific_layout.addRow("File:", file_btn)
            self._add_float_field("X", entry.x, lambda v: setattr(entry, 'x', v))
            self._add_float_field("Y", entry.y, lambda v: setattr(entry, 'y', v))
            self._add_float_field("Z", entry.z, lambda v: setattr(entry, 'z', v))
            self._add_float_field("Width", entry.width, lambda v: setattr(entry, 'width', v))
            self._add_float_field("Height", entry.height, lambda v: setattr(entry, 'height', v))
            
        self._update_coord_label()

    def _update_coord_label(self):
        if not self.current_entry or not self.galactic_map:
            self.coord_label.setText("")
            return
            
        abs_x, abs_y, abs_z = 0.0, 0.0, 0.0
        if isinstance(self.current_entry, Star) or isinstance(self.current_entry, ImageOverlay):
            abs_x, abs_y, abs_z = self.current_entry.x, self.current_entry.y, self.current_entry.z
        elif isinstance(self.current_entry, Planet):
            parent_star = next((e for e in self.galactic_map.entries if e.id == self.current_entry.parent_star_id), None)
            abs_x, abs_y, abs_z = self.current_entry.rel_x, self.current_entry.rel_y, self.current_entry.rel_z
            if parent_star and isinstance(parent_star, Star):
                abs_x += parent_star.x
                abs_y += parent_star.y
                abs_z += parent_star.z
        else:
            self.coord_label.setText("")
            return

        distance = math.sqrt(abs_x**2 + abs_y**2 + abs_z**2)
        ra_rad = math.atan2(abs_y, abs_x)
        ra_deg = math.degrees(ra_rad)
        if ra_deg < 0:
            ra_deg += 360
            
        dec_rad = math.asin(abs_z / distance) if distance > 0 else 0
        dec_deg = math.degrees(dec_rad)
        
        self.coord_label.setText(f"RA: {ra_deg:.2f}° | Dec: {dec_deg:.2f}° | Dist: {distance:.2f} pc")


    def _select_image(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select Image", "", "Images (*.png *.jpg *.jpeg *.bmp)")
        if path:
            self.current_entry.file_path = path
            self.entry_changed.emit(self.current_entry.id)

    def _add_float_field(self, label, value, setter):
        spin = QtWidgets.QDoubleSpinBox()
        spin.setRange(-100000, 100000)
        spin.setValue(value)
        spin.valueChanged.connect(setter)
        spin.valueChanged.connect(lambda _: self._update_coord_label())
        spin.valueChanged.connect(lambda _: self.entry_changed.emit(self.current_entry.id))
        self.specific_layout.addRow(label, spin)

    def _on_field_changed(self):
        if not self.current_entry:
            return
        
        self.current_entry.name = self.name_edit.text()
        self.current_entry.short_description = self.short_desc_edit.toHtml()
        self.current_entry.detailed_description = self.detailed_desc_edit.toHtml()
        self.current_entry.visible = self.visible_check.isChecked()
        self.current_entry.show_name = self.show_name_check.isChecked()
        self.current_entry.show_short_desc = self.show_desc_check.isChecked()
        self.entry_changed.emit(self.current_entry.id)

    def _choose_color(self):
        color = QtWidgets.QColorDialog.getColor(QtGui.QColor(self.current_entry.color), self)
        if color.isValid():
            self.current_entry.color = color.name()
            self.color_button.setStyleSheet(f"background-color: {color.name()};")
            self.entry_changed.emit(self.current_entry.id)

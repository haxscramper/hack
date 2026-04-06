#!/usr/bin/env python
from __future__ import annotations

import sys

from PySide6.QtCore import Qt, QRect, Signal
from PySide6.QtGui import QMouseEvent, QPainter, QColor, QPen, QBrush
from PySide6.QtWidgets import QApplication, QWidget, QVBoxLayout, QLabel


class RangeSlider(QWidget):
    lowerValueChanged = Signal(int)
    upperValueChanged = Signal(int)
    rangeChanged = Signal(int, int)

    def __init__(self, parent=None):
        super().__init__(parent)

        self._minimum = 0
        self._maximum = 100
        self._lower = 25
        self._upper = 75

        self._handle_radius = 8
        self._groove_height = 6
        self._margin = self._handle_radius

        self._active_handle = None
        self._mouse_offset = 0

        self.setMinimumHeight(32)
        self.setMouseTracking(True)

    def minimum(self) -> int:
        return self._minimum

    def maximum(self) -> int:
        return self._maximum

    def lowerValue(self) -> int:
        return self._lower

    def upperValue(self) -> int:
        return self._upper

    def setMinimum(self, value: int) -> None:
        self._minimum = value
        if self._maximum < self._minimum:
            self._maximum = self._minimum
        self.setLowerValue(max(self._lower, self._minimum))
        self.setUpperValue(max(self._upper, self._lower))
        self.update()

    def setMaximum(self, value: int) -> None:
        self._maximum = value
        if self._minimum > self._maximum:
            self._minimum = self._maximum
        self.setUpperValue(min(self._upper, self._maximum))
        self.setLowerValue(min(self._lower, self._upper))
        self.update()

    def setRange(self, minimum: int, maximum: int) -> None:
        self._minimum = minimum
        self._maximum = maximum
        if self._minimum > self._maximum:
            self._minimum, self._maximum = self._maximum, self._minimum

        self._lower = min(max(self._lower, self._minimum), self._maximum)
        self._upper = min(max(self._upper, self._minimum), self._maximum)

        if self._lower > self._upper:
            self._lower = self._upper

        self.update()
        self.rangeChanged.emit(self._lower, self._upper)

    def setLowerValue(self, value: int) -> None:
        value = max(self._minimum, min(value, self._upper))
        if value == self._lower:
            return
        self._lower = value
        self.update()
        self.lowerValueChanged.emit(value)
        self.rangeChanged.emit(self._lower, self._upper)

    def setUpperValue(self, value: int) -> None:
        value = min(self._maximum, max(value, self._lower))
        if value == self._upper:
            return
        self._upper = value
        self.update()
        self.upperValueChanged.emit(value)
        self.rangeChanged.emit(self._lower, self._upper)

    def paintEvent(self, event) -> None:
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)

        groove_rect = self._grooveRect()
        lower_x = self._valueToPos(self._lower)
        upper_x = self._valueToPos(self._upper)

        painter.setPen(Qt.NoPen)
        painter.setBrush(QColor("#c8c8c8"))
        painter.drawRoundedRect(groove_rect, 3, 3)

        selected_rect = QRect(
            lower_x,
            groove_rect.y(),
            upper_x - lower_x,
            groove_rect.height(),
        )
        painter.setBrush(QColor("#4a90e2"))
        painter.drawRoundedRect(selected_rect, 3, 3)

        self._drawHandle(painter, lower_x, self.height() // 2)
        self._drawHandle(painter, upper_x, self.height() // 2)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        pos = event.position().toPoint()
        lower_center_x = self._valueToPos(self._lower)
        upper_center_x = self._valueToPos(self._upper)
        center_y = self.height() // 2

        lower_hit = self._pointInHandle(pos.x(), pos.y(), lower_center_x, center_y)
        upper_hit = self._pointInHandle(pos.x(), pos.y(), upper_center_x, center_y)

        if lower_hit and upper_hit:
            if abs(pos.x() - lower_center_x) <= abs(pos.x() - upper_center_x):
                self._active_handle = "lower"
                self._mouse_offset = pos.x() - lower_center_x
            else:
                self._active_handle = "upper"
                self._mouse_offset = pos.x() - upper_center_x
        elif lower_hit:
            self._active_handle = "lower"
            self._mouse_offset = pos.x() - lower_center_x
        elif upper_hit:
            self._active_handle = "upper"
            self._mouse_offset = pos.x() - upper_center_x
        else:
            lower_dist = abs(pos.x() - lower_center_x)
            upper_dist = abs(pos.x() - upper_center_x)
            if lower_dist <= upper_dist:
                self._active_handle = "lower"
                self._mouse_offset = 0
                self.setLowerValue(self._posToValue(pos.x()))
            else:
                self._active_handle = "upper"
                self._mouse_offset = 0
                self.setUpperValue(self._posToValue(pos.x()))

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        if self._active_handle is None:
            return

        x = int(event.position().x()) - self._mouse_offset
        value = self._posToValue(x)

        if self._active_handle == "lower":
            self.setLowerValue(value)
        elif self._active_handle == "upper":
            self.setUpperValue(value)

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        self._active_handle = None

    def sizeHint(self):
        return self.minimumSizeHint()

    def minimumSizeHint(self):
        return self.size().expandedTo(self.rect().size()).grownBy(self.contentsMargins())

    def _grooveRect(self) -> QRect:
        y = (self.height() - self._groove_height) // 2
        return QRect(
            self._margin,
            y,
            self.width() - 2 * self._margin,
            self._groove_height,
        )

    def _valueToPos(self, value: int) -> int:
        groove = self._grooveRect()
        span = self._maximum - self._minimum
        if span == 0:
            return groove.x()
        ratio = (value - self._minimum) / span
        return groove.x() + int(ratio * groove.width())

    def _posToValue(self, x: int) -> int:
        groove = self._grooveRect()
        x = max(groove.left(), min(x, groove.right()))
        span = self._maximum - self._minimum
        if groove.width() == 0:
            return self._minimum
        ratio = (x - groove.x()) / groove.width()
        return self._minimum + round(ratio * span)

    def _drawHandle(self, painter: QPainter, x: int, y: int) -> None:
        painter.setPen(QPen(QColor("#808080"), 1))
        painter.setBrush(QBrush(QColor("#ffffff")))
        painter.drawEllipse(x - self._handle_radius, y - self._handle_radius,
                            2 * self._handle_radius, 2 * self._handle_radius)

    def _pointInHandle(self, px: int, py: int, hx: int, hy: int) -> bool:
        dx = px - hx
        dy = py - hy
        return dx * dx + dy * dy <= self._handle_radius * self._handle_radius


if __name__ == "__main__":
    class Demo(QWidget):
        def __init__(self):
            super().__init__()

            self.slider = RangeSlider()
            self.slider.setRange(0, 100)
            self.slider.setLowerValue(20)
            self.slider.setUpperValue(80)

            self.label = QLabel()
            self._updateLabel(self.slider.lowerValue(), self.slider.upperValue())

            self.slider.rangeChanged.connect(self._updateLabel)

            layout = QVBoxLayout(self)
            layout.addWidget(self.slider)
            layout.addWidget(self.label)

            self.setWindowTitle("PySide6 Range Slider")

        def _updateLabel(self, lower: int, upper: int) -> None:
            self.label.setText(f"Selected range: {lower} - {upper}")


    app = QApplication(sys.argv)
    window = Demo()
    window.resize(400, 100)
    window.show()
    sys.exit(app.exec())

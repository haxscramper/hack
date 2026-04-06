from PySide6 import QtCore, QtWidgets, QtGui
import numpy as np


class GalacticGridItem(QtWidgets.QGraphicsItem):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setZValue(-2)

    def boundingRect(self):
        return QtCore.QRectF(-16000, -16000, 32000, 32000)

    def paint(self, painter, option, widget):
        # Adaptive pen width based on zoom
        zoom = option.levelOfDetailFromTransform(painter.worldTransform())
        pen_width = max(0.5, 1.0 / zoom) if zoom > 1e-6 else 1.0
        
        pen = QtGui.QPen(QtGui.QColor(255, 255, 255, 60))
        pen.setWidthF(pen_width)
        pen.setCosmetic(True)  # Keeps pen width constant in pixel-space
        painter.setPen(pen)

        # Concentric circles
        for r in range(1000, 16000, 1000):
            painter.drawEllipse(QtCore.QRectF(-r, -r, 2 * r, 2 * r))

        # Radial lines
        for angle in np.linspace(0, 2 * np.pi, 12, endpoint=False):
            x = 15000 * np.cos(angle)
            y = 15000 * np.sin(angle)
            painter.drawLine(0, 0, int(x), int(y))

        # Galactic center outline
        pen.setColor(QtGui.QColor(128, 204, 255, 120))
        painter.setPen(pen)
        painter.drawEllipse(QtCore.QRectF(8000 - 15000, -15000, 30000, 30000))

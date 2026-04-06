from PySide6 import QtCore, QtWidgets, QtGui
import numpy as np


class GalacticGridItem(QtWidgets.QGraphicsItem):
    def __init__(self, scene, parent=None):
        super().__init__(parent)
        self.scene = scene
        self.setZValue(-2)
        self.labels = []

    def boundingRect(self):
        return QtCore.QRectF(-16000, -16000, 32000, 32000)

    def update_labels(self):
        # Remove existing labels
        for label in self.labels:
            self.scene.removeItem(label)
        self.labels = []

        inner_rings = [10, 50, 100, 500]
        for r in inner_rings:
            text = QtWidgets.QGraphicsTextItem(f"{r}pc")
            text.setFont(QtGui.QFont("Arial", 8))
            text.setDefaultTextColor(QtCore.Qt.white)
            text.setPos(r, 0)
            text.setFlag(QtWidgets.QGraphicsItem.ItemIgnoresTransformations)
            self.scene.addItem(text)
            self.labels.append(text)

    def paint(self, painter, option, widget):
        painter.setRenderHint(QtGui.QPainter.Antialiasing)
        
        pen = QtGui.QPen(QtGui.QColor(255, 255, 255, 60))
        pen.setWidth(1)
        pen.setCosmetic(True) 
        painter.setPen(pen)

        inner_rings = [10, 50, 100, 500]
        outer_rings = range(1000, 16000, 1000)
        
        for r in list(outer_rings) + inner_rings:
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

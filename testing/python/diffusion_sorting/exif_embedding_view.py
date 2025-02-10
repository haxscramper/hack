#!/usr/bin/env python

import pywebio
from pywebio.output import put_html
import pandas as pd
import altair as alt
from beartype.typing import List, Any
from pathlib import Path
import json
import base64
import sys
from vispy import scene, app, io
import numpy as np
from PySide6 import QtCore, QtWidgets, QtGui

if __name__ == "__main__":
    workspace_root = Path(sys.argv[1])


class CustomGraphicsView(QtWidgets.QGraphicsView):
    zoomChanged = QtCore.Signal(float)

    def __init__(self, scene: QtWidgets.QGraphicsScene, parent: Any = None):
        super().__init__(parent)
        self.setScene(scene)
        self.setRenderHint(QtGui.QPainter.RenderHint.Antialiasing)
        self.setDragMode(QtWidgets.QGraphicsView.DragMode.ScrollHandDrag)
        self._zoom = 1.0

    def wheelEvent(self, event: QtGui.QWheelEvent) -> None:
        delta = event.angleDelta().y()
        if delta > 0:
            self.scale(1.1, 1.1)
            self._zoom *= 1.1
        else:
            self.scale(0.9, 0.9)
            self._zoom *= 0.9
        self.zoomChanged.emit(self._zoom)


class CustomGraphicsPixmapItem(QtWidgets.QGraphicsPixmapItem):

    def __init__(self, pixmap: QtGui.QPixmap) -> None:
        super().__init__(pixmap)
        self.setAcceptHoverEvents(True)

    def hoverEnterEvent(self,
                        event: QtWidgets.QGraphicsSceneHoverEvent) -> None:
        QtWidgets.QApplication.setOverrideCursor(
            QtCore.Qt.CursorShape.PointingHandCursor)
        super().hoverEnterEvent(event)

    def hoverLeaveEvent(self,
                        event: QtWidgets.QGraphicsSceneHoverEvent) -> None:
        QtWidgets.QApplication.restoreOverrideCursor()
        super().hoverLeaveEvent(event)


class CustomGraphicsScene(QtWidgets.QGraphicsScene):

    def __init__(self, parent: QtWidgets.QWidget | None = None) -> None:
        super().__init__(parent)

    def mousePressEvent(self,
                        event: QtWidgets.QGraphicsSceneMouseEvent) -> None:
        if event.button() == QtCore.Qt.MouseButton.LeftButton:
            item = self.itemAt(event.scenePos(), QtGui.QTransform())
            if isinstance(item,
                          QtWidgets.QGraphicsPixmapItem) and item.data(0):
                text = item.data(0)
                clipboard = QtWidgets.QApplication.clipboard()
                clipboard.setText(text)
        super().mousePressEvent(event)


class MainWindow(QtWidgets.QMainWindow):

    def __init__(self, json_path: Path):
        super().__init__()

        self.scene = CustomGraphicsScene()
        self.view = CustomGraphicsView(self.scene)
        self.setCentralWidget(self.view)

        self.points = []
        self.images = []
        self.dots = []
        self.threshold = 5.0

        embeddings = json.loads(json_path.read_text())

        base_size = 180
        for entry in embeddings:
            x = entry["x"] * self.width()
            y = entry["y"] * self.height()

            point = QtWidgets.QGraphicsEllipseItem()
            point.setPos(x, y)
            point.setRect(-0.5, -0.5, 1, 1)
            point.setTransformOriginPoint(0, 0)
            # point.setBrush(QtGui.QBrush(QtGui.QColor(255, 255, 255, 76)))
            # point.setPen(QtGui.QPen(QtCore.Qt.PenStyle.NoPen))
            self.scene.addItem(point)
            self.points.append(point)

            pixmap = QtGui.QPixmap(str(entry["image_path"]))
            if pixmap.isNull():
                continue

            img = CustomGraphicsPixmapItem(
                pixmap.scaled(
                    base_size, base_size,
                    QtCore.Qt.AspectRatioMode.KeepAspectRatio,
                    QtCore.Qt.TransformationMode.SmoothTransformation))

            img.setData(0, entry["associated"])

            img.setPos(x, y)
            img.setVisible(False)
            img.setOffset(-base_size / 2, -base_size / 2)
            self.scene.addItem(img)
            self.images.append(img)

        self.view.setSceneRect(self.scene.itemsBoundingRect())
        self.view.zoomChanged.connect(self.handle_zoom)

    def handle_zoom(self, level: float) -> None:
        print(level)
        show_images = level >= self.threshold
        scale = 1.0 / level if show_images else 1.0

        for point in self.points:
            point.setVisible(not show_images)

        for img in self.images:
            img.setVisible(show_images)
            if show_images:
                img.setScale(scale)


def visualize_adaptive_images() -> None:
    app = QtWidgets.QApplication([])
    window = MainWindow(workspace_root.joinpath("embedding.json"))
    window.setMinimumHeight(1200)
    window.setMinimumWidth(1200)
    window.show()
    app.exec()


if __name__ == "__main__":
    visualize_adaptive_images()

from PySide6 import QtCore, QtWidgets, QtGui
from models import (
    Star,
    Planet,
    Shape,
    GalacticEntry,
    GalacticMap,
)
from grid import GalacticGridItem


class GalacticSignals(QtCore.QObject):
    selected_entry = QtCore.Signal(object)


class GalacticCanvas(QtWidgets.QGraphicsView):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.pyside_signals = GalacticSignals()
        self.scene = QtWidgets.QGraphicsScene()
        self.setScene(self.scene)
        self.setRenderHint(QtGui.QPainter.Antialiasing)
        self.setDragMode(QtWidgets.QGraphicsView.ScrollHandDrag)
        self.setTransformationAnchor(QtWidgets.QGraphicsView.AnchorUnderMouse)

        self.galactic_map = None
        self.entry_to_visual = {}

        self.setBackgroundBrush(QtGui.QBrush(QtCore.Qt.black))
        self.grid = GalacticGridItem()
        self.scene.addItem(self.grid)

    def wheelEvent(self, event):
        factor = 1.1 if event.angleDelta().y() > 0 else 0.9
        self.scale(factor, factor)
        self.scene.update()
        self.grid.update()

    def get_camera_state(self):
        transform = self.transform()
        return {
            "zoom": transform.m11(),
            "center_x": self.mapToScene(self.viewport().rect().center()).x(),
            "center_y": self.mapToScene(self.viewport().rect().center()).y(),
        }

    def set_camera_state(self, state):
        self.resetTransform()
        self.scale(state["zoom"], state["zoom"])
        self.centerOn(state["center_x"], state["center_y"])
        self.scene.update()

    def clear_scene(self):
        for entry_id in list(self.entry_to_visual.keys()):
            self.remove_entry_visual(entry_id)

    def remove_entry_visual(self, entry_id):
        if entry_id in self.entry_to_visual:
            items = self.entry_to_visual.pop(entry_id)
            for item in items:
                self.scene.removeItem(item)

    def update_from_model(self, galactic_map):
        self.galactic_map = galactic_map
        self.clear_scene()
        for entry in galactic_map.entries:
            self.add_entry_visual(entry, galactic_map)

    def add_entry_visual(self, entry: GalacticEntry, galactic_map: GalacticMap):
        if not entry.visible:
            return

        items = []
        pos = (entry.x, entry.y)
        if isinstance(entry, Planet):
            parent_star = next((e for e in galactic_map.entries if e.id == entry.parent_star_id), None)
            if parent_star:
                pos = (pos[0] + parent_star.x, pos[1] + parent_star.y)

        # Draw as Marker
        brush = QtGui.QBrush(QtGui.QColor(entry.color))
        size = 10 if isinstance(entry, Star) else 6
        item = self.scene.addEllipse(pos[0] - size/2, pos[1] - size/2, size, size, QtGui.QPen(QtCore.Qt.NoPen), brush)
        item.setFlag(QtWidgets.QGraphicsItem.ItemIgnoresTransformations)
        items.append(item)

        if entry.show_name:
            text = QtWidgets.QGraphicsTextItem(entry.name)
            text.setFont(QtGui.QFont("Arial", 8))
            text.setDefaultTextColor(QtCore.Qt.white)
            text.setPos(8, -8)
            text.setParentItem(item)
            text.setFlag(QtWidgets.QGraphicsItem.ItemIgnoresTransformations)
            items.append(text)

        # Draw Shapes (lines) as world-space
        if isinstance(entry, Shape):
            path = QtGui.QPainterPath()
            if entry.points:
                path.moveTo(*entry.points[0][:2])
                for p in entry.points[1:]:
                    path.lineTo(*p[:2])
            item = self.scene.addPath(path, QtGui.QPen(QtGui.QColor(entry.color), 3))
            items.append(item)

        self.entry_to_visual[entry.id] = items


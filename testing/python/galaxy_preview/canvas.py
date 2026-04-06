from PySide6 import QtCore, QtWidgets, QtGui
from models import (
    Star,
    Planet,
    Shape,
    GalacticEntry,
    GalacticMap,
)
from grid import GalacticGridItem
import logging


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
        self.grid = GalacticGridItem(self.scene)
        self.scene.addItem(self.grid)
        self.grid.update_labels()
        
        # Ensure scene rect is initialized
        self.setSceneRect(-16000, -16000, 32000, 32000)
        self.centerOn(0, 0)

    def wheelEvent(self, event):
        factor = 1.1 if event.angleDelta().y() > 0 else 0.9
        self.scale(factor, factor)
        self.scene.update()
        self.grid.update()

    def get_camera_state(self):
        import math
        transform = self.transform()
        m11 = transform.m11()
        m12 = transform.m12()
        
        # If the view hasn't been shown yet, the center might be wrong.
        # But we still want to save something.
        center = self.viewport().rect().center()
        if center.x() == 0 and center.y() == 0:
            # Fallback to (0,0) scene coordinates if viewport is not yet sized
            # or use current scene center
            scene_center = self.mapToScene(center)
            center_x = scene_center.x()
            center_y = scene_center.y()
        else:
            scene_center = self.mapToScene(center)
            center_x = scene_center.x()
            center_y = scene_center.y()

        return {
            "zoom": math.sqrt(m11**2 + m12**2),
            "rotation": math.degrees(math.atan2(m12, m11)),
            "center_x": center_x,
            "center_y": center_y,
        }

    def set_camera_state(self, state):
        self.resetTransform()
        zoom = state.get("zoom", 1.0)
        if zoom < 1e-4:
            zoom = 1.0
        self.scale(zoom, zoom)
        self.rotate(state.get("rotation", 0))
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
        logging.info(f"Loading {len(galactic_map.entries)} entries into scene.")
        for entry in galactic_map.entries:
            self.add_entry_visual(entry, galactic_map)
        logging.info(f"Scene has {len(self.scene.items())} items after update.")

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


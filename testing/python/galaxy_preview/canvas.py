import numpy as np
import pyvista as pv
from pyvistaqt import QtInteractor
from PySide6 import QtCore, QtWidgets
from models import (
    Star,
    Planet,
    ImageOverlay,
    Shape,
    GalacticEntry,
    EntryType,
    GalacticMap,
)


def _hex_to_rgba(hex_color: str) -> tuple:
    h = hex_color.lstrip("#")
    if len(h) == 3:
        h = "".join(c * 2 for c in h)
    r = int(h[0:2], 16) / 255.0
    g = int(h[2:4], 16) / 255.0
    b = int(h[4:6], 16) / 255.0
    return (r, g, b, 1.0)


class GalacticSignals(QtCore.QObject):
    selected_entry = QtCore.Signal(object)  # Emits the selected entry ID or None
    shape_drawn = QtCore.Signal(list)  # Emits list of (x, y, z) points


class GalacticCanvas(QtInteractor):
    def __init__(self, parent=None, **kwargs):
        super().__init__(parent=parent, **kwargs)
        self.pyside_signals = GalacticSignals()

        # Storage for visuals keyed by entry ID
        self.visual_to_entry = {}  # Actor to entry ID
        self.entry_to_visual = {}  # entry ID to Actor
        self.entry_to_label = {}  # entry ID to Label Actor (if any)
        self.use_markers = False
        self.galactic_map = None

        # Drawing state
        self.is_drawing = False
        self.current_draw_points = []
        self.temp_draw_actor = None

        # Initialize scene
        self.set_background("black")
        self._add_grid()
        self._add_milky_way_outline()

        # Enable picking
        self.enable_mesh_picking(
            callback=self._on_picked, show=False, left_clicking=True
        )

    def _add_grid(self):
        # Concentric circles every 1000 pc up to 15000 pc
        for r in range(1000, 16000, 1000):
            # pv.Circle creates a polygon, adding it as wireframe ensures it's hollow
            circle = pv.Circle(radius=r, resolution=100)
            self.add_mesh(
                circle,
                color="white",
                opacity=0.2,
                style="wireframe",
                line_width=1,
                pickable=False,
            )

        # 12 radial lines
        for angle in np.linspace(0, 2 * np.pi, 12, endpoint=False):
            line = pv.Line([0, 0, 0], [15000 * np.cos(angle), 15000 * np.sin(angle), 0])
            self.add_mesh(
                line, color="white", opacity=0.2, line_width=1, pickable=False
            )

    def _add_milky_way_outline(self):
        # Galactic center is at ~8000 pc towards +X (l=0). Radius ~ 15000 pc.
        center = [8000, 0, 0]
        radius = 15000
        outline = pv.Circle(radius=radius, resolution=200)
        outline.points += center
        self.add_mesh(
            outline,
            color=(0.5, 0.8, 1.0),
            opacity=0.4,
            style="wireframe",
            line_width=2,
            pickable=False,
        )

    def set_3d_mode(self):
        self.is_drawing = False
        self.view_isometric()
        self.render()

    def set_top_down_mode(self):
        self.is_drawing = False

        # Re-center view so RA=0 line (origin) is centered
        # and galactic center (+X direction) appears at top of screen
        camera = self.camera
        camera.position = [0, 0, 20000]
        camera.focal_point = [0, 0, 0]
        camera.up = [1, 0, 0]

        self.render()

    def set_marker_mode(self, enabled: bool):
        self.use_markers = enabled
        if self.galactic_map:
            self.update_from_model(self.galactic_map)

    def start_drawing(self):
        # For drawing in PyVista, we might want to use a more native way or stick to event handling
        # Since we are in 3D, "top-down" drawing needs to project mouse to the XY plane.
        self.is_drawing = True
        self.current_draw_points = []
        if self.temp_draw_actor:
            self.remove_actor(self.temp_draw_actor)
            self.temp_draw_actor = None

        # We can use the plotter's track_click_position or similar, but let's try to override mouse events
        # or use a widget. For now, let's use a simple plane picker for drawing.
        pass

    def _on_picked(self, mesh):
        if id(mesh) in self.visual_to_entry:
            entry_id = self.visual_to_entry[id(mesh)]
            self.pyside_signals.selected_entry.emit(entry_id)
        else:
            self.pyside_signals.selected_entry.emit(None)

    def clear_scene(self):
        for entry_id in list(self.entry_to_visual.keys()):
            self.remove_entry_visual(entry_id)

    def remove_entry_visual(self, entry_id):
        if entry_id in self.entry_to_visual:
            actor = self.entry_to_visual.pop(entry_id)
            if hasattr(actor, "mapper") and actor.mapper.dataset:
                mesh_id = id(actor.mapper.dataset)
                if mesh_id in self.visual_to_entry:
                    del self.visual_to_entry[mesh_id]
            self.remove_actor(actor)

        if entry_id in self.entry_to_label:
            label_actor = self.entry_to_label.pop(entry_id)
            self.remove_actor(label_actor)

    def update_from_model(self, galactic_map):
        self.galactic_map = galactic_map
        self.use_markers = galactic_map.use_markers
        self.clear_scene()
        for entry in galactic_map.entries:
            self.add_entry_visual(entry, galactic_map)
        self.render()

    def add_entry_visual(self, entry: GalacticEntry, galactic_map: GalacticMap):
        if not entry.visible:
            return

        actor = None
        pos = [entry.x, entry.y, entry.z]

        if isinstance(entry, Star):
            if self.use_markers:
                actor = self.add_points(
                    np.array([pos]),
                    color=entry.color,
                    point_size=15,
                    render_points_as_spheres=True,
                    pickable=True,
                )
            else:
                sphere = pv.Sphere(radius=entry.radius, center=pos)
                actor = self.add_mesh(sphere, color=entry.color, smooth_shading=True)

        elif isinstance(entry, Planet):
            parent_star = next(
                (e for e in galactic_map.entries if e.id == entry.parent_star_id), None
            )
            abs_pos = [entry.rel_x, entry.rel_y, entry.rel_z]
            if parent_star and isinstance(parent_star, Star):
                abs_pos[0] += parent_star.x
                abs_pos[1] += parent_star.y
                abs_pos[2] += parent_star.z

            if self.use_markers:
                actor = self.add_points(
                    np.array([abs_pos]),
                    color=entry.color,
                    point_size=10,
                    render_points_as_spheres=True,
                    pickable=True,
                )
            else:
                sphere = pv.Sphere(radius=entry.radius, center=abs_pos)
                actor = self.add_mesh(sphere, color=entry.color, smooth_shading=True)

        elif isinstance(entry, Planet):
            parent_star = next(
                (e for e in galactic_map.entries if e.id == entry.parent_star_id), None
            )
            abs_pos = [entry.rel_x, entry.rel_y, entry.rel_z]
            if parent_star and isinstance(parent_star, Star):
                abs_pos[0] += parent_star.x
                abs_pos[1] += parent_star.y
                abs_pos[2] += parent_star.z

            if self.use_markers:
                actor = self.add_points(
                    np.array([abs_pos]),
                    color=entry.color,
                    point_size=10,
                    render_points_as_spheres=True,
                    pickable=True,
                )
            else:
                sphere = pv.Sphere(radius=entry.radius, center=abs_pos)
                actor = self.add_mesh(sphere, color=entry.color, smooth_shading=True)

        elif isinstance(entry, ImageOverlay):
            try:
                plane = pv.Plane(
                    center=[entry.x, entry.y, entry.z],
                    direction=[0, 0, 1],
                    i_size=entry.width,
                    j_size=entry.height,
                )
                texture = pv.read_texture(entry.file_path)
                actor = self.add_mesh(plane, texture=texture)
            except Exception as e:
                print(f"Failed to load image {entry.file_path}: {e}")

        elif isinstance(entry, Shape):
            if len(entry.points) > 1:
                pts = np.array(entry.points)
                # Create a PolyData line
                line = pv.lines_from_points(pts)
                actor = self.add_mesh(line, color=entry.color, line_width=3)

        if actor:
            self.entry_to_visual[entry.id] = actor
            self.visual_to_entry[id(actor.mapper.dataset)] = entry.id

            # Add labels if requested
            labels = []
            if entry.show_name:
                labels.append(entry.name)
            if entry.show_short_desc:
                import re

                clean = re.sub("<[^<]+?>", "", entry.short_description)
                labels.append(clean)

            if labels:
                txt = "\n".join(labels)
                label_actor = self.add_point_labels(
                    [pos],
                    [txt],
                    font_size=10,
                    show_points=False,
                    text_color="white",
                    always_visible=True,
                )
                self.entry_to_label[entry.id] = label_actor

    # Overriding mouse events for custom drawing if needed
    def mousePressEvent(self, event):
        if self.is_drawing:
            if event.button() == QtCore.Qt.LeftButton:
                # Project click to XY plane
                click_pos = (
                    self.pick_mouse_position()
                )  # This might not be exactly what we want for XY plane
                # Better: intersect ray with XY plane
                pos = self._get_xy_plane_pos(event.pos())
                if pos:
                    self.current_draw_points.append(pos)
                    self._update_temp_draw()
            elif event.button() == QtCore.Qt.RightButton:
                if len(self.current_draw_points) > 1:
                    self.pyside_signals.shape_drawn.emit(self.current_draw_points)
                self.is_drawing = False
                if self.temp_draw_actor:
                    self.remove_actor(self.temp_draw_actor)
                    self.temp_draw_actor = None
            return
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        if self.is_drawing and event.buttons() & QtCore.Qt.LeftButton:
            pos = self._get_xy_plane_pos(event.pos())
            if pos:
                self.current_draw_points.append(pos)
                self._update_temp_draw()
        super().mouseMoveEvent(event)

    def _get_xy_plane_pos(self, qpoint):
        # PyVista/VTK ray-plane intersection
        near, far = self._get_ray(qpoint)
        # XY plane: z=0. Intersection of line (near, far) with z=0
        # P = near + t * (far - near)
        # P.z = near.z + t * (far.z - near.z) = 0
        # t = -near.z / (far.z - near.z)
        if abs(far[2] - near[2]) < 1e-6:
            return None
        t = -near[2] / (far[2] - near[2])
        intersect = near + t * (far - near)
        return intersect.tolist()

    def _get_ray(self, qpoint):
        # Convert QPoint to normalized device coordinates
        width = self.width()
        height = self.height()
        x = (2.0 * qpoint.x() / width) - 1.0
        y = 1.0 - (2.0 * qpoint.y() / height)

        # This is a bit low-level for PyVista, but let's use the renderer
        renderer = self.renderer
        camera = renderer.GetActiveCamera()

        # Using VTK methods to get world coordinates
        from vtkmodules.vtkRenderingCore import vtkCoordinate

        coord = vtkCoordinate()
        coord.SetCoordinateSystemToDisplay()
        coord.SetValue(qpoint.x(), height - qpoint.y(), 0)
        world_near = coord.GetComputedWorldValue(renderer)

        coord.SetValue(qpoint.x(), height - qpoint.y(), 1)
        world_far = coord.GetComputedWorldValue(renderer)

        return np.array(world_near), np.array(world_far)

    def _update_temp_draw(self):
        if len(self.current_draw_points) > 1:
            pts = np.array(self.current_draw_points)
            line = pv.lines_from_points(pts)
            if not self.temp_draw_actor:
                self.temp_draw_actor = self.add_mesh(line, color="yellow", line_width=2)
            else:
                # Update existing actor's mapper
                self.temp_draw_actor.mapper.dataset = line
        self.render()

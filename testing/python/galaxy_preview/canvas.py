import numpy as np
from vispy import scene
from vispy.scene import visuals
from vispy.visuals.filters import TextureFilter
from PySide6 import QtCore
from models import Star, Planet, ImageOverlay, Shape, GalacticEntry, EntryType, GalacticMap
from vispy.scene.cameras import TurntableCamera
from vispy.scene.cameras.perspective import PerspectiveCamera
from vispy.util import keys

class CustomTurntableCamera(TurntableCamera):
    def viewbox_mouse_event(self, event):
        """
        The viewbox received a mouse event; update transform
        accordingly.
        """
        if event.handled or not self.interactive:
            return

        PerspectiveCamera.viewbox_mouse_event(self, event)

        if event.type == 'mouse_release':
            self._event_value = None  # Reset
        elif event.type == 'mouse_press':
            event.handled = True
        elif event.type == 'mouse_move':
            if event.press_event is None:
                return
            if 1 in event.buttons and 2 in event.buttons:
                return

            modifiers = event.mouse_event.modifiers
            p1 = event.mouse_event.press_event.pos
            p2 = event.mouse_event.pos
            d = p2 - p1

            if 1 in event.buttons and not modifiers:
                # Rotate
                self._update_rotation(event)

            elif (1 in event.buttons and keys.SHIFT in modifiers) or (2 in event.buttons and not modifiers):
                # Translate (Pan) - originally Shift+LMB, now also RMB
                norm = np.mean(self._viewbox.size)
                if self._event_value is None or len(self._event_value) == 2:
                    self._event_value = self.center
                dist = (p1 - p2) / norm * self._scale_factor
                dist[1] *= -1
                # Black magic part 1: turn 2D into 3D translations
                dx, dy, dz = self._dist_to_trans(dist)
                # Black magic part 2: take up-vector and flipping into account
                ff = self._flip_factors
                up, forward, right = self._get_dim_vectors()
                dx, dy, dz = right * dx + forward * dy + up * dz
                dx, dy, dz = ff[0] * dx, ff[1] * dy, dz * ff[2]
                c = self._event_value
                self.center = c[0] + dx, c[1] + dy, c[2] + dz

            elif 3 in event.buttons and not modifiers:
                # Zoom (moved to MMB)
                if self._event_value is None:
                    self._event_value = (self._scale_factor, self._distance)
                zoomy = (1 + self.zoom_factor) ** d[1]

                self.scale_factor = self._event_value[0] * zoomy
                # Modify distance if its given
                if self._distance is not None:
                    self._distance = self._event_value[1] * zoomy
                self.view_changed()

            elif 2 in event.buttons and keys.SHIFT in modifiers:
                # Change fov
                if self._event_value is None:
                    self._event_value = self._fov
                fov = self._event_value - d[1] / 5.0
                self.fov = min(180.0, max(0.0, fov))

class GalacticSignals(QtCore.QObject):
    selected_entry = QtCore.Signal(object) # Emits the selected entry ID or None
    shape_drawn = QtCore.Signal(list) # Emits list of (x, y, z) points

class GalacticCanvas(scene.SceneCanvas):
    def __init__(self, **kwargs):
        super().__init__(keys='interactive', **kwargs)
        self.unfreeze()
        self.pyside_signals = GalacticSignals()
        
        self.view = self.central_widget.add_view()
        
        # Cameras
        self.turntable_camera = CustomTurntableCamera(
            fov=45, distance=10, up='z'
        )
        self.pan_zoom_camera = scene.cameras.PanZoomCamera(
            aspect=1, up='y' # Top-down view (x, y plane)
        )
        
        self.view.camera = self.turntable_camera
        
        # Storage for visuals keyed by entry ID
        self.visual_to_entry = {}
        self.entry_to_visual = {}
        
        # Background map group
        self.bg_group = scene.Node(parent=self.view.scene)
        
        # Radial Coordinate grid (XY plane, centered at 0,0,0)
        grid_points = []
        grid_connect = []
        point_idx = 0
        
        # Concentric circles every 1000 pc up to 15000 pc
        for r in range(1000, 16000, 1000):
            circle_points = []
            for theta in np.linspace(0, 2*np.pi, 100, endpoint=False):
                circle_points.append([r * np.cos(theta), r * np.sin(theta), 0])
            grid_points.extend(circle_points)
            for i in range(100):
                grid_connect.append([point_idx + i, point_idx + ((i + 1) % 100)])
            point_idx += 100
            
        # 12 radial lines
        for angle in np.linspace(0, 2*np.pi, 12, endpoint=False):
            grid_points.extend([[0, 0, 0], [15000 * np.cos(angle), 15000 * np.sin(angle), 0]])
            grid_connect.append([point_idx, point_idx + 1])
            point_idx += 2
            
        self.grid = visuals.Line(
            pos=np.array(grid_points), 
            connect=np.array(grid_connect),
            color=(1, 1, 1, 0.2), 
            parent=self.bg_group
        )

        # Milky Way Outline
        # Earth is at (0,0,0). Galactic center is at ~8000 pc towards +X (l=0). Radius ~ 15000 pc.
        mw_points = []
        for theta in np.linspace(0, 2*np.pi, 200):
            mw_points.append([8000 + 15000 * np.cos(theta), 15000 * np.sin(theta), 0])
        self.mw_outline = visuals.Line(pos=np.array(mw_points), color=(0.5, 0.8, 1.0, 0.4), parent=self.bg_group)

        
        # Systems group
        self.systems_group = scene.Node(parent=self.view.scene)
        
        # Shapes group
        self.shapes_group = scene.Node(parent=self.view.scene)

        # Drawing state
        self.is_drawing = False
        self.current_draw_points = []
        self.temp_draw_visual = None

        self.freeze()

    def set_3d_mode(self):
        self.is_drawing = False
        self.view.camera = self.turntable_camera
        self.update()

    def set_top_down_mode(self):
        self.view.camera = self.pan_zoom_camera
        # Reset camera to see the plane
        self.pan_zoom_camera.set_range(x=(-10, 10), y=(-10, 10))
        self.update()

    def start_drawing(self):
        if self.view.camera == self.pan_zoom_camera:
            self.is_drawing = True
            self.current_draw_points = []
            if self.temp_draw_visual:
                self.temp_draw_visual.parent = None
                self.temp_draw_visual = None

    def on_mouse_press(self, event):
        if self.is_drawing:
            if event.button == 1:
                pos = self.view.camera.transform.imap(event.pos)
                self.current_draw_points.append((pos[0], pos[1], 0))
                self._update_temp_draw()
            elif event.button == 2: # Finish drawing
                if len(self.current_draw_points) > 1:
                    self.pyside_signals.shape_drawn.emit(self.current_draw_points)
                self.is_drawing = False
                if self.temp_draw_visual:
                    self.temp_draw_visual.parent = None
                    self.temp_draw_visual = None
            return

        if event.button == 1: # Left click for selection
            visual = self.visual_at(event.pos)
            if visual in self.visual_to_entry:
                entry_id = self.visual_to_entry[visual]
                self.pyside_signals.selected_entry.emit(entry_id)
            else:
                self.pyside_signals.selected_entry.emit(None)

    def on_mouse_move(self, event):
        if self.is_drawing and event.button == 1:
            pos = self.view.camera.transform.imap(event.pos)
            self.current_draw_points.append((pos[0], pos[1], 0))
            self._update_temp_draw()

    def _update_temp_draw(self):
        if len(self.current_draw_points) > 1:
            if not self.temp_draw_visual:
                self.temp_draw_visual = visuals.Line(
                    pos=np.array(self.current_draw_points),
                    color='yellow',
                    parent=self.view.scene
                )
            else:
                self.temp_draw_visual.set_data(pos=np.array(self.current_draw_points))

    def clear_scene(self):
        for visual in self.entry_to_visual.values():
            visual.parent = None
        self.entry_to_visual.clear()
        self.visual_to_entry.clear()

    def update_from_model(self, galactic_map):
        self.clear_scene()
        for entry in galactic_map.entries:
            self.add_entry_visual(entry, galactic_map)

    def add_entry_visual(self, entry: GalacticEntry, galactic_map: GalacticMap):
        visual = None
        if not entry.visible:
            return

        if isinstance(entry, Star):
            visual = visuals.Sphere(
                radius=entry.radius,
                color=entry.color,
                parent=self.systems_group,
                shading='smooth'
            )
            visual.transform = scene.transforms.STTransform(translate=(entry.x, entry.y, entry.z))
            
        elif isinstance(entry, Planet):
            parent_star = next((e for e in galactic_map.entries if e.id == entry.parent_star_id), None)
            abs_x, abs_y, abs_z = entry.rel_x, entry.rel_y, entry.rel_z
            if parent_star and isinstance(parent_star, Star):
                abs_x += parent_star.x
                abs_y += parent_star.y
                abs_z += parent_star.z
                
            visual = visuals.Sphere(
                radius=entry.radius,
                color=entry.color,
                parent=self.systems_group,
                shading='smooth'
            )
            visual.transform = scene.transforms.STTransform(translate=(abs_x, abs_y, abs_z))

        elif isinstance(entry, ImageOverlay):
            try:
                from PIL import Image as PILImage
                img_data = np.array(PILImage.open(entry.file_path))
                visual = visuals.Image(
                    img_data,
                    parent=self.bg_group
                )
                h, w = img_data.shape[:2]
                visual.transform = scene.transforms.STTransform(
                    scale=(entry.width / w, entry.height / h, 1),
                    translate=(entry.x - entry.width/2, entry.y - entry.height/2, entry.z)
                )
            except Exception as e:
                print(f"Failed to load image {entry.file_path}: {e}")
            
        elif isinstance(entry, Shape):
            if len(entry.points) > 1:
                pts = np.array(entry.points)
                visual = visuals.Line(
                    pos=pts,
                    color=entry.color,
                    parent=self.shapes_group
                )

        if visual:
            visual.interactive = True
            self.entry_to_visual[entry.id] = visual
            self.visual_to_entry[visual] = entry.id
            
            # Add labels if requested
            labels = []
            if entry.show_name:
                labels.append(entry.name)
            if entry.show_short_desc:
                import re
                clean = re.sub('<[^<]+?>', '', entry.short_description)
                labels.append(clean)
                
            if labels:
                txt = "\n".join(labels)
                label_visual = visuals.Text(
                    txt,
                    pos=(0, 0, 0),
                    color='white',
                    font_size=8,
                    anchor_x='left',
                    anchor_y='bottom',
                    parent=visual
                )
                # Label shouldn't be interactive to avoid interfering with picking the object
                label_visual.interactive = False

import sys
import json
from PySide6 import QtWidgets, QtCore, QtGui
from models import GalacticMap, Star, Planet, Shape, ImageOverlay, GalacticEntry, EntryType
from canvas import GalacticCanvas
from properties import PropertiesPanel

class MainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Galactic Map Editor")
        self.resize(1200, 800)
        
        self.galactic_map = GalacticMap()
        
        # UI Setup
        self.splitter = QtWidgets.QSplitter(QtCore.Qt.Horizontal)
        self.setCentralWidget(self.splitter)
        
        self.canvas = GalacticCanvas()
        self.splitter.addWidget(self.canvas.native)

        self.tree = QtWidgets.QTreeWidget()
        self.tree.setHeaderLabels(["Scene Entries"])
        self.tree.itemSelectionChanged.connect(self._on_tree_selection_changed)
        self.splitter.addWidget(self.tree)
        
        self.properties_panel = PropertiesPanel()
        self.splitter.addWidget(self.properties_panel)
        
        self.splitter.setStretchFactor(0, 4)
        self.splitter.setStretchFactor(1, 1)
        self.splitter.setStretchFactor(2, 2)
        
        # Toolbar
        self.toolbar = self.addToolBar("Main")
        
        add_star_action = self.toolbar.addAction("Add Star")
        add_star_action.triggered.connect(self._add_star)
        
        add_planet_action = self.toolbar.addAction("Add Planet")
        add_planet_action.triggered.connect(self._add_planet)
        
        add_image_action = self.toolbar.addAction("Add Image")
        add_image_action.triggered.connect(self._add_image)

        add_shape_action = self.toolbar.addAction("Add Shape")
        add_shape_action.triggered.connect(self._add_shape)

        delete_action = self.toolbar.addAction("Delete Selected")
        delete_action.triggered.connect(self._delete_selected)

        self.toolbar.addSeparator()
        
        view_3d_action = self.toolbar.addAction("3D View")
        view_3d_action.triggered.connect(self.canvas.set_3d_mode)
        
        view_top_action = self.toolbar.addAction("Top-Down View")
        view_top_action.triggered.connect(self.canvas.set_top_down_mode)
        
        draw_action = self.toolbar.addAction("Draw Shape (2D)")
        draw_action.triggered.connect(self.canvas.start_drawing)

        self.toolbar.addSeparator()
        
        save_action = self.toolbar.addAction("Save")
        save_action.triggered.connect(self._save_map)
        
        load_action = self.toolbar.addAction("Load")
        load_action.triggered.connect(self._load_map)
        
        # Connections
        self.canvas.pyside_signals.selected_entry.connect(self._on_entry_selected)
        self.canvas.pyside_signals.shape_drawn.connect(self._on_shape_drawn)
        self.properties_panel.entry_changed.connect(self._on_entry_changed)

    def _refresh_tree(self):
        self.tree.blockSignals(True)
        self.tree.clear()
        
        star_items = {}
        for entry in self.galactic_map.entries:
            if isinstance(entry, Star):
                item = QtWidgets.QTreeWidgetItem([entry.name])
                item.setData(0, QtCore.Qt.UserRole, entry.id)
                self.tree.addTopLevelItem(item)
                star_items[entry.id] = item
            elif isinstance(entry, ImageOverlay) or isinstance(entry, Shape):
                item = QtWidgets.QTreeWidgetItem([entry.name])
                item.setData(0, QtCore.Qt.UserRole, entry.id)
                self.tree.addTopLevelItem(item)
                
        for entry in self.galactic_map.entries:
            if isinstance(entry, Planet):
                parent_item = star_items.get(entry.parent_star_id)
                item = QtWidgets.QTreeWidgetItem([entry.name])
                item.setData(0, QtCore.Qt.UserRole, entry.id)
                if parent_item:
                    parent_item.addChild(item)
                else:
                    self.tree.addTopLevelItem(item)
                    
        self.tree.expandAll()
        
        if self.properties_panel.current_entry:
            self._select_tree_item(self.properties_panel.current_entry.id)
            
        self.tree.blockSignals(False)

    def _select_tree_item(self, entry_id):
        iterator = QtWidgets.QTreeWidgetItemIterator(self.tree)
        while iterator.value():
            item = iterator.value()
            if item.data(0, QtCore.Qt.UserRole) == entry_id:
                item.setSelected(True)
                self.tree.scrollToItem(item)
                break
            iterator += 1

    def _on_tree_selection_changed(self):
        selected = self.tree.selectedItems()
        if selected:
            entry_id = selected[0].data(0, QtCore.Qt.UserRole)
            self._on_entry_selected(entry_id, from_tree=True)
        else:
            self._on_entry_selected(None, from_tree=True)

    def _add_star(self):
        new_star = Star(name=f"Star {len(self.galactic_map.get_stars()) + 1}")
        self.galactic_map.entries.append(new_star)
        self.canvas.add_entry_visual(new_star, self.galactic_map)
        self._refresh_tree()
        self._on_entry_selected(new_star.id)

    def _add_planet(self):
        stars = self.galactic_map.get_stars()
        if not stars:
            QtWidgets.QMessageBox.warning(self, "No Stars", "Add a star first before adding a planet.")
            return
            
        # For simplicity, add to the first star or the currently selected star if it is a star
        parent_star = stars[0]
        curr = self.properties_panel.current_entry
        if curr and isinstance(curr, Star):
            parent_star = curr
            
        new_planet = Planet(
            name=f"Planet {len(self.galactic_map.get_planets()) + 1}",
            parent_star_id=parent_star.id,
            rel_x=parent_star.x + 0.5,
            rel_y=parent_star.y,
            rel_z=parent_star.z
        )
        self.galactic_map.entries.append(new_planet)
        self.canvas.add_entry_visual(new_planet, self.galactic_map)
        self._refresh_tree()
        self._on_entry_selected(new_planet.id)

    def _add_image(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select Image", "", "Images (*.png *.jpg *.jpeg *.bmp)")
        if path:
            new_image = ImageOverlay(
                name=f"Image {len([e for e in self.galactic_map.entries if isinstance(e, ImageOverlay)]) + 1}",
                file_path=path
            )
            self.galactic_map.entries.append(new_image)
            self.canvas.add_entry_visual(new_image, self.galactic_map)
            self._refresh_tree()
            self._on_entry_selected(new_image.id)

    def _add_shape(self):
        new_shape = Shape(
            name=f"Shape {len([e for e in self.galactic_map.entries if isinstance(e, Shape)]) + 1}",
            points=[(0, 0, 0), (1, 1, 0)] # Default line
        )
        self.galactic_map.entries.append(new_shape)
        self.canvas.add_entry_visual(new_shape, self.galactic_map)
        self._refresh_tree()
        self._on_entry_selected(new_shape.id)

    def _delete_selected(self):
        entry = self.properties_panel.current_entry
        if entry:
            res = QtWidgets.QMessageBox.question(self, "Delete?", f"Are you sure you want to delete {entry.name}?")
            if res == QtWidgets.QMessageBox.Yes:
                self.galactic_map.entries = [e for e in self.galactic_map.entries if e.id != entry.id]
                if entry.id in self.canvas.entry_to_visual:
                    visual = self.canvas.entry_to_visual[entry.id]
                    visual.parent = None
                    del self.canvas.visual_to_entry[visual]
                    del self.canvas.entry_to_visual[entry.id]
                self._refresh_tree()
                self._on_entry_selected(None)

    def _on_shape_drawn(self, points):
        new_shape = Shape(
            name=f"Freeform Shape {len([e for e in self.galactic_map.entries if isinstance(e, Shape)]) + 1}",
            points=points
        )
        self.galactic_map.entries.append(new_shape)
        self.canvas.add_entry_visual(new_shape, self.galactic_map)
        self._refresh_tree()
        self._on_entry_selected(new_shape.id)

    def _on_entry_selected(self, entry_id, from_tree=False):
        if entry_id is None:
            self.properties_panel.set_entry(None, self.galactic_map)
            if not from_tree:
                self.tree.clearSelection()
            return
            
        entry = next((e for e in self.galactic_map.entries if e.id == entry_id), None)
        self.properties_panel.set_entry(entry, self.galactic_map)
        if not from_tree:
            self.tree.blockSignals(True)
            self.tree.clearSelection()
            self._select_tree_item(entry_id)
            self.tree.blockSignals(False)

    def _on_entry_changed(self, entry_id):
        # Full refresh of visuals for simplicity, or just update the specific one
        # For now, let's just re-sync the visual for that entry
        entry = next((e for e in self.galactic_map.entries if e.id == entry_id), None)
        if entry:
            if entry_id in self.canvas.entry_to_visual:
                # Remove and re-add (simple approach)
                visual = self.canvas.entry_to_visual[entry_id]
                visual.parent = None
                del self.canvas.visual_to_entry[visual]
                del self.canvas.entry_to_visual[entry_id]
                
            self.canvas.add_entry_visual(entry, self.galactic_map)
            
            # If it's a star, we might need to update its planets
            if isinstance(entry, Star):
                for planet in self.galactic_map.get_planets():
                    if planet.parent_star_id == entry.id:
                        if planet.id in self.canvas.entry_to_visual:
                            visual = self.canvas.entry_to_visual[planet.id]
                            visual.parent = None
                            del self.canvas.visual_to_entry[visual]
                            del self.canvas.entry_to_visual[planet.id]
                        self.canvas.add_entry_visual(planet, self.galactic_map)
            self._refresh_tree()

    def _save_map(self):
        path, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Save Galactic Map", "", "JSON Files (*.json)")
        if path:
            with open(path, 'w') as f:
                f.write(self.galactic_map.model_dump_json(indent=2))

    def _load_map(self):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Open Galactic Map", "", "JSON Files (*.json)")
        if path:
            with open(path, 'r') as f:
                data = json.load(f)
                self.galactic_map = GalacticMap.model_validate(data)
                self.canvas.update_from_model(self.galactic_map)
                self._refresh_tree()
                self.properties_panel.set_entry(None, self.galactic_map)

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec())

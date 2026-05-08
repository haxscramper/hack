# ASCII Diagrammer - Active Plan

## Overview
GUI application for editing ASCII diagrams using Python, PySide6, and UV.

## Stack
- Package manager / runner: `uv` (`pyproject.toml`, `src` layout)
- GUI: `PySide6`
- Testing: `pytest` + `pytest-qt`

## Architecture Decisions

### Grid and Coordinates
- The scene is organized in a cell-based grid. All shape coordinates and geometry values are whole integers (cell units).
- Cell size is computed from a fixed monospaced font (Courier New, 12pt) using `QFontMetrics`. The cell dimensions (width in px, height in px) set the grid unit.
- Shape coordinates are relative to the parent shape's upper-left corner.
- Absolute positions are computed on-the-fly by summing ancestor offsets, used only for rendering and hit-testing.
- Moving shapes snaps to integer grid values. Parent moving moves children visually; children maintain their constant relative model coordinates.

### Rendering Strategy
- **Vector rendering**: Shapes rendered as regular geometry running through midpoints of cells. No configurable parameters. For example, a rectangle at `(1,1)` with size `(4,3)` is rendered from `(1.5,1.5)` to `(4.5,3.5)`.
- **Raster rendering**: Geometry rendered onto a 2D ASCII grid using per-shape character sets.
- Both modes can be enabled simultaneously. Raster overlay is a non-selectable `QGraphicsItem` drawn beneath vector items.
- Raster grid covers all shapes plus a 10-cell buffer in all directions. Grid is recomputed when the model emits structural or geometry changes.
- For text shapes, raster rendering draws the text lines into the ASCII grid respecting width, height, horizontal justification, and vertical alignment.

### Overlap Modes
Configurable per shape:
1. **OVERWRITE**: Second shape's characters replace existing.
2. **EMPTY_ONLY**: Second shape writes only into empty cells.
3. **MERGE**: Uses basic character-specific intersection rules:
   - `|` + `-` or `-` + `|` \rightarrow `+`
   - Intersection with existing `+` keeps `+`
   - Extensible: `AsciiGrid.write()` accepts a rule dictionary for future expansion.

### Edge Rendering
- **POLYLINE**: Straight segments through start, bends, and end points.
- **SPLINE (Bezier)**: Based on number of bends:
  - 0 bends: straight line.
  - 1 bend: quadratic Bezier.
  - 2 bends: cubic Bezier.
  - >2 bends: fallback to polyline.
- **ORTHOGONAL**: Manhattan axis-aligned routing. If explicit bends exist, routing snaps through bends using horizontal/vertical segments.

### Default Character Sets (configurable per shape instance)
| Shape | Defaults |
|---|---|
| Rectangle | Corners `+`, Top/Bottom `-`, Sides `\|` |
| Edge | Horizontal `-`, Vertical `\|`, Arrowhead `>`, Bend point `+` |
| Ellipse | Outline `*` |
| Text | Plain text with wrapping and alignment |

### Shape Palette (Left Panel)
- Scrollable list of shape previews (Rectangle, Edge, Ellipse, Text).
- Previews rendered on scaled-down `QGraphicsView`-like widgets using the same rendering class as the scene.
- Drag from palette to scene creates a default shape at the snapped drop coordinates.

### Main Scene View (Center)
- `QGraphicsView` with infinite-ish canvas (negative coordinates allowed).
- Zoom, pan, rubber-band multi-select, Ctrl toggle selection.
- Objects rendered in model tree order (parents first, then children on top).
- Dragging elements on the scene moves the model coordinates (through model API), which updates the view.

### Properties Panel + Scene Tree (Right Panel)
- **Properties Panel**: Top section showing editable fields for the currently selected shape.
- **Scene Tree**: Bottom section, `QTreeView` bound to `SceneModel`.
  - Supports drag-drop for re-parenting.
  - Deleting elements.
  - Selecting elements syncs with the scene view.
  - Each node displays shape type and ID.

### Data Export Import
- Model is single source of truth for all properties.
- JSON serialization/deserialization via model APIs.
- ASCII export:
  - Opens dialog with monospaced text and Copy button.
  - Exports always the full scene.
  - Cropped to the overall bounding box of all shapes.

## File Structure
```
ascii_diagram/
├── pyproject.toml
└── src/
    └── ascii_diagram/
        ├── main.py                 # Entry point; constructs QApplication and MainWindow
        ├── model/
        │   ├── scene_model.py      # QAbstractItemModel; single source of truth
        │   ├── scene_item.py       # Tree node base (parent/child, shape type, id)
        │   ├── shape_properties.py # Dataclasses: RectData, EdgeData, EllipseData, TextData
        │   └── enums.py            # EdgeType, OverlapMode, HJustify, VAlign
        ├── rendering/
        │   ├── ascii_grid.py       # 2D char buffer + overlap resolution rules
        │   ├── raster_renderer.py  # Stamps shapes into AsciiGrid from model state
        │   └── vector_renderer.py  # Factory for QGraphicsItems from model geometry
        ├── view/
        │   ├── main_window.py      # QMainWindow with left/center/right layout
        │   ├── scene_view.py       # QGraphicsView: zoom, pan, rubber-band, drag-drop target
        │   ├── shape_palette.py    # Left panel: preview list with internal drag-out
        │   ├── properties_panel.py # Right panel top: editor for selected model index
        │   ├── scene_tree.py       # Right panel bottom: QTreeView bound to SceneModel
        │   ├── export_dialog.py    # Modal dialog: monospace label + Copy button
        │   └── graphics/
        │       ├── raster_overlay.py   # Non-selectable full-scene QGraphicsItem (cache)
        │       ├── base_shape_item.py  # Reads coords from model; handles drag moves
        │       └── shape_items.py      # Rect, Edge, Ellipse, Text vector items
        ├── io/
        │   └── scene_io.py         # JSON serialize / deserialize via model APIs
        └── interaction/
            └── drag_drop.py        # Shared mime-type helpers for palette and tree reparenting
└── tests/
    ├── conftest.py                 # qtbot fixture, reusable model/scene fixtures
    ├── test_model/
    ├── test_rendering/
    └── test_gui/
```

## Component Responsibilities
- **model/**: Stores all properties. Coordinates are relative integers. Structural changes happen only through `QAbstractItemModel` APIs (`insertRows`, `moveRows`, `removeRows`), emitting standard signals consumed by the scene view and tree view.
- **rendering/**: `raster_renderer.py` iterates the model to write into `ascii_grid.py`, applying `OverlapMode`. `vector_renderer.py` creates geometry items that read from the model on paint/move.
- **view/graphics/raster_overlay.py**: Caches the full-scene ASCII grid (computed size = shape bounds + 10-cell buffer). Rebuilds when the model emits structural or geometry changes. Drawn beneath selectable vector items.
- **interaction/**: Encapsulates drag/drop MIME data and drop logic so `shape_palette` (creation) and `scene_tree` (re-parenting) share the same model-update path.

## Testing Strategy
- **Model Unit Tests** (`test_model/`): Validate tree structure, relative/absolute coordinate math, signals, and JSON round-trips without a GUI event loop where possible.
- **Rendering Tests** (`test_rendering/`): Build models programmatically, feed to `raster_renderer` and `ascii_grid`, assert against expected 2D string arrays. No GUI required.
- **GUI Integration Tests** (`test_gui/`): Use `pytest-qt` (`qtbot`) to instantiate widgets, simulate mouse/keyboard/drag-drop, and verify that model state and view state remain synchronized.

## Detailed Test Collection

### Model & Coordinates
- `test_insert_root`: Insert shapes via `insertRows` at root; verify row count and `QModelIndex` parent.
- `test_insert_nested`: Insert child under parent; verify tree depth and `rowsInserted` signal.
- `test_relative_coords_unchanged_on_parent_move`: Changing parent position updates only the parent's stored offset; child offsets remain constant in the model.
- `test_absolute_coord_accumulation`: Nested depth \geq 2 computes absolute position by summing ancestor offsets; assert integer math.
- `test_reparent_via_moveRows`: Move node between parents via model API; assert new parent, correct signals, and absolute rendering position updated.
- `test_delete_subtree`: Remove parent node; assert all descendants removed from model.
- `test_geometry_change_signals`: Modify width/height via `setData`; assert `dataChanged` emitted with correct index/role.

### Serialization
- `test_json_roundtrip`: Complex nested scene serialized to JSON and deserialized into a fresh model; assert full tree equality and property equality.
- `test_json_unknown_shape_raises`: Input with unknown shape type during deserialization raises an exception (no silent fallback).

### ASCII Grid & Raster Rendering
- `test_grid_size_with_buffer`: Shapes placed at extremes produce a grid covering all shapes plus a 10-cell buffer.
- `test_grid_invalidation_on_move`: Change shape coordinates; assert cached grid rebuilds and overlay updates.
- `test_rectangle_raster_charset`: Rectangle with custom corners/sides renders exact expected characters.
- `test_edge_polyline_raster`: Polyline with bends renders chars along straight segments and bend points.
- `test_edge_orthogonal_raster`: Orthogonal edge renders only horizontal/vertical runs.
- `test_edge_spline_raster`: Spline edge renders approximate curve via interpolated chars.
- `test_ellipse_raster`: Closed ellipse curve renders expected characters on grid.
- `test_text_wrapping`: Text exceeding width wraps correctly within bounding box.
- `test_text_hjustify_left_right_center`: Horizontal justification offsets text correctly within box.
- `test_text_valign_top_center_bottom`: Vertical alignment offsets text correctly within box.
- `test_overlap_mode_overwrite`: Second shape overwrites first at shared cells.
- `test_overlap_mode_empty_only`: Second shape writes only where no character exists.
- `test_overlap_intersection_plus`: Intersection rules convert `|` + `-` crossing into `+`.
- `test_raster_excludes_invisible_shapes`: Hidden or outlier shapes still contribute to grid bounds but render normally.

### Vector Rendering
- `test_vector_midpoint_offset`: Rectangle at `(1,1)` size `(4,3)` produces vector rect from `(1.5,1.5)` to `(4.5,3.5)`.
- `test_vector_ignores_charset`: Vector renderer never consults charset properties.

### Export
- `test_export_crops_to_bounds`: Negative and positive coordinates exist; exported string cropped to exact bounding box.
- `test_export_always_full_scene`: Export output contains every shape regardless of current viewport.
- `test_export_dialog_copy`: Simulate clicking Copy button; assert clipboard contains exported ASCII.

### GUI & Integration
- `test_palette_preview_renders`: Palette widgets display scaled-down vector representations.
- `test_palette_drag_drop_creates_shape`: Simulate drag from palette to scene; assert model insertion at grid-snapped coordinates.
- `test_scene_drag_snaps_to_grid`: Move existing shape; assert final model coordinates are whole integers.
- `test_rubber_band_multi_select`: Rubber-band drag selects multiple items.
- `test_ctrl_toggle_select`: Ctrl-click adds/removes items from selection.
- `test_parent_move_translates_children`: Drag parent; children translate visually while model-relative coordinates stay fixed.
- `test_scene_selection_syncs_tree`: Select on canvas; tree updates to same `QModelIndex` set.
- `test_tree_selection_syncs_scene_and_properties`: Select in tree; canvas highlights and properties panel populates.
- `test_tree_drag_reparents`: Drag node in tree onto new parent; assert model parent change and view update.
- `test_property_edit_updates_model_and_view`: Edit width in properties panel; model updates, vector repaints, raster rebuilds.
- `test_zoom_pan`: Zoom changes scene transform; pan changes view translation.
- `test_raster_overlay_non_selectable`: Click on raster overlay does not alter selection.

## Active Build Tasks
```json
[
  {
    "content": "Create project structure: pyproject.toml, directories, __init__.py files",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Implement model layer: enums, shape properties, scene item, scene model",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Implement rendering layer: ascii grid + overlap rules, raster renderer, vector renderer",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Implement view/graphics: base shape item, shape items (rect, edge, ellipse, text), raster overlay",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Implement view widgets: main window, scene view, shape palette, properties panel, scene tree, export dialog",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Implement interaction & IO: drag_drop, scene_io",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Implement main entry point",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Create tests: conftest, model tests, rendering tests, export tests, GUI integration tests",
    "priority": "high",
    "status": "pending"
  },
  {
    "content": "Run tests and fix any issues",
    "priority": "high",
    "status": "pending"
  }
]```

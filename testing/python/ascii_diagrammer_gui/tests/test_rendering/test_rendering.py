"""Rendering tests: ASCII grid, raster rendering, overlap rules."""

import pytest

from ascii_diagram.model.enums import (
    EdgeType,
    HJustify,
    OverlapMode,
    ShapeType,
    VAlign,
)
from ascii_diagram.model.scene_model import SceneModel
from ascii_diagram.model.shape_properties import (
    EdgeData,
    EllipseData,
    RectData,
    TextData,
)
from ascii_diagram.rendering.ascii_grid import (
    AsciiGrid,
    DEFAULT_INTERSECTION_RULES,
)
from ascii_diagram.rendering.raster_renderer import (
    render_item,
    render_scene_to_grid,
)


class TestAsciiGrid:

    def test_basic_write(self):
        grid = AsciiGrid()
        grid.write(0, 0, "A")
        assert grid.get(0, 0) == "A"
        assert grid.get(1, 1) == " "

    def test_overwrite_mode(self):
        grid = AsciiGrid()
        grid.write(0, 0, "A", OverlapMode.OVERWRITE)
        grid.write(0, 0, "B", OverlapMode.OVERWRITE)
        assert grid.get(0, 0) == "B"

    def test_empty_only_mode(self):
        grid = AsciiGrid()
        grid.write(0, 0, "A", OverlapMode.EMPTY_ONLY)
        grid.write(0, 0, "B", OverlapMode.EMPTY_ONLY)
        assert grid.get(0, 0) == "A"
        grid.write(1, 0, "C", OverlapMode.EMPTY_ONLY)
        assert grid.get(1, 0) == "C"

    def test_merge_mode_intersection(self):
        grid = AsciiGrid()
        grid.write(0, 0, "|", OverlapMode.MERGE)
        grid.write(0, 0, "-", OverlapMode.MERGE)
        assert grid.get(0, 0) == "+"

    def test_merge_mode_no_rule(self):
        grid = AsciiGrid()
        grid.write(0, 0, "A", OverlapMode.MERGE)
        grid.write(0, 0, "B", OverlapMode.MERGE)
        assert grid.get(0, 0) == "A"

    def test_grid_bounds(self):
        grid = AsciiGrid()
        grid.write(-2, 3, "X")
        grid.write(5, -1, "Y")
        bounds = grid.bounds()
        assert bounds == (-2, -1, 6, 4)

    def test_grid_to_string_cropped(self):
        grid = AsciiGrid()
        grid.write(0, 0, "A")
        grid.write(2, 1, "B")
        text = grid.to_string(crop_to_bounds=True)
        lines = text.split("\n")
        assert len(lines) == 2
        assert lines[0] == "A  "
        assert lines[1] == "  B"

    def test_grid_to_lines(self):
        grid = AsciiGrid()
        grid.write(0, 0, "X")
        lines = grid.to_lines()
        assert lines == ["X"]

    def test_custom_intersection_rules(self):
        grid = AsciiGrid()
        rules = {("A", "B"): "C"}
        grid.write(0, 0, "A")
        grid.write(0, 0, "B", OverlapMode.MERGE, rules)
        assert grid.get(0, 0) == "C"

    def test_grid_size_with_buffer(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.RECTANGLE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, RectData)
        item.properties.x = 1
        item.properties.y = 1
        item.properties.width = 2
        item.properties.height = 1

        grid = render_scene_to_grid(model)
        bounds = grid.bounds()
        assert bounds[0] == 1
        assert bounds[1] == 1
        assert bounds[2] == 3
        assert bounds[3] == 2


class TestRectangleRaster:

    def test_rectangle_basic(self):
        model = SceneModel()
        model.insert_item(ShapeType.RECTANGLE)
        grid = render_scene_to_grid(model)
        text = grid.to_string()
        assert "+" in text
        assert "-" in text
        assert "|" in text

    def test_rectangle_custom_charset(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.RECTANGLE)
        item = model.item_from_index(idx)
        assert item is not None
        props = item.properties
        assert isinstance(props, RectData)
        props.width = 5
        props.height = 3
        props.corner_ch = "#"
        props.top_ch = "="
        props.left_ch = "!"
        props.right_ch = "!"
        props.bottom_ch = "="

        grid = render_scene_to_grid(model)
        text = grid.to_string()
        assert "#===#" in text
        assert "!   !" in text

    def test_rectangle_zero_size(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.RECTANGLE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, RectData)
        item.properties.height = 0
        grid = render_scene_to_grid(model)
        assert grid.to_string() == ""

    def test_rectangle_1x1(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.RECTANGLE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, RectData)
        item.properties.width = 1
        item.properties.height = 1
        grid = render_scene_to_grid(model)
        assert grid.get(0, 0) == item.properties.corner_ch


class TestEdgeRaster:

    def test_edge_polyline_straight(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.EDGE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EdgeData)
        item.properties.start.setX(0)
        item.properties.start.setY(0)
        item.properties.end.setX(5)
        item.properties.end.setY(0)

        grid = render_scene_to_grid(model)
        line = grid.to_lines()[0]
        assert ">" in line
        assert "-" in line

    def test_edge_polyline_with_bends(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.EDGE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EdgeData)
        item.properties.start.setX(0)
        item.properties.start.setY(0)
        item.properties.end.setX(3)
        item.properties.end.setY(2)
        item.properties.edge_type = EdgeType.POLYLINE
        item.properties.bend_char = "+"

        grid = render_scene_to_grid(model)
        lines = grid.to_lines()
        assert len(lines) >= 3

    def test_edge_orthogonal(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.EDGE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EdgeData)
        item.properties.start.setX(0)
        item.properties.start.setY(0)
        item.properties.end.setX(2)
        item.properties.end.setY(2)

        grid = render_scene_to_grid(model)
        lines = grid.to_lines()
        assert len(lines) >= 3

    def test_edge_spline_straight(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.EDGE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EdgeData)
        item.properties.start.setX(0)
        item.properties.start.setY(0)
        item.properties.end.setX(5)
        item.properties.end.setY(0)
        item.properties.edge_type = EdgeType.SPLINE

        grid = render_scene_to_grid(model)
        line = grid.to_lines()[0]
        assert "-" in line

    def test_edge_spline_with_one_bend(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.EDGE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EdgeData)
        item.properties.start.setX(0)
        item.properties.start.setY(2)
        item.properties.end.setX(4)
        item.properties.end.setY(2)
        item.properties.edge_type = EdgeType.SPLINE

        grid = render_scene_to_grid(model)
        assert grid.to_string() != ""

    def test_edge_spline_with_two_bends(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.EDGE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EdgeData)
        item.properties.start.setX(0)
        item.properties.start.setY(0)
        item.properties.end.setX(4)
        item.properties.end.setY(4)
        item.properties.bends.append(item.properties.start.__class__(3, 4))
        item.properties.edge_type = EdgeType.SPLINE

        grid = render_scene_to_grid(model)
        assert grid.to_string() != ""

    def test_edge_spline_many_bends_fallback(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.EDGE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EdgeData)
        item.properties.edge_type = EdgeType.SPLINE
        item.properties.bends.append(item.properties.start.__class__(1, 0))
        item.properties.bends.append(item.properties.start.__class__(2, 0))
        item.properties.bends.append(item.properties.start.__class__(3, 0))
        grid = render_scene_to_grid(model)
        assert grid.to_string() != ""


class TestEllipseRaster:

    def test_ellipse_render(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.ELLIPSE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, EllipseData)
        item.properties.width = 4
        item.properties.height = 2

        grid = render_scene_to_grid(model)
        text = grid.to_string()
        assert "*" in text


class TestTextRaster:

    def test_text_render(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.TEXT)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, TextData)
        item.properties.text = "Hello"
        item.properties.width = 10
        item.properties.height = 2

        grid = render_scene_to_grid(model)
        text = grid.to_string()
        assert "Hello" in text

    def test_text_wrapping(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.TEXT)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, TextData)
        item.properties.text = "ABCDEFGHIJ"
        item.properties.width = 5
        item.properties.height = 3

        grid = render_scene_to_grid(model)
        lines = grid.to_lines()
        assert len(lines) <= 3
        assert "ABCDE" in lines[0]
        assert "FGHIJ" in lines[1]

    def test_text_hjustify_center(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.TEXT)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, TextData)
        item.properties.text = "Hi"
        item.properties.width = 8
        item.properties.height = 2
        item.properties.h_justify = HJustify.CENTER

        grid = render_scene_to_grid(model)
        line = grid.to_string(crop_to_bounds=False).split("\n")[0]
        assert "Hi" in line
        assert line.index("H") > 0

    def test_text_hjustify_right(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.TEXT)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, TextData)
        item.properties.text = "OK"
        item.properties.width = 8
        item.properties.height = 2
        item.properties.h_justify = HJustify.RIGHT

        grid = render_scene_to_grid(model)
        line = grid.to_string(crop_to_bounds=False).split("\n")[0]
        assert "OK" in line
        assert line.index("O") > 3

    def test_text_valign_center(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.TEXT)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, TextData)
        item.properties.text = "Hi"
        item.properties.width = 8
        item.properties.height = 5
        item.properties.v_align = VAlign.CENTER

        grid = render_scene_to_grid(model)
        assert "Hi" in grid.to_string()

    def test_text_valign_bottom(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.TEXT)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, TextData)
        item.properties.text = "Hi"
        item.properties.width = 8
        item.properties.height = 5
        item.properties.v_align = VAlign.BOTTOM

        grid = render_scene_to_grid(model)
        lines = grid.to_lines()
        assert "Hi" in lines[-1]


class TestOverlap:

    def test_overlap_overwrite(self):
        model = SceneModel()
        model.insert_item(ShapeType.RECTANGLE)
        idx2 = model.insert_item(ShapeType.RECTANGLE)
        item2 = model.item_from_index(idx2)
        assert item2 is not None
        assert isinstance(item2.properties, RectData)
        item2.properties.x = 0
        item2.properties.y = 0
        item2.properties.overlap_mode = OverlapMode.OVERWRITE
        item2.properties.corner_ch = "X"
        item2.properties.width = 1
        item2.properties.height = 1

        grid = render_scene_to_grid(model)
        assert grid.get(0, 0) == "X"

    def test_overlap_empty_only(self):
        model = SceneModel()
        model.insert_item(ShapeType.RECTANGLE)
        idx2 = model.insert_item(ShapeType.RECTANGLE)
        item2 = model.item_from_index(idx2)
        assert item2 is not None
        assert isinstance(item2.properties, RectData)
        item2.properties.x = 1
        item2.properties.y = 1
        item2.properties.width = 3
        item2.properties.height = 3
        item2.properties.overlap_mode = OverlapMode.EMPTY_ONLY
        item2.properties.corner_ch = "X"
        item2.properties.top_ch = "="
        item2.properties.left_ch = "!"
        item2.properties.right_ch = "!"
        item2.properties.bottom_ch = "="

        grid = render_scene_to_grid(model)
        assert grid.get(0, 0) == "+"


class TestExport:

    def test_export_crops_to_bounds(self):
        model = SceneModel()
        idx = model.insert_item(ShapeType.RECTANGLE)
        item = model.item_from_index(idx)
        assert item is not None
        assert isinstance(item.properties, RectData)
        item.properties.x = -2
        item.properties.y = -1
        item.properties.width = 5
        item.properties.height = 3

        grid = render_scene_to_grid(model)
        text = grid.to_string(crop_to_bounds=True)
        lines = text.split("\n")
        assert len(lines) == 3

    def test_export_full_scene(self):
        model = SceneModel()
        idx1 = model.insert_item(ShapeType.RECTANGLE)
        item1 = model.item_from_index(idx1)
        assert item1 is not None
        assert isinstance(item1.properties, RectData)
        item1.properties.x = 0
        item1.properties.y = 0
        item1.properties.width = 3
        item1.properties.height = 2

        idx2 = model.insert_item(ShapeType.TEXT)
        item2 = model.item_from_index(idx2)
        assert item2 is not None
        assert isinstance(item2.properties, TextData)
        item2.properties.x = 10
        item2.properties.y = 5
        item2.properties.text = "X"

        grid = render_scene_to_grid(model)
        text = grid.to_string(crop_to_bounds=True)
        assert "X" in text
        assert "+" in text

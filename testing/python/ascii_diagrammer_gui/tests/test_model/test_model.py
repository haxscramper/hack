"""Model unit tests: tree structure, coordinates, signals, JSON round-trips."""

from PySide6.QtCore import QModelIndex, Qt

from ascii_diagram.model.enums import ShapeType
from ascii_diagram.model.scene_model import (
    SceneModel,
    ITEM_ROLE,
    ITEM_ID_ROLE,
)
from ascii_diagram.model.shape_properties import RectData, EllipseData


class TestModelStructure:

    def test_insert_root(self, empty_model):
        idx = empty_model.insert_item(ShapeType.RECTANGLE)
        assert idx.isValid()
        assert empty_model.rowCount() == 1
        assert idx.parent() == QModelIndex()

    def test_insert_nested(self, empty_model):
        parent_idx = empty_model.insert_item(ShapeType.RECTANGLE)
        child_idx = empty_model.insert_item(ShapeType.ELLIPSE, parent_idx)
        assert child_idx.isValid()
        assert child_idx.parent() == parent_idx
        assert empty_model.rowCount(parent_idx) == 1

    def test_insert_at_specific_row(self, empty_model):
        empty_model.insert_item(ShapeType.RECTANGLE)
        idx2 = empty_model.insert_item(ShapeType.ELLIPSE, row=0)
        assert idx2.isValid()
        item0 = empty_model.item_from_index(empty_model.index(0, 0))
        assert item0.shape_type == ShapeType.ELLIPSE

    def test_relative_coords_unchanged_on_parent_move(self, nested_model):
        parent_idx = nested_model.index(0, 0)
        child_idx = nested_model.index(0, 0, parent_idx)
        parent_item = nested_model.item_from_index(parent_idx)
        child_item = nested_model.item_from_index(child_idx)

        orig_child_x = child_item.properties.x
        orig_child_y = child_item.properties.y

        parent_item.properties.x += 10
        parent_item.properties.y += 5

        assert child_item.properties.x == orig_child_x
        assert child_item.properties.y == orig_child_y

    def test_absolute_coord_accumulation(self, empty_model):
        p_idx = empty_model.insert_item(ShapeType.RECTANGLE)
        p_item = empty_model.item_from_index(p_idx)
        p_item.properties.x = 10
        p_item.properties.y = 20

        c_idx = empty_model.insert_item(ShapeType.ELLIPSE, p_idx)
        c_item = empty_model.item_from_index(c_idx)
        c_item.properties.x = 5
        c_item.properties.y = 3

        abs_pos = c_item.absolute_position()
        assert abs_pos.x() == 15
        assert abs_pos.y() == 23

    def test_reparent_via_moveRows(self, empty_model):
        p1_idx = empty_model.insert_item(ShapeType.RECTANGLE)
        p2_idx = empty_model.insert_item(ShapeType.RECTANGLE)
        child_idx = empty_model.insert_item(ShapeType.ELLIPSE, p1_idx)

        assert empty_model.moveRows(p1_idx, 0, 1, p2_idx, 0) is True
        assert empty_model.rowCount(p1_idx) == 0
        assert empty_model.rowCount(p2_idx) == 1
        child = empty_model.item_from_index(empty_model.index(0, 0, p2_idx))
        assert child.shape_type == ShapeType.ELLIPSE

    def test_reparent_prevent_ancestor_cycle(self, empty_model):
        p_idx = empty_model.insert_item(ShapeType.RECTANGLE)
        c_idx = empty_model.insert_item(ShapeType.ELLIPSE, p_idx)

        assert empty_model.moveRows(p_idx, 0, 1, c_idx, 0) is False
        assert empty_model.rowCount(p_idx) == 1

    def test_delete_subtree(self, nested_model):
        parent_idx = nested_model.index(0, 0)
        assert nested_model.rowCount(parent_idx) == 1

        nested_model.remove_item(parent_idx)
        assert nested_model.rowCount() == 0

    def test_geometry_change_signals(self, empty_model):
        signals_received = []

        def on_data_changed(tl, br):
            signals_received.append((tl, br))

        empty_model.dataChanged.connect(on_data_changed)

        idx = empty_model.insert_item(ShapeType.RECTANGLE)
        item = empty_model.item_from_index(idx)
        item.properties.width = 99
        empty_model.setData(idx, item.properties, Qt.UserRole + 101)
        empty_model.dataChanged.emit(idx, idx)

        assert len(signals_received) >= 1

    def test_model_reset(self, complex_model):
        assert complex_model.rowCount() == 4
        complex_model.reset_model()
        assert complex_model.rowCount() == 0

    def test_display_name(self, empty_model):
        idx = empty_model.insert_item(ShapeType.EDGE)
        item = empty_model.item_from_index(idx)
        name = item.display_name()
        assert "EDGE" in name
        assert str(item.item_id) in name


class TestSerialization:

    def test_json_roundtrip(self, complex_model):
        from ascii_diagram.io.scene_io import (
            model_to_json,
            json_to_model,
        )

        original_count = complex_model.rowCount()
        json_str = model_to_json(complex_model)

        new_model = SceneModel()
        json_to_model(new_model, json_str)

        assert new_model.rowCount() == original_count

        orig_items = complex_model.all_items()
        new_items = new_model.all_items()
        assert len(orig_items) == len(new_items)

        for oi, ni in zip(orig_items, new_items):
            assert oi.shape_type == ni.shape_type
            assert type(oi.properties) == type(ni.properties)

    def test_json_unknown_shape_raises(self):
        from ascii_diagram.io.scene_io import json_to_model

        bad_json = '{"shapes": [{"id": 1, "shape_type": "FOOBAR", "properties": {}, "children": []}]}'

        model = SceneModel()
        with pytest.raises(Exception):
            json_to_model(model, bad_json)

    def test_json_preserves_properties(self, empty_model):
        from ascii_diagram.io.scene_io import (
            model_to_json,
            json_to_model,
        )

        idx = empty_model.insert_item(ShapeType.RECTANGLE)
        item = empty_model.item_from_index(idx)
        props = item.properties
        props.width = 42
        props.height = 7
        props.corner_ch = "#"

        json_str = model_to_json(empty_model)
        new_model = SceneModel()
        json_to_model(new_model, json_str)

        new_item = new_model.item_from_index(new_model.index(0, 0))
        assert isinstance(new_item.properties, RectData)
        assert new_item.properties.width == 42
        assert new_item.properties.height == 7
        assert new_item.properties.corner_ch == "#"


import pytest

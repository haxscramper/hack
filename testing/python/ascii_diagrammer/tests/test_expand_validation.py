import pytest

from diagram_layout.errors import ValidationError
from diagram_layout.expand import expand_diagram
from diagram_layout.schema import DiagramInput


def test_duplicate_shape_ids_fail():
    diagram = DiagramInput.model_validate(
        {
            "canvas_width": 100,
            "canvas_height": 100,
            "shapes": [
                {
                    "id": "a",
                    "shape_type": "rect",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 10},
                        "h": {"type": "axis-value", "fixed": 10},
                    },
                    "position": {"type": "absolute", "x": 0, "y": 0},
                },
                {
                    "id": "a",
                    "shape_type": "rect",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 10},
                        "h": {"type": "axis-value", "fixed": 10},
                    },
                    "position": {"type": "absolute", "x": 0, "y": 0},
                },
            ],
        }
    )
    with pytest.raises(ValidationError):
        expand_diagram(diagram)


def test_non_group_with_children_fails():
    diagram = DiagramInput.model_validate(
        {
            "canvas_width": 100,
            "canvas_height": 100,
            "shapes": [
                {
                    "id": "a",
                    "shape_type": "rect",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 50},
                        "h": {"type": "axis-value", "fixed": 50},
                    },
                    "position": {"type": "absolute", "x": 0, "y": 0},
                    "children": [
                        {
                            "id": "b",
                            "shape_type": "rect",
                            "size": {
                                "type": "fixed",
                                "w": {"type": "axis-value", "fixed": 10},
                                "h": {"type": "axis-value", "fixed": 10},
                            },
                            "position": {"type": "absolute", "x": 1, "y": 1},
                        }
                    ],
                }
            ],
        }
    )
    with pytest.raises(ValidationError):
        expand_diagram(diagram)


def test_descendant_reference_fails():
    diagram = DiagramInput.model_validate(
        {
            "canvas_width": 300,
            "canvas_height": 300,
            "shapes": [
                {
                    "id": "g",
                    "shape_type": "group",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 200},
                        "h": {"type": "axis-value", "fixed": 200},
                    },
                    "position": {"type": "absolute", "x": 10, "y": 10},
                    "children": [
                        {
                            "id": "p",
                            "shape_type": "group",
                            "size": {
                                "type": "fixed",
                                "w": {"type": "axis-value", "fixed": 100},
                                "h": {"type": "axis-value", "fixed": 100},
                            },
                            "position": {"type": "absolute", "x": 20, "y": 20},
                            "children": [
                                {
                                    "id": "c",
                                    "shape_type": "rect",
                                    "size": {
                                        "type": "fixed",
                                        "w": {"type": "axis-value", "fixed": 10},
                                        "h": {"type": "axis-value", "fixed": 10},
                                    },
                                    "position": {"type": "absolute", "x": 1, "y": 1},
                                }
                            ],
                        }
                    ],
                },
                {
                    "id": "outside",
                    "shape_type": "rect",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 20},
                        "h": {"type": "axis-value", "fixed": 20},
                    },
                    "position": {
                        "type": "relative",
                        "relation": "right-of",
                        "target": {"shape_id": "c", "anchor": "bbox-right"},
                        "gap": 5,
                    },
                },
            ],
        }
    )
    with pytest.raises(ValidationError):
        expand_diagram(diagram)

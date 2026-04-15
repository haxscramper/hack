from diagram_layout.resolver import resolve_diagram
from diagram_layout.schema import DiagramInput


def shape_map(resolved):
    return {shape.id: shape for shape in resolved.shapes}


def test_absolute_and_relative_resolution():
    diagram = DiagramInput.model_validate(
        {
            "canvas_width": 500,
            "canvas_height": 300,
            "shapes": [
                {
                    "id": "a",
                    "shape_type": "rect",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 100},
                        "h": {"type": "axis-value", "fixed": 40},
                    },
                    "position": {"type": "absolute", "x": 10, "y": 20},
                },
                {
                    "id": "b",
                    "shape_type": "rect",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 80},
                        "h": {"type": "axis-value", "fixed": 40},
                    },
                    "position": {
                        "type": "conjunction",
                        "exprs": [
                            {
                                "type": "relative",
                                "relation": "right-of",
                                "target": {"shape_id": "a", "anchor": "bbox-right"},
                                "gap": 10,
                            },
                            {
                                "type": "align-with",
                                "anchors": [
                                    {"shape_id": "b", "anchor": "bbox-top"},
                                    {"shape_id": "a", "anchor": "bbox-top"},
                                ],
                                "axis": "y",
                            },
                        ],
                    },
                },
            ],
        }
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["a"].x == 10
    assert shapes["a"].y == 20
    assert shapes["b"].x == 120
    assert shapes["b"].y == 20


def test_percent_of_parent_defaults_to_parent():
    diagram = DiagramInput.model_validate(
        {
            "canvas_width": 400,
            "canvas_height": 200,
            "shapes": [
                {
                    "id": "g",
                    "shape_type": "group",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 200},
                        "h": {"type": "axis-value", "fixed": 100},
                    },
                    "position": {"type": "absolute", "x": 50, "y": 30},
                    "children": [
                        {
                            "id": "c",
                            "shape_type": "rect",
                            "size": {
                                "type": "percent-of",
                                "w": {"type": "axis-value", "pct": 50},
                                "h": {"type": "axis-value", "pct": 50},
                            },
                            "position": {
                                "type": "percent-of",
                                "x_pct": 10,
                                "y_pct": 20,
                            },
                        }
                    ],
                }
            ],
        }
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["c"].w == 100
    assert shapes["c"].h == 50
    assert shapes["c"].x == 70
    assert shapes["c"].y == 50

from diagram_layout.resolver import resolve_diagram
from diagram_layout.schema import DiagramInput


def shape_map(resolved):
    return {shape.id: shape for shape in resolved.shapes}


def test_line_points_are_resolved_to_absolute_coordinates():
    diagram = DiagramInput.model_validate(
        {
            "canvas_width": 300,
            "canvas_height": 200,
            "shapes": [
                {
                    "id": "line1",
                    "shape_type": "line",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 100},
                        "h": {"type": "axis-value", "fixed": 50},
                    },
                    "position": {"type": "absolute", "x": 20, "y": 30},
                    "line": {
                        "points": [
                            {"type": "point-literal", "x": 0, "y": 0},
                            {"type": "point-add", "left": {"type": "point-literal", "x": 10, "y": 5}, "right": {"type": "point-literal", "x": 5, "y": 5}},
                            {"type": "point-scale", "expr": {"type": "point-literal", "x": 20, "y": 10}, "factor": 2},
                        ]
                    },
                }
            ],
        }
    )

    resolved = resolve_diagram(diagram)
    line = shape_map(resolved)["line1"]

    assert [(p.x, p.y) for p in line.points] == [
        (20, 30),
        (35, 40),
        (60, 50),
    ]


def test_line_end_point_anchor_can_be_used():
    diagram = DiagramInput.model_validate(
        {
            "canvas_width": 500,
            "canvas_height": 300,
            "shapes": [
                {
                    "id": "line1",
                    "shape_type": "line",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 80},
                        "h": {"type": "axis-value", "fixed": 20},
                    },
                    "position": {"type": "absolute", "x": 50, "y": 60},
                    "line": {
                        "points": [
                            {"type": "point-literal", "x": 0, "y": 0},
                            {"type": "point-literal", "x": 40, "y": 10},
                        ]
                    },
                },
                {
                    "id": "r",
                    "shape_type": "rect",
                    "size": {
                        "type": "fixed",
                        "w": {"type": "axis-value", "fixed": 20},
                        "h": {"type": "axis-value", "fixed": 10},
                    },
                    "position": {
                        "type": "conjunction",
                        "exprs": [
                            {
                                "type": "relative",
                                "relation": "right-of",
                                "target": {"shape_id": "line1", "anchor": "end-point"},
                                "gap": 5,
                            },
                            {
                                "type": "relative",
                                "relation": "below",
                                "target": {"shape_id": "line1", "anchor": "end-point"},
                                "gap": 5,
                            },
                        ],
                    },
                },
            ],
        }
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["r"].x == 95
    assert shapes["r"].y == 75

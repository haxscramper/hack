from diagram_layout.resolver import resolve_diagram
from beartype import beartype
from diagram_layout.schema import DiagramInput


@beartype
def shape_map(resolved):
    return {shape.id: shape for shape in resolved.shapes}


@beartype
def test_spaced_by_anchors():
    diagram = DiagramInput.model_validate({
        "canvas_width":
            500,
        "canvas_height":
            200,
        "shapes": [
            {
                "id": "a",
                "shape_type": "rect",
                "size": {
                    "type": "fixed",
                    "w": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                    "h": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                },
                "position": {
                    "type": "absolute",
                    "x": 10,
                    "y": 10
                },
            },
            {
                "id": "b",
                "shape_type": "rect",
                "size": {
                    "type": "fixed",
                    "w": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                    "h": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                },
                "position": {
                    "type": "absolute",
                    "x": 0,
                    "y": 0
                },
            },
            {
                "id": "c",
                "shape_type": "rect",
                "size": {
                    "type": "fixed",
                    "w": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                    "h": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                },
                "position": {
                    "type": "absolute",
                    "x": 0,
                    "y": 0
                },
            },
        ],
        "constraints": [{
            "type": "spaced-by",
            "anchors": [
                {
                    "shape_id": "a",
                    "anchor": "bbox-left"
                },
                {
                    "shape_id": "b",
                    "anchor": "bbox-left"
                },
                {
                    "shape_id": "c",
                    "anchor": "bbox-left"
                },
            ],
            "spacing": {
                "x": 30,
                "y": 0
            },
        }],
    })

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["a"].x == 10
    assert shapes["b"].x == 40
    assert shapes["c"].x == 70
    assert shapes["a"].y == shapes["b"].y == shapes["c"].y


@beartype
def test_vertical_align_with_tolerance():
    diagram = DiagramInput.model_validate({
        "canvas_width":
            300,
        "canvas_height":
            200,
        "shapes": [
            {
                "id": "a",
                "shape_type": "rect",
                "size": {
                    "type": "fixed",
                    "w": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                    "h": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                },
                "position": {
                    "type": "absolute",
                    "x": 40,
                    "y": 10
                },
            },
            {
                "id": "b",
                "shape_type": "rect",
                "size": {
                    "type": "fixed",
                    "w": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                    "h": {
                        "type": "axis-value",
                        "fixed": 20
                    },
                },
                "position": {
                    "type": "conjunction",
                    "exprs": [{
                        "type": "absolute",
                        "x": 42,
                        "y": 50
                    },],
                },
            },
        ],
        "constraints": [{
            "type": "vertical-align",
            "anchors": [
                {
                    "shape_id": "a",
                    "anchor": "bbox-left"
                },
                {
                    "shape_id": "b",
                    "anchor": "bbox-left"
                },
            ],
            "max_offset": 5,
        }],
    })

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert abs(shapes["a"].x - shapes["b"].x) <= 5

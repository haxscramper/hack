from diagram_layout.resolver import resolve_diagram
from beartype import beartype
from diagram_layout.schema import *


@beartype
def shape_map(resolved):
    return {shape.id: shape for shape in resolved.shapes}


@beartype
def test_spaced_by_anchors():
    diagram = DiagramInput(
        canvas_width=500,
        canvas_height=200,
        shapes=[
            ShapeDefinition(
                id="a",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=20),
                    h=AxisValue(type="axis-value", fixed=20),
                ),
                position=AbsolutePos(type="absolute", x=10, y=10),
            ),
            ShapeDefinition(
                id="b",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=20),
                    h=AxisValue(type="axis-value", fixed=20),
                ),
                position=AbsolutePos(type="absolute", x=0, y=0),
            ),
            ShapeDefinition(
                id="c",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=20),
                    h=AxisValue(type="axis-value", fixed=20),
                ),
                position=AbsolutePos(type="absolute", x=0, y=0),
            ),
        ],
        constraints=[
            SpacedBy(
                type="spaced-by",
                anchors=[
                    AnchorRef(shape_id="a", anchor=BBoxAnchor.LEFT),
                    AnchorRef(shape_id="b", anchor=BBoxAnchor.LEFT),
                    AnchorRef(shape_id="c", anchor=BBoxAnchor.LEFT),
                ],
                spacing=Vec2(x=30, y=0),
            ),
        ],
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["a"].x == 10
    assert shapes["b"].x == 40
    assert shapes["c"].x == 70
    assert shapes["a"].y == shapes["b"].y == shapes["c"].y


@beartype
def test_vertical_align_with_tolerance():
    diagram = DiagramInput(
        canvas_width=300,
        canvas_height=200,
        shapes=[
            ShapeDefinition(
                id="a",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=20),
                    h=AxisValue(type="axis-value", fixed=20),
                ),
                position=AbsolutePos(type="absolute", x=40, y=10),
            ),
            ShapeDefinition(
                id="b",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=20),
                    h=AxisValue(type="axis-value", fixed=20),
                ),
                position=Conjunction(
                    type="conjunction",
                    exprs=[
                        AbsolutePos(type="absolute", x=42, y=50),
                    ],
                ),
            ),
        ],
        constraints=[
            VerticalAlign(
                type="vertical-align",
                anchors=[
                    AnchorRef(shape_id="a", anchor=BBoxAnchor.LEFT),
                    AnchorRef(shape_id="b", anchor=BBoxAnchor.LEFT),
                ],
                max_offset=5.0,
            ),
        ],
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert abs(shapes["a"].x - shapes["b"].x) <= 5

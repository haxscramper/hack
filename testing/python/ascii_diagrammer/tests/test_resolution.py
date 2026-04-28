from diagram_layout.resolver import resolve_diagram
from diagram_layout.schema import *
from beartype import beartype


def shape_map(resolved):
    return {shape.id: shape for shape in resolved.shapes}


def test_absolute_and_relative_resolution():
    diagram = DiagramInput(
        canvas_width=500,
        canvas_height=300,
        shapes=[
            ShapeDefinition(
                id="a",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=100),
                    h=AxisValue(type="axis-value", fixed=40),
                ),
                position=AbsolutePos(type="absolute", x=10, y=20),
            ),
            ShapeDefinition(
                id="b",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=80),
                    h=AxisValue(type="axis-value", fixed=40),
                ),
                position=Conjunction(
                    type="conjunction",
                    exprs=[
                        RelativePlacement(
                            type="relative",
                            relation=SpatialRelation.RIGHT_OF,
                            target=AnchorRef(shape_id="a", anchor=BBoxAnchor.RIGHT),
                            gap=10.0,
                        ),
                        AlignWith(
                            type="align-with",
                            anchors=[
                                AnchorRef(shape_id="b", anchor=BBoxAnchor.TOP),
                                AnchorRef(shape_id="a", anchor=BBoxAnchor.TOP),
                            ],
                            axis=AlignAxis.Y,
                        ),
                    ],
                ),
            ),
        ],
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["a"].x == 10
    assert shapes["a"].y == 20
    assert shapes["b"].x == 120
    assert shapes["b"].y == 20


def test_percent_of_parent_defaults_to_parent():
    diagram = DiagramInput(
        canvas_width=400,
        canvas_height=200,
        shapes=[
            ShapeDefinition(
                id="g",
                shape_type=ShapeType.GROUP,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=200),
                    h=AxisValue(type="axis-value", fixed=100),
                ),
                position=AbsolutePos(type="absolute", x=50, y=30),
                children=[
                    ShapeDefinition(
                        id="c",
                        shape_type=ShapeType.RECT,
                        size=PercentOfRefSize(
                            type="percent-of",
                            w=AxisValue(type="axis-value", pct=50),
                            h=AxisValue(type="axis-value", pct=50),
                        ),
                        position=PercentOfRefPos(
                            type="percent-of",
                            x_pct=10,
                            y_pct=20,
                        ),
                    ),
                ],
            ),
        ],
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["c"].w == 100
    assert shapes["c"].h == 50
    assert shapes["c"].x == 70
    assert shapes["c"].y == 50

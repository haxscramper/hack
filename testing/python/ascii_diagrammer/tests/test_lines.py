from diagram_layout.resolver import resolve_diagram
from diagram_layout.schema import *
from beartype import beartype


@beartype
def shape_map(resolved):
    return {shape.id: shape for shape in resolved.shapes}


@beartype
def test_line_points_are_resolved_to_absolute_coordinates():
    diagram = DiagramInput(
        canvas_width=300,
        canvas_height=200,
        shapes=[
            ShapeDefinition(
                id="line1",
                shape_type=ShapeType.LINE,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=100),
                    h=AxisValue(type="axis-value", fixed=50),
                ),
                position=AbsolutePos(type="absolute", x=20, y=30),
                line=LinePointExprs(points=[
                    PointLiteral(type="point-literal", x=0, y=0),
                    PointAdd(
                        type="point-add",
                        left=PointLiteral(type="point-literal", x=10, y=5),
                        right=PointLiteral(type="point-literal", x=5, y=5),
                    ),
                    PointScale(
                        type="point-scale",
                        expr=PointLiteral(type="point-literal", x=20, y=10),
                        factor=2.0,
                    ),
                ]),
            ),
        ],
    )

    resolved = resolve_diagram(diagram)
    line = shape_map(resolved)["line1"]

    assert [(p.x, p.y) for p in line.points] == [
        (20, 30),
        (35, 40),
        (60, 50),
    ]


@beartype
def test_line_end_point_anchor_can_be_used():
    diagram = DiagramInput(
        canvas_width=500,
        canvas_height=300,
        shapes=[
            ShapeDefinition(
                id="line1",
                shape_type=ShapeType.LINE,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=80),
                    h=AxisValue(type="axis-value", fixed=20),
                ),
                position=AbsolutePos(type="absolute", x=50, y=60),
                line=LinePointExprs(points=[
                    PointLiteral(type="point-literal", x=0, y=0),
                    PointLiteral(type="point-literal", x=40, y=10),
                ],),
            ),
            ShapeDefinition(
                id="r",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=20),
                    h=AxisValue(type="axis-value", fixed=10),
                ),
                position=Conjunction(
                    type="conjunction",
                    exprs=[
                        RelativePlacement(
                            type="relative",
                            relation=SpatialRelation.RIGHT_OF,
                            target=AnchorRef(shape_id="line1",
                                             anchor=PointAccessor.END_POINT),
                            gap=5.0,
                        ),
                        RelativePlacement(
                            type="relative",
                            relation=SpatialRelation.BELOW,
                            target=AnchorRef(shape_id="line1",
                                             anchor=PointAccessor.END_POINT),
                            gap=5.0,
                        ),
                    ],
                ),
            ),
        ],
    )

    resolved = resolve_diagram(diagram)
    shapes = shape_map(resolved)

    assert shapes["r"].x == 95
    assert shapes["r"].y == 75

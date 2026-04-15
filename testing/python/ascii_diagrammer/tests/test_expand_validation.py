import pytest

from diagram_layout.errors import ValidationError
from diagram_layout.expand import expand_diagram
from diagram_layout.schema import *
from beartype import beartype


@beartype
def test_duplicate_shape_ids_fail():
    diagram = DiagramInput(
        canvas_width=100,
        canvas_height=100,
        shapes=[
            ShapeDefinition(
                id="a",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=10),
                    h=AxisValue(type="axis-value", fixed=10),
                ),
                position=AbsolutePos(type="absolute", x=0, y=0),
            ),
            ShapeDefinition(
                id="a",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=10),
                    h=AxisValue(type="axis-value", fixed=10),
                ),
                position=AbsolutePos(type="absolute", x=0, y=0),
            ),
        ],
    )

    with pytest.raises(ValidationError):
        expand_diagram(diagram)


@beartype
def test_non_group_with_children_fails():
    diagram = DiagramInput(
        canvas_width=100,
        canvas_height=100,
        shapes=[
            ShapeDefinition(
                id="a",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=50),
                    h=AxisValue(type="axis-value", fixed=50),
                ),
                position=AbsolutePos(type="absolute", x=0, y=0),
                children=[
                    ShapeDefinition(
                        id="b",
                        shape_type=ShapeType.RECT,
                        size=FixedSize(
                            w=AxisValue(type="axis-value", fixed=10),
                            h=AxisValue(type="axis-value", fixed=10),
                        ),
                        position=AbsolutePos(type="absolute", x=1, y=1),
                    ),
                ],
            ),
        ],
    )

    with pytest.raises(ValidationError):
        expand_diagram(diagram)


@beartype
def test_descendant_reference_fails():
    diagram = DiagramInput(
        canvas_width=300,
        canvas_height=300,
        shapes=[
            ShapeDefinition(
                id="g",
                shape_type=ShapeType.GROUP,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=200),
                    h=AxisValue(type="axis-value", fixed=200),
                ),
                position=AbsolutePos(type="absolute", x=10, y=10),
                children=[
                    ShapeDefinition(
                        id="p",
                        shape_type=ShapeType.GROUP,
                        size=FixedSize(
                            w=AxisValue(type="axis-value", fixed=100),
                            h=AxisValue(type="axis-value", fixed=100),
                        ),
                        position=AbsolutePos(type="absolute", x=20, y=20),
                        children=[
                            ShapeDefinition(
                                id="c",
                                shape_type=ShapeType.RECT,
                                size=FixedSize(
                                    w=AxisValue(type="axis-value", fixed=10),
                                    h=AxisValue(type="axis-value", fixed=10),
                                ),
                                position=AbsolutePos(type="absolute", x=1, y=1),
                            ),
                        ],
                    ),
                ],
            ),
            ShapeDefinition(
                id="outside",
                shape_type=ShapeType.RECT,
                size=FixedSize(
                    w=AxisValue(type="axis-value", fixed=20),
                    h=AxisValue(type="axis-value", fixed=20),
                ),
                position=RelativePlacement(
                    type="relative",
                    relation=SpatialRelation.RIGHT_OF,
                    target=AnchorRef(shape_id="c", anchor=BBoxAnchor.RIGHT),
                    gap=5.0,
                ),
            ),
        ],
    )

    with pytest.raises(ValidationError):
        expand_diagram(diagram)

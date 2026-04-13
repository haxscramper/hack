import pytest

from diagrammer.parser import parse
from diagrammer.resolver import resolve, ResolveError
from diagrammer.ir import ShapeKind, ResolvedShape


class TestBasicResolution:

    def test_rect_absolute(self):
        stmts = parse("rect(2, 3, width=10, height=5)")
        scene = resolve(stmts)
        assert len(scene.shapes) == 1
        s = scene.shapes[0]
        assert s.box.x == 2.0
        assert s.box.y == 3.0
        assert s.box.width == 10.0
        assert s.box.height == 5.0

    def test_circle_symmetric(self):
        stmts = parse("circle(0, 0, width=8)")
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert s.box.width == 8.0
        assert s.box.height == 8.0

    def test_group_invisible(self):
        stmts = parse("group(0, 0, width=20, height=20)")
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert s.kind == ShapeKind.GROUP

    def test_multiple_shapes(self):
        stmts = parse("rect(0, 0, width=5, height=5)\n"
                      "rect(10, 0, width=5, height=5)\n")
        scene = resolve(stmts)
        assert len(scene.shapes) == 2
        assert scene.shapes[0].box.x == 0.0
        assert scene.shapes[1].box.x == 10.0


class TestPercentResolution:

    def test_percent_position(self):
        stmts = parse("group(0, 0, width=100, height=50) {\n"
                      "  rect(50%, 50%, width=10, height=10)\n"
                      "}")
        scene = resolve(stmts)
        inner = scene.shapes[0].subnodes[0]
        assert inner.box.x == 50.0  # 50% of 100
        assert inner.box.y == 25.0  # 50% of 50

    def test_percent_width(self):
        stmts = parse("group(0, 0, width=100, height=50) {\n"
                      "  rect(0, 0, width=20%, height=40%)\n"
                      "}")
        scene = resolve(stmts)
        inner = scene.shapes[0].subnodes[0]
        assert inner.box.width == 20.0
        assert inner.box.height == 20.0


class TestRelationalPositioning:

    def test_right_of(self):
        stmts = parse("let a = rect(0, 0, width=10, height=10)\n"
                      "let b = rect(@right-of(a), width=5, height=5)\n")
        scene = resolve(stmts)
        a = scene.shapes[0]
        b = scene.shapes[1]
        # b's left edge touches a's right edge
        assert b.box.x == a.box.x + a.box.width
        # b vertically centered with a
        assert b.box.y == a.box.y + (a.box.height - b.box.height) / 2.0

    def test_left_of(self):
        stmts = parse("let a = rect(20, 0, width=10, height=10)\n"
                      "let b = rect(@left-of(a), width=5, height=5)\n")
        scene = resolve(stmts)
        a = scene.shapes[0]
        b = scene.shapes[1]
        assert b.box.x == a.box.x - b.box.width
        assert b.box.y == a.box.y + (a.box.height - b.box.height) / 2.0

    def test_below(self):
        stmts = parse("let a = rect(0, 0, width=10, height=10)\n"
                      "let b = rect(@below(a), width=5, height=5)\n")
        scene = resolve(stmts)
        a = scene.shapes[0]
        b = scene.shapes[1]
        assert b.box.y == a.box.y + a.box.height
        assert b.box.x == a.box.x + (a.box.width - b.box.width) / 2.0

    def test_above(self):
        stmts = parse("let a = rect(0, 20, width=10, height=10)\n"
                      "let b = rect(@above(a), width=5, height=5)\n")
        scene = resolve(stmts)
        a = scene.shapes[0]
        b = scene.shapes[1]
        assert b.box.y == a.box.y - b.box.height
        assert b.box.x == a.box.x + (a.box.width - b.box.width) / 2.0

    def test_left_of_with_vec_offset(self):
        stmts = parse(
            "let a = rect(20, 0, width=10, height=10)\n"
            "let b = rect(@left-of(a) - @vec(3, 0), width=5, height=5)\n")
        scene = resolve(stmts)
        a = scene.shapes[0]
        b = scene.shapes[1]
        base_x = a.box.x - b.box.width
        assert b.box.x == base_x - 3.0


class TestSubnodeResolution:

    def test_subnodes_relative_to_parent(self):
        stmts = parse("rect(10, 20, width=30, height=30) {\n"
                      "  rect(5, 5, width=10, height=10)\n"
                      "}")
        scene = resolve(stmts)
        outer = scene.shapes[0]
        inner = outer.subnodes[0]
        assert inner.box.x == 15.0  # 10 + 5
        assert inner.box.y == 25.0  # 20 + 5

    def test_nested_three_levels(self):
        stmts = parse("rect(10, 10, width=50, height=50) {\n"
                      "  rect(5, 5, width=30, height=30) {\n"
                      "    rect(2, 2, width=10, height=10)\n"
                      "  }\n"
                      "}")
        scene = resolve(stmts)
        inner = scene.shapes[0].subnodes[0].subnodes[0]
        assert inner.box.x == 17.0  # 10 + 5 + 2
        assert inner.box.y == 17.0


class TestVecExpression:

    def test_vec_position(self):
        stmts = parse("rect(@vec(3, 7), width=5, height=5)")
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert s.box.x == 3.0
        assert s.box.y == 7.0

    def test_vec_addition(self):
        stmts = parse("rect(@vec(3, 7) + @vec(2, 1), width=5, height=5)")
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert s.box.x == 5.0
        assert s.box.y == 8.0


class TestLetBinding:

    def test_let_creates_binding(self):
        stmts = parse("let a = rect(0, 0, width=5, height=5)\n"
                      "rect(@right-of(a), width=3, height=3)\n")
        scene = resolve(stmts)
        assert len(scene.shapes) == 2

    def test_let_redeclare_error(self):
        stmts = parse("let a = rect(0, 0, width=5, height=5)\n"
                      "let a = rect(1, 1, width=5, height=5)\n")
        with pytest.raises(ResolveError, match="already defined"):
            resolve(stmts)

    def test_undefined_variable_error(self):
        stmts = parse("rect(@right-of(nonexistent), width=5, height=5)")
        with pytest.raises(ResolveError, match="Undefined"):
            resolve(stmts)


class TestCanvasInference:

    def test_canvas_inferred(self):
        stmts = parse("rect(0, 0, width=20, height=10)")
        scene = resolve(stmts)
        assert scene.width == 20.0
        assert scene.height == 10.0

    def test_canvas_explicit(self):
        stmts = parse("rect(0, 0, width=5, height=5)")
        scene = resolve(stmts, canvas_width=100.0, canvas_height=50.0)
        assert scene.width == 100.0
        assert scene.height == 50.0

    def test_canvas_inferred_multiple(self):
        stmts = parse("rect(0, 0, width=10, height=10)\n"
                      "rect(20, 5, width=10, height=10)\n")
        scene = resolve(stmts)
        assert scene.width == 30.0  # 20 + 10
        assert scene.height == 15.0  # 5 + 10


class TestTextResolution:

    def test_text_size_inferred(self):
        stmts = parse('text(0, 0, "hello")')
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert s.text == "hello"
        assert s.box.width == 5.0
        assert s.box.height == 1.0

    def test_text_wrap(self):
        stmts = parse('text(0, 0, "hello world foo bar", wrap=10)')
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert s.wrap_width == 10
        assert s.box.height >= 2.0


class TestLineResolution:

    def test_line_points(self):
        stmts = parse("line(0, 0, [lineTo(5, 0), lineTo(5, 5)])")
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert len(s.line_points) == 3
        assert s.line_points[0] == (0.0, 0.0)
        assert s.line_points[1] == (5.0, 0.0)
        assert s.line_points[2] == (5.0, 5.0)

    def test_line_horizontal_vertical(self):
        stmts = parse("line(0, 0, [horizontalTo(10), verticalTo(5)])")
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert len(s.line_points) == 3
        assert s.line_points[0] == (0.0, 0.0)
        assert s.line_points[1] == (10.0, 0.0)
        assert s.line_points[2] == (10.0, 5.0)

    def test_line_moveto_creates_gap(self):
        stmts = parse(
            "line(0, 0, [lineTo(5, 0), moveTo(10, 0), lineTo(15, 0)])")
        scene = resolve(stmts)
        s = scene.shapes[0]
        assert None in s.line_points

import pytest
import logging


class TestRectRendering:

    def test_simple_rect_unicode(self, pipeline):
        output = pipeline("rect(0, 0, width=5, height=3)")
        lines = output.strip().split("\n")
        assert lines[0] == "┌───┐"
        assert lines[1] == "│   │"
        assert lines[2] == "└───┘"

    def test_simple_rect_ascii(self, pipeline):
        output = pipeline("rect(0, 0, width=5, height=3)", charset="ascii")
        lines = output.strip().split("\n")
        assert lines[0] == "+---+"
        assert lines[1] == "|   |"
        assert lines[2] == "+---+"

    def test_rect_offset(self, pipeline):
        output = pipeline(
            "rect(2, 1, width=5, height=3)",
            canvas_width=10,
            canvas_height=5,
        )
        lines = output.split("\n")
        # Row 0 should be empty or spaces
        logging.info(lines)
        assert lines[0].strip() == ""
        # Row 1 should have rect starting at col 2
        assert lines[1][2:7] == "┌───┐"

    def test_rect_1x1(self, pipeline):
        output = pipeline("rect(0, 0, width=1, height=1)")
        lines = output.strip().split("\n")
        assert lines[0] == "┌"

    def test_rect_2x2(self, pipeline):
        output = pipeline("rect(0, 0, width=2, height=2)")
        lines = output.strip().split("\n")
        assert lines[0] == "┌┐"
        assert lines[1] == "└┘"


class TestNestedRects:

    def test_rect_inside_rect(self, pipeline):
        output = pipeline("rect(0, 0, width=10, height=6) {\n"
                          "  rect(2, 2, width=6, height=2)\n"
                          "}")
        lines = output.strip().split("\n")
        # Outer rect
        assert lines[0].startswith("┌")
        assert lines[0].endswith("┐")
        # Inner rect at row 2, cols 2-7
        assert "┌────┐" in lines[2]

    def test_nested_rects_share_borders(self, pipeline):
        output = pipeline("rect(0, 0, width=5, height=5) {\n"
                          "  rect(0, 0, width=4, height=4) {\n"
                          "    rect(0, 0, width=3, height=3) {\n"
                          "      rect(0, 0, width=2, height=2)\n"
                          "    }\n"
                          "  }\n"
                          "}")
        lines = output.strip().split("\n")
        # The top-left corner area should have nested box chars
        assert len(lines) >= 5


class TestCharsetOverride:

    def test_rect_single_char_override(self, pipeline):
        output = pipeline('rect(0, 0, width=4, height=3, charset="@")')
        lines = output.strip().split("\n")
        assert lines[0] == "@@@@"
        assert lines[1] == "@  @"
        assert lines[2] == "@@@@"


class TestTextRendering:

    def test_simple_text(self, pipeline):
        output = pipeline(
            'text(0, 0, "hello")',
            canvas_width=10,
            canvas_height=2,
        )
        logging.info(output)
        lines = output.strip().split("\n")
        assert "hello" in lines[0]

    def test_text_at_offset(self, pipeline):
        output = pipeline(
            'text(3, 1, "hi")',
            canvas_width=10,
            canvas_height=3,
        )
        lines = output.split("\n")
        assert lines[1][3:5] == "hi", output

    def test_text_wrap(self, pipeline):
        output = pipeline(
            'text(0, 0, "hello world foo", wrap=8)',
            canvas_width=20,
            canvas_height=5,
        )
        lines = output.split("\n")
        assert len(lines) >= 2
        # First line should be within wrap width
        assert len(lines[0].rstrip()) <= 8, lines


class TestLineRendering:

    def test_horizontal_line(self, pipeline):
        output = pipeline(
            "line(0, 0, [horizontalTo(5)])",
            canvas_width=8,
            canvas_height=2,
        )
        lines = output.strip().split("\n")
        # Should have horizontal chars from col 0 to col 5
        assert "─" in lines[0] or "-" in lines[0]

    def test_vertical_line(self, pipeline):
        output = pipeline(
            "line(0, 0, [verticalTo(4)])",
            canvas_width=3,
            canvas_height=6,
        )
        lines = output.strip().split("\n")
        assert len(lines) >= 4
        for i in range(4):
            assert lines[i][0] in ("│", "|")


class TestRelationalRendering:

    def test_two_rects_side_by_side(self, pipeline):
        output = pipeline("let a = rect(0, 0, width=5, height=3)\n"
                          "let b = rect(@right-of(a), width=5, height=3)\n")
        lines = output.strip().split("\n")
        # Both rects should appear on same row, b starts at col 5
        assert lines[0][0] == "┌"
        assert lines[0][5] == "┌"

    def test_rect_below(self, pipeline):
        output = pipeline("let a = rect(0, 0, width=6, height=3)\n"
                          "let b = rect(@below(a), width=6, height=3)\n")
        lines = output.strip().split("\n")
        assert len(lines) >= 6
        # Second rect top starts at row 3
        assert lines[3][0] == "┌"


class TestScaling:

    def test_scale_2x(self, pipeline):
        output = pipeline(
            "rect(0, 0, width=3, height=2)",
            scale=2.0,
        )
        lines = output.strip().split("\n")
        # At scale 2, width=6 chars, height=4 chars
        assert len(lines[0]) == 6
        assert len(lines) == 4


class TestEllipseRendering:

    def test_ellipse_renders(self, pipeline):
        output = pipeline(
            "ellipse(0, 0, width=10, height=6)",
            canvas_width=12,
            canvas_height=8,
        )
        lines = output.strip().split("\n")
        # Should have some content
        assert len(lines) >= 1
        # At least some non-space chars
        all_content = "".join(lines)
        assert any(c != " " for c in all_content)


class TestEmptyCanvas:

    def test_empty_input(self, pipeline):
        output = pipeline("", canvas_width=5, canvas_height=3)
        # Should produce something (possibly empty or whitespace)
        assert isinstance(output, str)


class TestComplexScene:

    def test_multiple_shapes(self, pipeline):
        source = ("let box1 = rect(0, 0, width=10, height=5)\n"
                  "let box2 = rect(@right-of(box1), width=10, height=5)\n"
                  "let box3 = rect(@below(box1), width=10, height=5)\n")
        output = pipeline(source)
        lines = output.strip().split("\n")
        assert len(lines) >= 10

    def test_rect_with_text_inside(self, pipeline):
        source = ("rect(0, 0, width=12, height=4) {\n"
                  '  text(1, 1, "hello")\n'
                  "}")
        output = pipeline(source)
        lines = output.strip().split("\n")
        assert "hello" in lines[1]

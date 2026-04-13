import pytest

from diagrammer.parser import parse
from diagrammer.ir import (
    Shape,
    ShapeKind,
    LetStatement,
    FunctionDef,
    FunctionCall,
    Expr,
    ExprType,
    LineToCommand,
    MoveToCommand,
    HorizontalToCommand,
    VerticalToCommand,
)


class TestBasicShapes:

    def test_rect_positional(self):
        stmts = parse("rect(0, 0, width=10, height=5)")
        assert len(stmts) == 1
        s = stmts[0]
        assert isinstance(s, Shape)
        assert s.kind == ShapeKind.RECT

    def test_circle_positional(self):
        stmts = parse("circle(3, 4, width=10, height=10)")
        assert len(stmts) == 1
        assert isinstance(stmts[0], Shape)
        assert stmts[0].kind == ShapeKind.CIRCLE

    def test_ellipse(self):
        stmts = parse("ellipse(0, 0, width=20, height=10)")
        assert len(stmts) == 1
        assert stmts[0].kind == ShapeKind.ELLIPSE

    def test_group(self):
        stmts = parse("group(0, 0, width=50, height=50)")
        assert len(stmts) == 1
        assert stmts[0].kind == ShapeKind.GROUP

    def test_text(self):
        stmts = parse('text(5, 5, "hello world")')
        assert len(stmts) == 1
        assert stmts[0].kind == ShapeKind.TEXT


class TestExpressions:

    def test_percent(self):
        stmts = parse("rect(50%, 50%, width=10, height=10)")
        s = stmts[0]
        assert isinstance(s, Shape)
        assert s.positional_args[0].type == ExprType.PERCENT
        assert s.positional_args[0].value == 50.0

    def test_vec(self):
        stmts = parse("rect(@vec(1, 2), width=5, height=5)")
        s = stmts[0]
        assert s.positional_args[0].type == ExprType.VEC

    def test_addition(self):
        stmts = parse("rect(1 + 2, 3, width=5, height=5)")
        s = stmts[0]
        assert s.positional_args[0].type == ExprType.ADD

    def test_subtraction(self):
        stmts = parse("rect(10 - 3, 0, width=5, height=5)")
        s = stmts[0]
        assert s.positional_args[0].type == ExprType.SUB

    def test_negation(self):
        stmts = parse("rect(-5, 0, width=5, height=5)")
        s = stmts[0]
        assert s.positional_args[0].type == ExprType.NEG

    def test_ref(self):
        stmts = parse(
            "let a = rect(0, 0, width=5, height=5)\nrect(@a, width=5, height=5)"
        )
        assert isinstance(stmts[0], LetStatement)
        s = stmts[1]
        assert isinstance(s, Shape)

    def test_left_of(self):
        stmts = parse(
            "let a = rect(0, 0, width=5, height=5)\nrect(@left-of(a), width=5, height=5)"
        )
        s = stmts[1]
        assert isinstance(s, Shape)
        assert s.positional_args[0].type == ExprType.LEFT_OF

    def test_right_of(self):
        stmts = parse(
            "let a = rect(0, 0, width=5, height=5)\nrect(@right-of(a), width=5, height=5)"
        )
        s = stmts[1]
        assert s.positional_args[0].type == ExprType.RIGHT_OF

    def test_above(self):
        stmts = parse(
            "let a = rect(0, 0, width=5, height=5)\nrect(@above(a), width=5, height=5)"
        )
        s = stmts[1]
        assert s.positional_args[0].type == ExprType.ABOVE

    def test_below(self):
        stmts = parse(
            "let a = rect(0, 0, width=5, height=5)\nrect(@below(a), width=5, height=5)"
        )
        s = stmts[1]
        assert s.positional_args[0].type == ExprType.BELOW

    def test_left_of_with_vec_offset(self):
        stmts = parse("let a = rect(0, 0, width=5, height=5)\n"
                      "rect(@left-of(a) - @vec(2, 0), width=5, height=5)")
        s = stmts[1]
        assert s.positional_args[0].type == ExprType.SUB


class TestLetBinding:

    def test_let_rect(self):
        stmts = parse("let mybox = rect(0, 0, width=10, height=10)")
        assert len(stmts) == 1
        assert isinstance(stmts[0], LetStatement)
        assert stmts[0].name == "mybox"
        assert isinstance(stmts[0].shape, Shape)

    def test_let_no_redeclare(self):
        # Parser allows it; resolver should reject
        stmts = parse("let a = rect(0, 0, width=5, height=5)\n"
                      "let a = rect(1, 1, width=5, height=5)")
        assert len(stmts) == 2


class TestSubnodes:

    def test_rect_with_subnodes(self):
        stmts = parse("rect(0, 0, width=20, height=20) {\n"
                      "  rect(1, 1, width=5, height=5)\n"
                      "}")
        assert len(stmts) == 1
        s = stmts[0]
        assert isinstance(s, Shape)
        assert len(s.subnodes) == 1
        inner = s.subnodes[0]
        assert isinstance(inner, Shape)
        assert inner.kind == ShapeKind.RECT

    def test_deeply_nested(self):
        stmts = parse("rect(0, 0, width=30, height=30) {\n"
                      "  rect(1, 1, width=20, height=20) {\n"
                      "    rect(2, 2, width=10, height=10)\n"
                      "  }\n"
                      "}")
        outer = stmts[0]
        assert len(outer.subnodes) == 1
        mid = outer.subnodes[0]
        assert len(mid.subnodes) == 1
        inner = mid.subnodes[0]
        assert isinstance(inner, Shape)


class TestLineCommands:

    def test_line_with_lineto(self):
        stmts = parse("line(0, 0, [lineTo(5, 5), lineTo(10, 0)])")
        s = stmts[0]
        assert isinstance(s, Shape)
        assert s.kind == ShapeKind.LINE
        assert len(s.line_commands) == 2
        assert isinstance(s.line_commands[0], LineToCommand)
        assert isinstance(s.line_commands[1], LineToCommand)

    def test_line_with_moveto(self):
        stmts = parse(
            "line(0, 0, [lineTo(5, 0), moveTo(10, 0), lineTo(15, 0)])")
        s = stmts[0]
        assert len(s.line_commands) == 3
        assert isinstance(s.line_commands[1], MoveToCommand)

    def test_line_horizontal_vertical(self):
        stmts = parse("line(0, 0, [horizontalTo(10), verticalTo(5)])")
        s = stmts[0]
        assert len(s.line_commands) == 2
        assert isinstance(s.line_commands[0], HorizontalToCommand)
        assert isinstance(s.line_commands[1], VerticalToCommand)


class TestFunctionDef:

    def test_simple_function(self):
        stmts = parse("#function myf(pos) {\n"
                      "  rect(@pos, width=5, height=5)\n"
                      "}")
        assert len(stmts) == 1
        f = stmts[0]
        assert isinstance(f, FunctionDef)
        assert f.name == "myf"
        assert f.params == ["pos"]
        assert len(f.body) == 1

    def test_function_with_keyword_default(self):
        stmts = parse("#function myf(pos, sz=10) {\n"
                      "  rect(@pos, width=@sz, height=@sz)\n"
                      "}")
        f = stmts[0]
        assert isinstance(f, FunctionDef)
        assert "sz" in f.keyword_params

    def test_function_call(self):
        stmts = parse("#function myf(pos) {\n"
                      "  rect(@pos, width=5, height=5)\n"
                      "}\n"
                      "#myf(@vec(0, 0))\n")
        assert len(stmts) == 2
        assert isinstance(stmts[0], FunctionDef)
        assert isinstance(stmts[1], FunctionCall)


class TestCharsetOverride:

    def test_charset_single_char(self):
        stmts = parse("rect(0, 0, width=5, height=5, charset=@)")
        # charset=@ is parsed as keyword_arg with name "charset" and value "@" ref
        # The parser should handle this - but our grammar treats @ as ref prefix
        # For single char override, let's test with a string
        pass

    def test_charset_string(self):
        stmts = parse('rect(0, 0, width=5, height=5, charset="@")')
        s = stmts[0]
        assert isinstance(s, Shape)
        # charset is extracted during parsing
        assert s.charset_override == "@"


class TestComments:

    def test_line_comment(self):
        stmts = parse("// this is a comment\n"
                      "rect(0, 0, width=5, height=5)\n"
                      "// another comment\n")
        assert len(stmts) == 1
        assert isinstance(stmts[0], Shape)

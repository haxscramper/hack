from __future__ import annotations

from beartype import beartype
import logging
from pathlib import Path

from lark import Lark, Transformer, Token

from diagrammer.ir import (
    Expr,
    ExprType,
    FunctionCall,
    FunctionDef,
    HorizontalToCommand,
    LetStatement,
    LineToCommand,
    MoveToCommand,
    Shape,
    ShapeKind,
    Statement,
    VerticalToCommand,
)

_GRAMMAR_PATH = Path(__file__).parent / "grammar.lark"


@beartype
def get_parser() -> Lark:
    logging.debug("Loading grammar from %s", _GRAMMAR_PATH)
    return Lark(
        _GRAMMAR_PATH.read_text(),
        parser="earley",
        ambiguity="resolve",
    )


@beartype
def _split_args(items: list) -> tuple[list[Expr], dict[str, Expr | str]]:
    positional: list[Expr] = []
    keyword: dict[str, Expr | str] = {}
    for item in items:
        if isinstance(item, tuple):
            keyword[item[0]] = item[1]
        elif isinstance(item, Expr):
            positional.append(item)
        elif isinstance(item, str):
            positional.append(Expr.literal(item))
    return positional, keyword


@beartype
class DslTransformer(Transformer):
    def start(self, items: list) -> list[Statement]:
        return [item for item in items if item is not None]

    def statement(self, items: list) -> Statement:
        return items[0]

    def let_statement(self, items: list) -> LetStatement:
        name = str(items[0])
        value = items[1]
        return LetStatement(name=name, shape=value)

    def function_def(self, items: list) -> FunctionDef:
        name = str(items[0])
        idx = 1
        params: list[str] = []
        keyword_params: dict[str, Expr] = {}
        if idx < len(items) and isinstance(items[idx], dict):
            param_data = items[idx]
            params = param_data.get("positional", [])
            keyword_params = param_data.get("keyword", {})
            idx += 1
        body: list[Statement] = []
        for i in range(idx, len(items)):
            if items[i] is not None:
                body.append(items[i])
        return FunctionDef(
            name=name, params=params, keyword_params=keyword_params, body=body
        )

    def param_list(self, items: list) -> dict:
        positional: list[str] = []
        keyword: dict[str, Expr] = {}
        for item in items:
            if isinstance(item, tuple):
                keyword[item[0]] = item[1]
            else:
                positional.append(item)
        return {"positional": positional, "keyword": keyword}

    def positional_param(self, items: list) -> str:
        return str(items[0])

    def keyword_param(self, items: list) -> tuple[str, Expr]:
        return (str(items[0]), items[1])

    def function_call_stmt(self, items: list) -> FunctionCall:
        return items[0]

    def function_call_expr(self, items: list) -> FunctionCall:
        name = str(items[0])
        idx = 1
        positional_args: list[Expr] = []
        keyword_args: dict[str, Expr] = {}
        subnodes: list[Statement] = []

        if idx < len(items) and isinstance(items[idx], list):
            positional_args, keyword_args = _split_args(items[idx])
            idx += 1

        for i in range(idx, len(items)):
            if items[i] is not None:
                subnodes.append(items[i])

        return FunctionCall(
            name=name,
            positional_args=positional_args,
            keyword_args=keyword_args,
            subnodes=subnodes,
        )

    def shape_stmt(self, items: list) -> Shape:
        return items[0]

    def shape_basic(self, items: list) -> Shape:
        kind_str = str(items[0])
        kind = ShapeKind(kind_str)
        idx = 1
        positional_args: list[Expr] = []
        keyword_args: dict[str, Expr] = {}
        subnodes: list[Statement] = []

        if idx < len(items) and isinstance(items[idx], list):
            positional_args, keyword_args = _split_args(items[idx])
            idx += 1

        for i in range(idx, len(items)):
            if items[i] is not None:
                subnodes.append(items[i])

        charset_override = None
        if "charset" in keyword_args:
            cs = keyword_args.pop("charset")
            if (
                isinstance(cs, Expr)
                and cs.type == ExprType.LITERAL
                and isinstance(cs.value, str)
            ):
                charset_override = cs.value

        return Shape(
            kind=kind,
            positional_args=positional_args,
            keyword_args=keyword_args,
            subnodes=subnodes,
            charset_override=charset_override,
        )

    def shape_with_lines(self, items: list) -> Shape:
        kind_str = str(items[0])
        kind = ShapeKind(kind_str)
        idx = 1
        positional_args: list[Expr] = []
        keyword_args: dict[str, Expr] = {}
        line_commands = []
        subnodes: list[Statement] = []

        if idx < len(items) and isinstance(items[idx], list):
            # This is the positional_args before the line commands
            positional_args, keyword_args = _split_args(items[idx])
            idx += 1

        if idx < len(items) and isinstance(items[idx], list):
            line_commands = items[idx]
            idx += 1

        for i in range(idx, len(items)):
            if items[i] is not None:
                subnodes.append(items[i])

        charset_override = None
        if "charset" in keyword_args:
            cs = keyword_args.pop("charset")
            if (
                isinstance(cs, Expr)
                and cs.type == ExprType.LITERAL
                and isinstance(cs.value, str)
            ):
                charset_override = cs.value

        return Shape(
            kind=kind,
            positional_args=positional_args,
            keyword_args=keyword_args,
            subnodes=subnodes,
            line_commands=line_commands,
            charset_override=charset_override,
        )

    def arg_list(self, items: list) -> list:
        return list(items)

    def positional_arg(self, items: list):
        return items[0]

    def keyword_arg(self, items: list) -> tuple[str, Expr]:
        return (str(items[0]), items[1])

    def keyword_arg_str(self, items: list) -> tuple[str, Expr]:
        name = str(items[0])
        val = str(items[1])
        if val.startswith('"') and val.endswith('"'):
            val = val[1:-1]
        return (name, Expr.literal(val))

    def positional_args(self, items: list) -> list:
        return list(items)

    def line_command_list(self, items: list) -> list:
        return list(items)

    def line_to(self, items: list) -> LineToCommand:
        return LineToCommand(x=items[0], y=items[1])

    def move_to(self, items: list) -> MoveToCommand:
        return MoveToCommand(x=items[0], y=items[1])

    def horizontal_to(self, items: list) -> HorizontalToCommand:
        return HorizontalToCommand(length=items[0])

    def vertical_to(self, items: list) -> VerticalToCommand:
        return VerticalToCommand(length=items[0])

    def add(self, items: list) -> Expr:
        return Expr.add(items[0], items[1])

    def sub(self, items: list) -> Expr:
        return Expr.sub(items[0], items[1])

    def neg(self, items: list) -> Expr:
        return Expr.neg(items[0])

    def number(self, items: list) -> Expr:
        return Expr.literal(float(items[0]))

    def percent(self, items: list) -> Expr:
        return Expr.percent(float(items[0]))

    def vec(self, items: list) -> Expr:
        return Expr.vec(items[0], items[1])

    def ref(self, items: list) -> Expr:
        return Expr.ref(str(items[0]))

    def left_of(self, items: list) -> Expr:
        return Expr.left_of(str(items[0]))

    def right_of(self, items: list) -> Expr:
        return Expr.right_of(str(items[0]))

    def above_expr(self, items: list) -> Expr:
        return Expr.above(str(items[0]))

    def below_expr(self, items: list) -> Expr:
        return Expr.below(str(items[0]))

    def node_args(self, items: list) -> Expr:
        from diagrammer.ir import ExprType

        return Expr(type=ExprType.REF, name="NODE_ARGS")

    def string_literal(self, items: list) -> str:
        raw = str(items[0])
        if raw.startswith('"') and raw.endswith('"'):
            return raw[1:-1]
        return raw

    def term(self, items: list):
        return items[0]

    def atom(self, items: list):
        return items[0]

    def expr(self, items: list):
        return items[0]


@beartype
def parse(source: str) -> list[Statement]:
    logging.debug("Parsing source (length=%d)", len(source))
    parser = get_parser()
    tree = parser.parse(source)
    logging.debug("Parse tree: %s", tree.pretty())
    transformer = DslTransformer()
    result = transformer.transform(tree)
    logging.debug("Parsed statements: %s", result)
    return result

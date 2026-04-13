from __future__ import annotations
from beartype import beartype
import logging

from diagrammer.ir import (
    Expr,
    ExprType,
    FunctionCall,
    FunctionDef,
    HorizontalToCommand,
    LetStatement,
    LineToCommand,
    MoveToCommand,
    Rect,
    ResolvedBox,
    ResolvedShape,
    Scene,
    Shape,
    ShapeKind,
    Statement,
    Vec,
    VerticalToCommand,
)


@beartype
class ResolveError(Exception):
    pass


@beartype
class Scope:
    def __init__(self, parent: Scope | None = None):
        self.parent = parent
        self.variables: dict[str, Expr] = {}
        self.functions: dict[str, FunctionDef] = {}
        self.node_args: list[Statement] | None = None

    def lookup_var(self, name: str) -> Expr:
        if name in self.variables:
            return self.variables[name]
        if self.parent is not None:
            return self.parent.lookup_var(name)
        raise ResolveError(
            f"Undefined variable: {name}, known variables {self.variables}"
        )

    def lookup_func(self, name: str) -> FunctionDef:
        if name in self.functions:
            return self.functions[name]
        if self.parent is not None:
            return self.parent.lookup_func(name)
        raise ResolveError(f"Undefined function: {name}")

    def define_var(self, name: str, expr: Expr) -> None:
        if name in self.variables:
            raise ResolveError(f"Variable already defined: {name}")
        self.variables[name] = expr

    def define_func(self, name: str, func: FunctionDef) -> None:
        self.functions[name] = func


@beartype
def _resolve_expr_scalar(
    expr: Expr,
    scope: Scope,
    parent_size: float | None,
    axis: str,
) -> float:
    logging.debug("Resolving scalar expr type=%s axis=%s", expr.type, axis)
    match expr.type:
        case ExprType.LITERAL:
            if expr.value is None:
                raise ResolveError("Literal expression has no value")
            if isinstance(expr.value, (int, float, str)):
                return float(expr.value)
            raise ResolveError(
                f"Cannot convert {type(expr.value).__name__} to scalar for axis {axis}"
            )
        case ExprType.PERCENT:
            if parent_size is None:
                raise ResolveError("Cannot use % without a parent with known size")
            if expr.value is None:
                raise ResolveError("Percent expression has no value")
            if isinstance(expr.value, (int, float, str)):
                return (float(expr.value) / 100.0) * parent_size
            raise ResolveError(
                f"Cannot use {type(expr.value).__name__} as percent value"
            )
        case ExprType.ADD:
            if expr.left is None or expr.right is None:
                raise ResolveError("ADD expression missing operands")
            l = _resolve_expr_scalar(expr.left, scope, parent_size, axis)
            r = _resolve_expr_scalar(expr.right, scope, parent_size, axis)
            return l + r
        case ExprType.SUB:
            if expr.left is None or expr.right is None:
                raise ResolveError("SUB expression missing operands")
            l = _resolve_expr_scalar(expr.left, scope, parent_size, axis)
            r = _resolve_expr_scalar(expr.right, scope, parent_size, axis)
            return l - r
        case ExprType.NEG:
            if expr.left is None:
                raise ResolveError("NEG expression missing operand")
            v = _resolve_expr_scalar(expr.left, scope, parent_size, axis)
            return -v
        case ExprType.REF:
            if expr.name is None:
                raise ResolveError("REF expression missing name")
            var_expr = scope.lookup_var(expr.name)
            # If the variable contains a scalar value, return it
            if var_expr.type == ExprType.LITERAL and var_expr.value is not None:
                if isinstance(var_expr.value, (int, float, str)):
                    return float(var_expr.value)
                # If the variable contains a Rect, extract the appropriate field
                if isinstance(var_expr.value, Rect):
                    rect = var_expr.value
                    return rect.x if axis == "x" else rect.y
            # If the variable contains a Vec, extract the appropriate component
            if var_expr.type == ExprType.VEC and var_expr.value is not None:
                vec = var_expr.value
                if isinstance(vec, Vec):
                    return vec.x if axis == "x" else vec.y
            # Otherwise, recursively resolve the expression
            return _resolve_expr_scalar(var_expr, scope, parent_size, axis)
        case _:
            raise ResolveError(
                f"Cannot resolve expression type {expr.type} as scalar for axis {axis}"
            )


@beartype
def _resolve_expr_xy(
    expr: Expr,
    scope: Scope,
    parent_width: float | None,
    parent_height: float | None,
) -> tuple[float, float]:
    logging.debug("Resolving XY expr type=%s", expr.type)
    match expr.type:
        case ExprType.VEC:
            if expr.left is None or expr.right is None:
                raise ResolveError("VEC expression missing operands")
            x = _resolve_expr_scalar(expr.left, scope, parent_width, "x")
            y = _resolve_expr_scalar(expr.right, scope, parent_height, "y")
            return (x, y)
        case ExprType.ADD:
            if expr.left is None or expr.right is None:
                raise ResolveError("ADD expression missing operands")
            lx, ly = _resolve_expr_xy(expr.left, scope, parent_width, parent_height)
            rx, ry = _resolve_expr_xy(expr.right, scope, parent_width, parent_height)
            return (lx + rx, ly + ry)
        case ExprType.SUB:
            if expr.left is None or expr.right is None:
                raise ResolveError("SUB expression missing operands")
            lx, ly = _resolve_expr_xy(expr.left, scope, parent_width, parent_height)
            rx, ry = _resolve_expr_xy(expr.right, scope, parent_width, parent_height)
            return (lx - rx, ly - ry)
        case ExprType.NEG:
            if expr.left is None:
                raise ResolveError("NEG expression missing operand")
            vx, vy = _resolve_expr_xy(expr.left, scope, parent_width, parent_height)
            return (-vx, -vy)
        case _:
            v = _resolve_expr_scalar(expr, scope, parent_width, "x")
            return (v, v)


@beartype
def _resolve_var_to_box(name: str, scope: Scope) -> ResolvedBox:
    """Resolve a variable to its bounding box."""
    var_expr = scope.lookup_var(name)

    # If the variable contains a Rect value, use it directly
    if var_expr.type == ExprType.LITERAL and isinstance(var_expr.value, Rect):
        rect = var_expr.value
        return ResolvedBox(x=rect.x, y=rect.y, width=rect.width, height=rect.height)

    # If the variable contains a Vec value, treat it as a point with zero size
    if var_expr.type == ExprType.LITERAL and isinstance(var_expr.value, Vec):
        vec = var_expr.value
        return ResolvedBox(x=vec.x, y=vec.y, width=0, height=0)

    # If the variable is a VEC expression, resolve it to a point
    if var_expr.type == ExprType.VEC:
        vx, vy = _resolve_expr_xy(var_expr, scope, None, None)
        return ResolvedBox(x=vx, y=vy, width=0, height=0)

    # If the variable is a LITERAL scalar, treat it as x coordinate with y=0
    if var_expr.type == ExprType.LITERAL and var_expr.value is not None:
        if isinstance(var_expr.value, (int, float, str)):
            x = float(var_expr.value)
            return ResolvedBox(x=x, y=0, width=0, height=0)

    # For REF expressions, look up the referenced variable
    if var_expr.type == ExprType.REF and var_expr.name is not None:
        return _resolve_var_to_box(var_expr.name, scope)

    # Default fallback
    raise ResolveError(
        f"Cannot resolve variable '{name}' to a bounding box: {var_expr}"
    )


@beartype
def _resolve_position_expr(
    expr: Expr,
    scope: Scope,
    parent_width: float | None,
    parent_height: float | None,
    own_width: float,
    own_height: float,
    axis: str,
) -> float:
    match expr.type:
        case ExprType.LEFT_OF:
            if expr.name is None:
                raise ResolveError("LEFT_OF expression missing name")
            target_box = _resolve_var_to_box(expr.name, scope)
            x = target_box.x - own_width
            y = target_box.y + (target_box.height - own_height) / 2.0
            return x if axis == "x" else y
        case ExprType.RIGHT_OF:
            if expr.name is None:
                raise ResolveError("RIGHT_OF expression missing name")
            target_box = _resolve_var_to_box(expr.name, scope)
            x = target_box.x + target_box.width
            y = target_box.y + (target_box.height - own_height) / 2.0
            return x if axis == "x" else y
        case ExprType.ABOVE:
            if expr.name is None:
                raise ResolveError("ABOVE expression missing name")
            target_box = _resolve_var_to_box(expr.name, scope)
            x = target_box.x + (target_box.width - own_width) / 2.0
            y = target_box.y - own_height
            return x if axis == "x" else y
        case ExprType.BELOW:
            if expr.name is None:
                raise ResolveError("BELOW expression missing name")
            target_box = _resolve_var_to_box(expr.name, scope)
            x = target_box.x + (target_box.width - own_width) / 2.0
            y = target_box.y + target_box.height
            return x if axis == "x" else y
        case ExprType.ADD:
            if expr.left is None or expr.right is None:
                raise ResolveError("ADD expression missing operands")
            if _is_relational(expr.left):
                base = _resolve_position_expr(
                    expr.left,
                    scope,
                    parent_width,
                    parent_height,
                    own_width,
                    own_height,
                    axis,
                )
                if expr.right.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.right, scope, parent_width, parent_height
                    )
                    return base + (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.right,
                        scope,
                        parent_width if axis == "x" else parent_height,
                        axis,
                    )
                    return base + offset
            elif _is_relational(expr.right):
                base = _resolve_position_expr(
                    expr.right,
                    scope,
                    parent_width,
                    parent_height,
                    own_width,
                    own_height,
                    axis,
                )
                if expr.left.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.left, scope, parent_width, parent_height
                    )
                    return base + (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.left,
                        scope,
                        parent_width if axis == "x" else parent_height,
                        axis,
                    )
                    return base + offset
            else:
                return _resolve_expr_scalar(
                    expr, scope, parent_width if axis == "x" else parent_height, axis
                )
        case ExprType.SUB:
            if expr.left is None or expr.right is None:
                raise ResolveError("SUB expression missing operands")
            if _is_relational(expr.left):
                base = _resolve_position_expr(
                    expr.left,
                    scope,
                    parent_width,
                    parent_height,
                    own_width,
                    own_height,
                    axis,
                )
                if expr.right.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.right, scope, parent_width, parent_height
                    )
                    return base - (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.right,
                        scope,
                        parent_width if axis == "x" else parent_height,
                        axis,
                    )
                    return base - offset
            elif _is_relational(expr.right):
                base = _resolve_position_expr(
                    expr.right,
                    scope,
                    parent_width,
                    parent_height,
                    own_width,
                    own_height,
                    axis,
                )
                if expr.left.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.left, scope, parent_width, parent_height
                    )
                    return base + (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.left,
                        scope,
                        parent_width if axis == "x" else parent_height,
                        axis,
                    )
                    return base + offset
            else:
                return _resolve_expr_scalar(
                    expr, scope, parent_width if axis == "x" else parent_height, axis
                )
        case ExprType.SUB:
            if _is_relational(expr.left):
                base = _resolve_position_expr(
                    expr.left,
                    scope,
                    parent_width,
                    parent_height,
                    own_width,
                    own_height,
                    axis,
                )
                if expr.right.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.right, scope, parent_width, parent_height
                    )
                    return base - (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.right,
                        scope,
                        parent_width if axis == "x" else parent_height,
                        axis,
                    )
                    return base - offset
            else:
                return _resolve_expr_scalar(
                    expr, scope, parent_width if axis == "x" else parent_height, axis
                )
        case _:
            return _resolve_expr_scalar(
                expr, scope, parent_width if axis == "x" else parent_height, axis
            )


@beartype
def _is_relational(expr: Expr) -> bool:
    return expr.type in (
        ExprType.LEFT_OF,
        ExprType.RIGHT_OF,
        ExprType.ABOVE,
        ExprType.BELOW,
    )


@beartype
def _contains_relational(expr: Expr) -> bool:
    if _is_relational(expr):
        return True
    if expr.left and _contains_relational(expr.left):
        return True
    if expr.right and _contains_relational(expr.right):
        return True
    return False


@beartype
def _contains_vec(expr: Expr) -> bool:
    if expr.type == ExprType.VEC:
        return True
    if expr.left and _contains_vec(expr.left):
        return True
    if expr.right and _contains_vec(expr.right):
        return True
    return False


@beartype
def _get_kwarg_float(
    kwargs: dict[str, Expr | str],
    key: str,
    scope: Scope,
    parent_size: float | None,
    axis: str,
) -> float | None:
    if key not in kwargs:
        return None
    val = kwargs[key]
    if isinstance(val, str):
        return float(val)
    return _resolve_expr_scalar(val, scope, parent_size, axis)


@beartype
def _get_kwarg_str(kwargs: dict[str, Expr | str], key: str) -> str | None:
    if key not in kwargs:
        return None
    val = kwargs[key]
    if isinstance(val, str):
        return val
    return None


@beartype
def _get_kwarg_int(kwargs: dict[str, Expr | str], key: str) -> int | None:
    if key not in kwargs:
        return None
    val = kwargs[key]
    if isinstance(val, str):
        return int(val)
    if isinstance(val, Expr) and val.type == ExprType.LITERAL:
        if val.value is not None and isinstance(val.value, (int, float, str)):
            try:
                return int(val.value)
            except (ValueError, TypeError):
                return None
    return None


@beartype
def _resolve_shape(
    shape: Shape,
    scope: Scope,
    parent_x: float,
    parent_y: float,
    parent_width: float | None,
    parent_height: float | None,
) -> ResolvedShape:
    kwargs = dict(shape.keyword_args)

    width = _get_kwarg_float(kwargs, "width", scope, parent_width, "x")
    height = _get_kwarg_float(kwargs, "height", scope, parent_height, "y")

    if shape.kind in [ShapeKind.RECT]:
        assert width is not None
        assert height is not None

    if shape.kind == ShapeKind.CIRCLE:
        if width is not None and height is None:
            height = width
        elif height is not None and width is None:
            width = height

    if width is None:
        width = 1.0
    if height is None:
        height = 1.0

    x: float = 0.0
    y: float = 0.0

    pos_args = shape.positional_args

    if shape.kind == ShapeKind.TEXT:
        text_content = _get_kwarg_str(kwargs, "content")
        if text_content is None and len(pos_args) >= 3 and isinstance(pos_args[2], str):
            text_content = pos_args[2]
        elif text_content is None:
            for i, arg in enumerate(pos_args):
                if isinstance(arg, str):
                    text_content = arg
                    break

    if len(pos_args) >= 2:
        x_expr = pos_args[0]
        y_expr = pos_args[1]
        if isinstance(x_expr, Expr):
            x = _resolve_position_expr(
                x_expr, scope, parent_width, parent_height, width, height, "x"
            )
        if isinstance(y_expr, Expr):
            y = _resolve_position_expr(
                y_expr, scope, parent_width, parent_height, width, height, "y"
            )
    elif len(pos_args) == 1:
        expr = pos_args[0]
        if isinstance(expr, Expr) and _contains_relational(expr):
            x = _resolve_position_expr(
                expr, scope, parent_width, parent_height, width, height, "x"
            )
            y = _resolve_position_expr(
                expr, scope, parent_width, parent_height, width, height, "y"
            )
        elif isinstance(expr, Expr) and expr.type == ExprType.VEC:
            vx, vy = _resolve_expr_xy(expr, scope, parent_width, parent_height)
            x = vx
            y = vy
        elif isinstance(expr, Expr) and _contains_vec(expr):
            # Handle expressions like (3,7) + (2,1) as a single position argument
            vx, vy = _resolve_expr_xy(expr, scope, parent_width, parent_height)
            x = vx
            y = vy

    abs_x = parent_x + x
    abs_y = parent_y + y

    # Resolve line commands
    line_points: list[tuple[float, float]] = []
    if shape.line_commands:
        cur_x = abs_x
        cur_y = abs_y
        line_points.append((cur_x, cur_y))
        for cmd in shape.line_commands:
            if isinstance(cmd, LineToCommand):
                dx = _resolve_expr_scalar(cmd.x, scope, parent_width, "x")
                dy = _resolve_expr_scalar(cmd.y, scope, parent_height, "y")
                cur_x = abs_x + dx
                cur_y = abs_y + dy
                line_points.append((cur_x, cur_y))
            elif isinstance(cmd, MoveToCommand):
                dx = _resolve_expr_scalar(cmd.x, scope, parent_width, "x")
                dy = _resolve_expr_scalar(cmd.y, scope, parent_height, "y")
                cur_x = abs_x + dx
                cur_y = abs_y + dy
                line_points.append(None)  # type: ignore[arg-type]
                line_points.append((cur_x, cur_y))
            elif isinstance(cmd, HorizontalToCommand):
                length = _resolve_expr_scalar(cmd.length, scope, parent_width, "x")
                cur_x += length
                line_points.append((cur_x, cur_y))
            elif isinstance(cmd, VerticalToCommand):
                length = _resolve_expr_scalar(cmd.length, scope, parent_height, "y")
                cur_y += length
                line_points.append((cur_x, cur_y))

    # Resolve subnodes
    resolved_subnodes: list[ResolvedShape] = []
    sub_scope = Scope(parent=scope)
    for stmt in shape.subnodes:
        resolved = _resolve_statement(stmt, sub_scope, abs_x, abs_y, width, height)
        if resolved is not None:
            resolved_subnodes.extend(resolved)

    text_content_final: str | None = None
    wrap_width: int | None = None
    if shape.kind == ShapeKind.TEXT:
        # Try to get text from positional args
        text_expr = pos_args[2]
        if isinstance(text_expr, Expr):
            if text_expr.type == ExprType.LITERAL and isinstance(text_expr.value, str):
                text_content_final = text_expr.value
            elif text_expr.type == ExprType.REF and text_expr.name is not None:
                # Look up the variable and extract its string value
                var_expr = scope.lookup_var(text_expr.name)
                if var_expr.type == ExprType.LITERAL and isinstance(
                    var_expr.value, str
                ):
                    text_content_final = var_expr.value
                else:
                    raise ResolveError(
                        f"Variable '{text_expr.name}' does not contain a string value"
                    )
            else:
                raise ResolveError(
                    f"Second argument for the `text()` command is expected to be a string or string variable reference, but got {text_expr}"
                )
        else:
            raise ResolveError(
                f"Second argument for the `text()` command is expected to be an expression, but got {text_expr}"
            )
        wrap_width = _get_kwarg_int(kwargs, "wrap")
        if wrap_width:
            logging.debug("Wrap width specified")
            lines = _wrap_text(text_content_final, wrap_width)
            width = float(max(len(l) for l in lines)) if lines else 0.0
            height = float(len(lines))
        else:
            width = float(len(text_content_final))
            height = 1.0

    return ResolvedShape(
        kind=shape.kind,
        box=ResolvedBox(x=abs_x, y=abs_y, width=width, height=height),
        charset_override=shape.charset_override,
        text=text_content_final,
        wrap_width=wrap_width,
        line_points=line_points,
        subnodes=resolved_subnodes,
    )


@beartype
def _wrap_text(text: str, width: int) -> list[str]:
    words = text.split()
    lines: list[str] = []
    current_line = ""
    for word in words:
        if current_line:
            if len(current_line) + 1 + len(word) <= width:
                current_line += " " + word
            else:
                lines.append(current_line)
                current_line = word
        else:
            current_line = word
    if current_line:
        lines.append(current_line)
    return lines


@beartype
def _resolve_function_call(
    call: FunctionCall,
    scope: Scope,
    parent_x: float,
    parent_y: float,
    parent_width: float | None,
    parent_height: float | None,
) -> list[ResolvedShape]:
    func_def = scope.lookup_func(call.name)
    func_scope = Scope(parent=scope)

    logging.info(f"Resolving call to function {func_def.name}")

    # Bind positional args as expressions
    for i, param_name in enumerate(func_def.params):
        if i < len(call.positional_args):
            arg_expr = call.positional_args[i]
            func_scope.define_var(param_name, arg_expr)

    # Bind keyword args with defaults
    for param_name, default_expr in func_def.keyword_params.items():
        if param_name in call.keyword_args:
            val = call.keyword_args[param_name]
            if isinstance(val, Expr):
                func_scope.define_var(param_name, val)
            elif isinstance(val, str):
                # String values become literal expressions
                func_scope.define_var(param_name, Expr.literal(val))
        else:
            # Use default expression
            func_scope.define_var(param_name, default_expr)

    # Store NODE_ARGS
    if call.subnodes:
        func_scope.node_args = call.subnodes

    # Resolve body in two passes: first non-% shapes for bounding box, then %
    results: list[ResolvedShape] = []
    for stmt in func_def.body:
        resolved = _resolve_statement(
            stmt, func_scope, parent_x, parent_y, parent_width, parent_height
        )
        if resolved is not None:
            results.extend(resolved)

    # Create invisible group bounding box
    if results:
        min_x = min(r.box.x for r in results)
        min_y = min(r.box.y for r in results)
        max_x = max(r.box.x + r.box.width for r in results)
        max_y = max(r.box.y + r.box.height for r in results)
        return [
            ResolvedShape(
                kind=ShapeKind.GROUP,
                box=ResolvedBox(
                    x=min_x, y=min_y, width=max_x - min_x, height=max_y - min_y
                ),
                subnodes=results,
            )
        ]
    return results


@beartype
def _resolve_node_args_statement(
    scope: Scope,
    parent_x: float,
    parent_y: float,
    parent_width: float | None,
    parent_height: float | None,
) -> list[ResolvedShape]:
    # Walk up scopes to find NODE_ARGS
    current = scope
    while current is not None:
        if current.node_args is not None:
            results: list[ResolvedShape] = []
            for stmt in current.node_args:
                resolved = _resolve_statement(
                    stmt, scope, parent_x, parent_y, parent_width, parent_height
                )
                if resolved:
                    results.extend(resolved)
            return results
        current = current.parent
    return []


@beartype
def _resolve_statement(
    stmt: Statement,
    scope: Scope,
    parent_x: float,
    parent_y: float,
    parent_width: float | None,
    parent_height: float | None,
) -> list[ResolvedShape] | None:
    if isinstance(stmt, FunctionDef):
        scope.define_func(stmt.name, stmt)
        return None
    elif isinstance(stmt, LetStatement):
        if isinstance(stmt.shape, FunctionCall):
            resolved = _resolve_function_call(
                stmt.shape, scope, parent_x, parent_y, parent_width, parent_height
            )
            if resolved:
                # Store the bounding box as a Rect value in a literal expression
                box = resolved[0].box
                scope.define_var(
                    stmt.name,
                    Expr(
                        type=ExprType.LITERAL,
                        value=Rect(
                            x=box.x, y=box.y, width=box.width, height=box.height
                        ),
                    ),
                )
                return resolved
            return None
        else:
            resolved_shape = _resolve_shape(
                stmt.shape, scope, parent_x, parent_y, parent_width, parent_height
            )
            # Store the bounding box as a Rect value in a literal expression
            box = resolved_shape.box
            scope.define_var(
                stmt.name,
                Expr(
                    type=ExprType.LITERAL,
                    value=Rect(x=box.x, y=box.y, width=box.width, height=box.height),
                ),
            )
            return [resolved_shape]
    elif isinstance(stmt, FunctionCall):
        return _resolve_function_call(
            stmt, scope, parent_x, parent_y, parent_width, parent_height
        )
    elif isinstance(stmt, Shape):
        resolved_shape = _resolve_shape(
            stmt, scope, parent_x, parent_y, parent_width, parent_height
        )
        return [resolved_shape]
    return None


@beartype
def resolve(
    statements: list[Statement],
    canvas_width: float | None = None,
    canvas_height: float | None = None,
) -> Scene:
    logging.debug(
        "Resolving %d statements (canvas: %sx%s)",
        len(statements),
        canvas_width,
        canvas_height,
    )
    scope = Scope()
    all_shapes: list[ResolvedShape] = []

    for stmt in statements:
        resolved = _resolve_statement(
            stmt, scope, 0.0, 0.0, canvas_width, canvas_height
        )
        if resolved:
            all_shapes.extend(resolved)

    # Infer canvas size if not provided
    if all_shapes:
        if canvas_width is None:
            canvas_width = max(s.box.x + s.box.width for s in _flatten(all_shapes))
        if canvas_height is None:
            canvas_height = max(s.box.y + s.box.height for s in _flatten(all_shapes))

    logging.debug(
        "Resolved scene with %d shapes, canvas %sx%s",
        len(all_shapes),
        canvas_width,
        canvas_height,
    )
    return Scene(shapes=all_shapes, width=canvas_width, height=canvas_height)


@beartype
def _flatten(shapes: list[ResolvedShape]) -> list[ResolvedShape]:
    result: list[ResolvedShape] = []
    for s in shapes:
        result.append(s)
        result.extend(_flatten(s.subnodes))
    return result

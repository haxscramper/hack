from __future__ import annotations

from diagrammer.ir import (
    Expr,
    ExprType,
    FunctionCall,
    FunctionDef,
    HorizontalToCommand,
    LetStatement,
    LineToCommand,
    MoveToCommand,
    ResolvedBox,
    ResolvedShape,
    Scene,
    Shape,
    ShapeKind,
    Statement,
    VerticalToCommand,
)


class ResolveError(Exception):
    pass


class Scope:

    def __init__(self, parent: Scope | None = None):
        self.parent = parent
        self.variables: dict[str, ResolvedShape] = {}
        self.functions: dict[str, FunctionDef] = {}
        self.node_args: list[Statement] | None = None

    def lookup_var(self, name: str) -> ResolvedShape:
        if name in self.variables:
            return self.variables[name]
        if self.parent is not None:
            return self.parent.lookup_var(name)
        raise ResolveError(f"Undefined variable: {name}")

    def lookup_func(self, name: str) -> FunctionDef:
        if name in self.functions:
            return self.functions[name]
        if self.parent is not None:
            return self.parent.lookup_func(name)
        raise ResolveError(f"Undefined function: {name}")

    def define_var(self, name: str, shape: ResolvedShape) -> None:
        if name in self.variables:
            raise ResolveError(f"Variable already defined: {name}")
        self.variables[name] = shape

    def define_func(self, name: str, func: FunctionDef) -> None:
        self.functions[name] = func


def _resolve_expr_scalar(
    expr: Expr,
    scope: Scope,
    parent_size: float | None,
    axis: str,
) -> float:
    match expr.type:
        case ExprType.LITERAL:
            return expr.value
        case ExprType.PERCENT:
            if parent_size is None:
                raise ResolveError(
                    "Cannot use % without a parent with known size")
            return (expr.value / 100.0) * parent_size
        case ExprType.ADD:
            l = _resolve_expr_scalar(expr.left, scope, parent_size, axis)
            r = _resolve_expr_scalar(expr.right, scope, parent_size, axis)
            return l + r
        case ExprType.SUB:
            l = _resolve_expr_scalar(expr.left, scope, parent_size, axis)
            r = _resolve_expr_scalar(expr.right, scope, parent_size, axis)
            return l - r
        case ExprType.NEG:
            v = _resolve_expr_scalar(expr.left, scope, parent_size, axis)
            return -v
        case ExprType.REF:
            resolved = scope.lookup_var(expr.name)
            if axis == "x":
                return resolved.box.x
            else:
                return resolved.box.y
        case _:
            raise ResolveError(
                f"Cannot resolve expression type {expr.type} as scalar for axis {axis}"
            )


def _resolve_expr_xy(
    expr: Expr,
    scope: Scope,
    parent_width: float | None,
    parent_height: float | None,
) -> tuple[float, float]:
    match expr.type:
        case ExprType.VEC:
            x = _resolve_expr_scalar(expr.left, scope, parent_width, "x")
            y = _resolve_expr_scalar(expr.right, scope, parent_height, "y")
            return (x, y)
        case ExprType.ADD:
            lx, ly = _resolve_expr_xy(expr.left, scope, parent_width,
                                      parent_height)
            rx, ry = _resolve_expr_xy(expr.right, scope, parent_width,
                                      parent_height)
            return (lx + rx, ly + ry)
        case ExprType.SUB:
            lx, ly = _resolve_expr_xy(expr.left, scope, parent_width,
                                      parent_height)
            rx, ry = _resolve_expr_xy(expr.right, scope, parent_width,
                                      parent_height)
            return (lx - rx, ly - ry)
        case ExprType.NEG:
            vx, vy = _resolve_expr_xy(expr.left, scope, parent_width,
                                      parent_height)
            return (-vx, -vy)
        case _:
            v = _resolve_expr_scalar(expr, scope, parent_width, "x")
            return (v, v)


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
            target = scope.lookup_var(expr.name)
            x = target.box.x - own_width
            y = target.box.y + (target.box.height - own_height) / 2.0
            return x if axis == "x" else y
        case ExprType.RIGHT_OF:
            target = scope.lookup_var(expr.name)
            x = target.box.x + target.box.width
            y = target.box.y + (target.box.height - own_height) / 2.0
            return x if axis == "x" else y
        case ExprType.ABOVE:
            target = scope.lookup_var(expr.name)
            x = target.box.x + (target.box.width - own_width) / 2.0
            y = target.box.y - own_height
            return x if axis == "x" else y
        case ExprType.BELOW:
            target = scope.lookup_var(expr.name)
            x = target.box.x + (target.box.width - own_width) / 2.0
            y = target.box.y + target.box.height
            return x if axis == "x" else y
        case ExprType.ADD:
            if _is_relational(expr.left):
                base = _resolve_position_expr(expr.left, scope, parent_width,
                                              parent_height, own_width,
                                              own_height, axis)
                if expr.right.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.right, scope, parent_width, parent_height)
                    return base + (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.right, scope,
                        parent_width if axis == "x" else parent_height, axis)
                    return base + offset
            elif _is_relational(expr.right):
                base = _resolve_position_expr(expr.right, scope, parent_width,
                                              parent_height, own_width,
                                              own_height, axis)
                if expr.left.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.left, scope, parent_width, parent_height)
                    return base + (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.left, scope,
                        parent_width if axis == "x" else parent_height, axis)
                    return base + offset
            else:
                return _resolve_expr_scalar(
                    expr, scope,
                    parent_width if axis == "x" else parent_height, axis)
        case ExprType.SUB:
            if _is_relational(expr.left):
                base = _resolve_position_expr(expr.left, scope, parent_width,
                                              parent_height, own_width,
                                              own_height, axis)
                if expr.right.type == ExprType.VEC:
                    offset_x, offset_y = _resolve_expr_xy(
                        expr.right, scope, parent_width, parent_height)
                    return base - (offset_x if axis == "x" else offset_y)
                else:
                    offset = _resolve_expr_scalar(
                        expr.right, scope,
                        parent_width if axis == "x" else parent_height, axis)
                    return base - offset
            else:
                return _resolve_expr_scalar(
                    expr, scope,
                    parent_width if axis == "x" else parent_height, axis)
        case _:
            return _resolve_expr_scalar(
                expr, scope, parent_width if axis == "x" else parent_height,
                axis)


def _is_relational(expr: Expr) -> bool:
    return expr.type in (ExprType.LEFT_OF, ExprType.RIGHT_OF, ExprType.ABOVE,
                         ExprType.BELOW)


def _contains_relational(expr: Expr) -> bool:
    if _is_relational(expr):
        return True
    if expr.left and _contains_relational(expr.left):
        return True
    if expr.right and _contains_relational(expr.right):
        return True
    return False


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


def _get_kwarg_str(kwargs: dict[str, Expr | str], key: str) -> str | None:
    if key not in kwargs:
        return None
    val = kwargs[key]
    if isinstance(val, str):
        return val
    return None


def _get_kwarg_int(kwargs: dict[str, Expr | str], key: str) -> int | None:
    if key not in kwargs:
        return None
    val = kwargs[key]
    if isinstance(val, str):
        return int(val)
    if isinstance(val, Expr) and val.type == ExprType.LITERAL:
        return int(val.value)
    return None


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

    if shape.kind == ShapeKind.CIRCLE:
        if width is not None and height is None:
            height = width
        elif height is not None and width is None:
            width = height

    if width is None:
        width = 0.0
    if height is None:
        height = 0.0

    x: float = 0.0
    y: float = 0.0

    pos_args = shape.positional_args

    if shape.kind == ShapeKind.TEXT:
        text_content = _get_kwarg_str(kwargs, "content")
        if text_content is None and len(pos_args) >= 3 and isinstance(
                pos_args[2], str):
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
            x = _resolve_position_expr(x_expr, scope, parent_width,
                                       parent_height, width, height, "x")
        if isinstance(y_expr, Expr):
            y = _resolve_position_expr(y_expr, scope, parent_width,
                                       parent_height, width, height, "y")
    elif len(pos_args) == 1:
        expr = pos_args[0]
        if isinstance(expr, Expr) and _contains_relational(expr):
            x = _resolve_position_expr(expr, scope, parent_width,
                                       parent_height, width, height, "x")
            y = _resolve_position_expr(expr, scope, parent_width,
                                       parent_height, width, height, "y")
        elif isinstance(expr, Expr) and expr.type == ExprType.VEC:
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
                length = _resolve_expr_scalar(cmd.length, scope, parent_width,
                                              "x")
                cur_x += length
                line_points.append((cur_x, cur_y))
            elif isinstance(cmd, VerticalToCommand):
                length = _resolve_expr_scalar(cmd.length, scope, parent_height,
                                              "y")
                cur_y += length
                line_points.append((cur_x, cur_y))

    # Resolve subnodes
    resolved_subnodes: list[ResolvedShape] = []
    sub_scope = Scope(parent=scope)
    for stmt in shape.subnodes:
        resolved = _resolve_statement(stmt, sub_scope, abs_x, abs_y, width,
                                      height)
        if resolved is not None:
            resolved_subnodes.extend(resolved)

    text_content_final: str | None = None
    wrap_width: int | None = None
    if shape.kind == ShapeKind.TEXT:
        # Try to get text from positional args
        for arg in pos_args:
            if isinstance(arg, str):
                text_content_final = arg
                break
        if text_content_final is None:
            text_content_final = _get_kwarg_str(kwargs, "content")
        wrap_width = _get_kwarg_int(kwargs, "wrap")
        if text_content_final and width == 0.0:
            if wrap_width:
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

    # Bind positional args
    for i, param_name in enumerate(func_def.params):
        if i < len(call.positional_args):
            # Create a dummy resolved shape to hold the value
            # For position expressions, we store them and resolve during shape resolution
            pass

    # Bind keyword args with defaults
    for param_name, default_expr in func_def.keyword_params.items():
        if param_name in call.keyword_args:
            val = call.keyword_args[param_name]
            if isinstance(val, Expr) and val.type == ExprType.LITERAL:
                func_scope.define_var(
                    param_name,
                    ResolvedShape(
                        kind=ShapeKind.GROUP,
                        box=ResolvedBox(x=val.value,
                                        y=val.value,
                                        width=0,
                                        height=0),
                    ),
                )
        else:
            if default_expr.type == ExprType.LITERAL:
                func_scope.define_var(
                    param_name,
                    ResolvedShape(
                        kind=ShapeKind.GROUP,
                        box=ResolvedBox(
                            x=default_expr.value,
                            y=default_expr.value,
                            width=0,
                            height=0,
                        ),
                    ),
                )

    # Store NODE_ARGS
    if call.subnodes:
        func_scope.node_args = call.subnodes

    # Resolve body in two passes: first non-% shapes for bounding box, then %
    results: list[ResolvedShape] = []
    for stmt in func_def.body:
        resolved = _resolve_statement(stmt, func_scope, parent_x, parent_y,
                                      parent_width, parent_height)
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
                box=ResolvedBox(x=min_x,
                                y=min_y,
                                width=max_x - min_x,
                                height=max_y - min_y),
                subnodes=results,
            )
        ]
    return results


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
                resolved = _resolve_statement(stmt, scope, parent_x, parent_y,
                                              parent_width, parent_height)
                if resolved:
                    results.extend(resolved)
            return results
        current = current.parent
    return []


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
            resolved = _resolve_function_call(stmt.shape, scope, parent_x,
                                              parent_y, parent_width,
                                              parent_height)
            if resolved:
                scope.define_var(stmt.name, resolved[0])
                return resolved
            return None
        else:
            resolved_shape = _resolve_shape(stmt.shape, scope, parent_x,
                                            parent_y, parent_width,
                                            parent_height)
            scope.define_var(stmt.name, resolved_shape)
            return [resolved_shape]
    elif isinstance(stmt, FunctionCall):
        return _resolve_function_call(stmt, scope, parent_x, parent_y,
                                      parent_width, parent_height)
    elif isinstance(stmt, Shape):
        resolved_shape = _resolve_shape(stmt, scope, parent_x, parent_y,
                                        parent_width, parent_height)
        return [resolved_shape]
    return None


def resolve(
    statements: list[Statement],
    canvas_width: float | None = None,
    canvas_height: float | None = None,
) -> Scene:
    scope = Scope()
    all_shapes: list[ResolvedShape] = []

    for stmt in statements:
        resolved = _resolve_statement(stmt, scope, 0.0, 0.0, canvas_width,
                                      canvas_height)
        if resolved:
            all_shapes.extend(resolved)

    # Infer canvas size if not provided
    if all_shapes:
        if canvas_width is None:
            canvas_width = max(s.box.x + s.box.width
                               for s in _flatten(all_shapes))
        if canvas_height is None:
            canvas_height = max(s.box.y + s.box.height
                                for s in _flatten(all_shapes))

    return Scene(shapes=all_shapes, width=canvas_width, height=canvas_height)


def _flatten(shapes: list[ResolvedShape]) -> list[ResolvedShape]:
    result: list[ResolvedShape] = []
    for s in shapes:
        result.append(s)
        result.extend(_flatten(s.subnodes))
    return result

import math

type
  Pos* = object
    x*, y*: float

  Line* = object
    x1*, x2*, y1*, y2*: float



proc makePos*(x, y: int | float): Pos =
  when x is int:
    Pos(x: x.toFloat(), y: y.toFloat())
  else:
    Pos(x: x, y: y)

converter toPos*[N: float | int](pos: (N, N)): Pos =
  when N is int:
    Pos(x: pos[0].toFloat(), y: pos[1].toFloat())
  else:
    Pos(x: pos[0], y: pos[1])



proc `-`*(a, b: Pos): Pos = makePos(a.x - b.x, a.y - b.y)
proc `+`*(a, b: Pos): Pos = makePos(a.x + b.x, a.y + b.y)
proc begin*(l: Line): Pos = Pos(x: l.x1, y: l.y1)
proc final*(l: Line): Pos = Pos(x: l.x2, y: l.y2)
proc arg*(p: Pos): float = arctan2(p.y, p.x)

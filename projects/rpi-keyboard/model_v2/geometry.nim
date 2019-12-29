## Basic geometric primitives and supporting functions

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



func `-`*(a, b: Pos): Pos = makePos(a.x - b.x, a.y - b.y)
func `+`*(a, b: Pos): Pos = makePos(a.x + b.x, a.y + b.y)
func begin*(l: Line): Pos = Pos(x: l.x1, y: l.y1)
func final*(l: Line): Pos = Pos(x: l.x2, y: l.y2)
func arg*(p: Pos): float = arctan2(p.y, p.x)
func magnitude*(p: Pos): float = sqrt(p.x ^ 2 + p.y * 2)
func `/`*(p: Pos, a: float): Pos = Pos(x: p.x / a, y: p.y / 2)
func norm*(p: Pos): Pos = p / p.magnitude()

func toLine*(a, b: Pos): Line =
  Line(x1: a.x, y1: a.y, x2: b.x, y2: b.y)

func toLine*(p: (Pos, Pos)): Line = toLine(p[0], p[1])
func toPos*(l: Line): Pos = l.final - l.begin

type
  Pos3* = object
    x*, y*, z*: float

  Line3* = object
    s*, e*: Pos3

  Size3* = object
    w*, d*, h*: float


proc makePos3*(x: float = 0.0, y: float = 0.0, z: float = 0.0): Pos3 =
  Pos3(x: x, y: y, z: z)


func makePos3*(x: int = 0, y: int = 0, z: int = 0): Pos3 =
  Pos3(x: x.toFloat(), y: y.toFloat(), z: z.toFloat())

proc makeSize3*(w,d,h: int | float): Size3 =
  when w is int:
    Size3(
      w: w.toFloat(),
      d: d.toFloat(),
      h: h.toFloat()
    )
  else:
    Size3(w: w, d: d, h: h)

func toPos3*(pos: Pos): Pos3 = Pos3(x: pos.x, y: pos.y, z: 0)

func `+`*(a, b: Pos3): Pos3 = makePos3(a.x + b.x, a.y + b.y, a.z + b.z)
func magnitude*(a: Pos3): float =
  sqrt(a.x ^ 2 + a.y ^ 2 + a.z ^ 2)

func `/`*(a: Pos3, denom: float): Pos3 =
  makePos3(a.x / denom, a.y / denom, a.z / denom)

func norm*(a: Pos3): Pos3 = a / a.magnitude()

type
  RelPos* = enum
    rpLeft
    rpRight
    rpBottom
    rpTop

func invert*(pos: RelPos): RelPos =
  case pos:
    of rpLeft: rpRight
    of rpRight: rpLeft
    of rpBottom: rpTop
    of rpTop: rpBottom

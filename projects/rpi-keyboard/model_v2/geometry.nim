## Basic geometric primitives and supporting functions

import math

type
  Vec* = object
    x*, y*: float

  Line* = object
    x1*, x2*, y1*, y2*: float



proc makeVec*(x, y: int | float): Vec =
  when x is int:
    Vec(x: x.toFloat(), y: y.toFloat())
  else:
    Vec(x: x, y: y)

converter toVec*[N: float | int](pos: (N, N)): Vec =
  when N is int:
    Vec(x: pos[0].toFloat(), y: pos[1].toFloat())
  else:
    Vec(x: pos[0], y: pos[1])



func `-`*(a, b: Vec): Vec = makeVec(a.x - b.x, a.y - b.y)
func `+`*(a, b: Vec): Vec = makeVec(a.x + b.x, a.y + b.y)
func `*`*(a: Vec, s: float): Vec = makeVec(a.x * s, a.y * s)
func `*`*(s: float, a: Vec): Vec = a * s
func begin*(l: Line): Vec = Vec(x: l.x1, y: l.y1)
func final*(l: Line): Vec = Vec(x: l.x2, y: l.y2)
func arg*(p: Vec): float = arctan2(p.y, p.x)
func magnitude*(p: Vec): float = sqrt(p.x ^ 2 + p.y ^ 2)
func `/`*(p: Vec, a: float): Vec = Vec(x: p.x / a, y: p.y / a)
func norm*(p: Vec): Vec = p / p.magnitude()
func perp*(v: Vec): Vec = makeVec(-v.y, v.x)

func toLine*(a, b: Vec): Line =
  Line(x1: a.x, y1: a.y, x2: b.x, y2: b.y)

func toLine*(p: (Vec, Vec)): Line = toLine(p[0], p[1])
func toVec*(l: Line): Vec = l.final - l.begin
func magnitude*(l: Line): float = l.toVec.magnitude

type
  Vec3* = object
    x*, y*, z*: float

  Line3* = object
    s*, e*: Vec3

  Size3* = object
    w*, d*, h*: float


proc makeVec3*(x: float = 0.0, y: float = 0.0, z: float = 0.0): Vec3 =
  Vec3(x: x, y: y, z: z)


func makeVec3*(x: int = 0, y: int = 0, z: int = 0): Vec3 =
  Vec3(x: x.toFloat(), y: y.toFloat(), z: z.toFloat())

proc makeSize3*(w,d,h: int | float): Size3 =
  when w is int:
    Size3(
      w: w.toFloat(),
      d: d.toFloat(),
      h: h.toFloat()
    )
  else:
    Size3(w: w, d: d, h: h)

func toVec3*(pos: Vec): Vec3 = Vec3(x: pos.x, y: pos.y, z: 0)

func `+`*(a, b: Vec3): Vec3 = makeVec3(a.x + b.x, a.y + b.y, a.z + b.z)
func magnitude*(a: Vec3): float =
  sqrt(a.x ^ 2 + a.y ^ 2 + a.z ^ 2)

func `/`*(a: Vec3, denom: float): Vec3 =
  makeVec3(a.x / denom, a.y / denom, a.z / denom)

func norm*(a: Vec3): Vec3 = a / a.magnitude()

type
  RelVec* = enum
    rpLeft
    rpRight
    rpBottom
    rpTop

func invert*(pos: RelVec): RelVec =
  case pos:
    of rpLeft: rpRight
    of rpRight: rpLeft
    of rpBottom: rpTop
    of rpTop: rpBottom

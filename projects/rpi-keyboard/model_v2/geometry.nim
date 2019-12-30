## Basic geometric primitives and supporting functions

import math, options
import common
import strformat

type
  Vec* = object
    x*, y*: float

  Line* = object
    x1*, x2*, y1*, y2*: float


  GeometryKind* = enum
    gkLine
    gkVec

  Geometry* = object
    case kind*: GeometryKind:
      of gkLine:
        l*: Line
      of gkVec:
        v*: Vec

converter toGeometry*(val: Line | Vec): Geometry =
  when val is Line:
    Geometry(kind: gkLine, l: val)
  elif val is Line:
    Geometry(kind: gkVec, v: val)


func makeLine*(a, b: Vec): Line =
  ## Create two lines using `a`, `b` as start/end point
  Line(x1: a.x, y1: a.y, x2: b.x, y2: b.y)

func makeLine*(p: (Vec, Vec)): Line =
  ## Create two lines using `(a, b)` as start/end point
  makeLine(p[0], p[1])

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

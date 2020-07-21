## Basic geometric primitives and supporting functions

import options
import strformat

type
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

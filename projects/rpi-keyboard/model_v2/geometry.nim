## Basic geometric primitives and supporting functions

import math, options

type
  Vec* = object
    x*, y*: float

  Line* = object
    x1*, x2*, y1*, y2*: float

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


func `-`*(a, b: Vec): Vec = makeVec(a.x - b.x, a.y - b.y)
func `+`*(a, b: Vec): Vec = makeVec(a.x + b.x, a.y + b.y)
func `*`*(a: Vec, s: float): Vec = makeVec(a.x * s, a.y * s)
func `*`*(s: float, a: Vec): Vec = a * s
func begin*(l: Line): Vec = Vec(x: l.x1, y: l.y1)
func final*(l: Line): Vec = Vec(x: l.x2, y: l.y2)
func arg*(p: Vec): float =
  ## Return vector argument in polar coordinate system
  arctan2(p.y, p.x)

func toVec*(l: Line): Vec =
  ## Return vector with magnitude and direction equal to line
  l.final - l.begin

func magnitude*(p: Vec): float =
  ## Calculate magnitude of 2d vector
  sqrt(p.x ^ 2 + p.y ^ 2)

func `/`*(p: Vec, a: float): Vec = Vec(x: p.x / a, y: p.y / a)
func norm*(p: Vec): Vec =
  ## Return normal vector
  p / p.magnitude()

func perp*(v: Vec): Vec =
  ## Return perpendicular vector
  makeVec(-v.y, v.x)

func flip*(v: Vec): Vec =
  ## Return reversed vector
  makeVec(-v.x, -v.y)

func rotate*(v: Vec, a: float): Vec =
  ## CCW rotate vector by `a` radians
  makeVec(
    v.x * cos(a) - v.y * sin(a),
    v.x * sin(a) + v.y * cos(a))

func lineEqn(l: Line): Option[tuple[a, b: float]] =
  ## Return coefficients for line equation `f(x) = ax + b`. Empty
  ## option will be returned for degenerate lines (with `l.begin.x ==
  ## l.final.x`)
  let
    p1 = l.begin
    p2 = l.begin

  try:
    let a = (p2.y - p1.y) / (p2.x - p1.x)
    let b = p1.y - a * p1.x
    result = some((a, b))
  except:
    result = none((float, float))

func inRange(val: float, rng: (float, float)): bool =
    rng[0] <= val and val <= rng[1]

func intersect*(l1, l2: Line): Option[Vec] =
  ## Calculate point of intersection between two lines if any.
  ## Intersection is only counted if intersection point is **between**
  ## begin and end points for both lines. Intersections 'at infinity'
  ## does not count.
  let eqn_l1 = l1.lineEqn()
  let eqn_l2 = l2.lineEqn()
  if eqn_l1.isSome() and eqn_l2.isSome():
    let (a1, b1) = eqn_l1.get()
    let (a2, b2) = eqn_l2.get()

    try:
      let x = (b2 - b1) / (a1 - a2)
      let y = a1 * x + b1

      debugEcho l1
      debugEcho l2
      debugEcho x
      if x.inRange((l1.x1, l1.x2)) and x.inRange((l2.x1, l2.x2)):
        result = some(Vec(x: x, y: y))
    except:
      discard

func intersect*(l: (Line, Line)): Option[Vec] =
  intersect l[0], l[1]

func moveAlong*(l: Line, v: Vec, distance: float): Line =
  ## Move line in the **direction** of the vector by `distance`
  makeLine(
    l.begin + v.norm() * distance,
    l.final + v.norm() * distance
  )

func shiftNormal*(l: Line, distance: float): Line =
  ## Move line by `distance` in the direction perpendicular to it's
  ## vector direction
  l.moveAlong(l.toVec().perp(), distance)


func magnitude*(l: Line): float =
  ## Return length of the line
  l.toVec.magnitude

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

import math
import geometry,
       common

export geometry
import options
import strformat

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

func `=~=`(a, b: float): bool =
  abs(a - b) < 0.00000000001

func inRange(val: float, rng: (float, float)): bool =
    rng[0] <= val and val <= rng[1]

func isNanOrInf(f: float): bool = f.classify in @[fcNan, fcInf, fcNegInf]

func onSegment(p: Vec, l: Line): bool =
  let t1 = (p.x - l.x1) / (l.x2 - l.x1)
  let t2 = (p.y - l.y1) / (l.y2 - l.y1)
  defer:
    de "point", p, "is on line", l, ":", result
    de t1
    de t2
  result =
    (t1.isNanOrInf() and t1.inRange((0.0, 1.0))) or
    (t2.isNanOrInf() and t2.inRange((0.0, 1.0)))

func intersect*(l1, l2: Line): Option[Vec] =
  ## Calculate point of intersection between two lines if any.
  ## Intersection is only counted if intersection point is **between**
  ## begin and end points for both lines. Intersections 'at infinity'
  ## does not count.
  let
    x1 = l1.x1
    x2 = l1.x2
    x3 = l2.x1
    x4 = l2.x2

    y1 = l1.y1
    y2 = l1.y2
    y3 = l2.y1
    y4 = l2.y2

  let denom = ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))

  if denom =~= 0.0:
    result = none(Vec)
  else:
    let pX = (
      ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) /
      denom
    )

    let pY = (
      ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) /
      denom
    )

    let intersect = Vec(x: pX, y: pY)
    # if intersect.onSegment(l1) and intersect.onSegment(l2):
    result = some(intersect)


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

func len*(l: Line): float = l.toVec().magnitude()

func `+`*(a, b: Vec3): Vec3 = makeVec3(a.x + b.x, a.y + b.y, a.z + b.z)
func magnitude*(a: Vec3): float =
  sqrt(a.x ^ 2 + a.y ^ 2 + a.z ^ 2)


func `/`*(a: Vec3, denom: float): Vec3 =
  makeVec3(a.x / denom, a.y / denom, a.z / denom)

func norm*(a: Vec3): Vec3 = a / a.magnitude()
func norm*(l: Line): Vec = l.toVec().norm()
func nperp*(l: Line): Vec = l.toVec().perp().norm()
func arg*(l: Line): float = l.toVec().arg()

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

func `$`*(v: Vec): string = &"({v.x:4.2f}, {v.y:4.2f})"
func `$`*(l: Line): string = &"({l.begin()} {l.final()})"

when isMainModule:
  echo lineEqn(makeLine(Vec(x: 0, y: 0), Vec(x: 0, y: 1)))

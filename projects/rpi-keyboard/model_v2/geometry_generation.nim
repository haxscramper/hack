## This module is used for generation of boundary boxes, separation
## lines etc. It is used in `svg_generation` and `scad_generation` to
## calculate positions of the boundary edges etc.

import geometry
import keyboard
import sequtils
import hmisc/halgorithm, hmisc/helpers
import math
import strformat

proc getLeftPoints*(blc: Block): seq[Pos] =
  var points: seq[Pos]
  var rowSpacing = 0.0

  for it in blc.rows:
    rowSpacing += it.space
    let rowWidth = it.row.width()
    points &= @[
      makePos(it.row.indent, rowSpacing),
      makePos(it.row.indent, rowSpacing + rowWidth)
    ]

    rowSpacing += rowWidth

  result = points

proc getRightPoints*(blc: Block, noFirstRow = true): seq[Pos] =
  var points: seq[Pos]
  var rowSpacing = 0.0

  for idx, it in blc.rows:
    rowSpacing += it.space
    let rowLength = it.row.totalLength()
    let rowWidth = it.row.width()

    if not noFirstRow or idx != 0:
      points &= @[
        makePos(rowLength, rowSpacing),
        makePos(rowLength, rowSpacing + rowWidth)
      ]

    rowSpacing += rowWidth

    echo points
    result = points


proc getFitPoints(
  pivots: tuple[upper, lower: Pos],
  pointsIn: seq[Pos],
  isLeft: static[bool],
  lineAngle: float,
  xOffset: float,
  gridSnap: float = 1
     ): (tuple[s, e: Pos], seq[Pos]) =
  ## Get control points for fitting line into. I offset is not `none`
  ## it will be added to resulting points in direction dependent on
  ## `isLeft` (if left then x will be subtracted, otherwise added).
  ## Return fit point to fit line into and all other points excluding
  ## pivots.

  let points = pointsIn.filterIt(pivots.upper != it and pivots.lower != it)

  echo &"Pivots: {pivots}"
  echo &"Points: {points}"

  let startP = # Find correct pivot point
    if (lineAngle > 90 and isLeft) or (lineAngle < 90 and not isLeft):
      pivots.lower
    else:
      pivots.upper

  # Find farthest point's x coordinate
  let maxPoint =
    points.twoPassSortByIt(
      it.x,
      block:
        if (lineAngle < PI/2 and isLeft) or
           (lineAngle > PI/2 and not isLeft): -it.y
        else: it.y
    )[when isLeft: 0 else: ^1][0]

  echo &"Max point is {maxPoint}"

  let endP = Pos(
    x: maxPoint.x + cos(lineAngle),
    y: maxPoint.y + sin(lineAngle)
  )


  var fit: tuple[s, e: Pos] = (maxPoint, endP)

  echo fit
  fit.s.x += xOffset * tern(isLeft, -1, 1)
  fit.e.x += xOffset * tern(isLeft, -1, 1)

  echo &"control: {fit}"


  result = (fit, points)



proc fitLine(
  pivots: tuple[upper, lower: Pos],
  pointsIn: seq[Pos],
  isLeft: static[bool],
  targetAngle: float,
  xOffset: float
     ): Line =
  ## Find line that passes through one of the pivot points and have
  ## all of the other on one side of the plane.
  echo "---"

  let (fit, points) = getFitPoints(
    pivots,
    pointsIn,
    isLeft,
    targetAngle,
    xOffset
  )

  echo &"Line angle: {targetAngle.radToDeg()}"
  let maxY: float = points.mapIt(it.y).max()

  echo &"fit: {fit}"

  result =
    Line(
      x1: fit.s.x + fit.s.y * tan(targetAngle - PI / 2),
      y1: 0.0,
      x2: fit.e.x - (maxY - fit.e.y) * tan(targetAngle - PI / 2),
      y2: maxY
  )

  let yShift = 0.5

  result.y1 -= yShift
  result.y2 += yShift

  echo &"Fit line: {result.x1}-{result.y1} {result.x2}-{result.y2}"



proc getFitLines*(blc: Block): (Line, Line) =
  ## Calculate coordinates of the left and right edge of the block
  ## boundary
  let row0 = blc.rows[0]
  let rowN = blc.rows[^1]
  let left =
    fitLine((
        makePos(0.0, row0.space + row0.row.width),
        makePos(0.0, row0.space)
    ),
            blc.getLeftPoints(),
            isLeft = true,
            targetAngle = blc.angles.left,
            xOffset = blc.offsets.left
    )

  let right =
    fitLine((
        makePos(row0.row.totalLength(), row0.space + row0.row.width),
        makePos(row0.row.totalLength(), row0.space)
    ),
            blc.getRightPoints(),
            isLeft = false,
            targetAngle = blc.angles.right,
            xOffset = blc.offsets.right
    )

  result = (left, right)

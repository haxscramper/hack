## This module is used for generation of boundary boxes, separation
## lines etc. It is used in `svg_generation` and `scad_generation` to
## calculate positions of the boundary edges etc.

import geometry
import keyboard
import sequtils
import hmisc/[halgorithm, helpers]
import math
import strformat
import strutils
import common
import tables


proc getLeftPoints*(blc: Block): seq[Pos] =
  var points: seq[Pos]
  var rowSpacing = 0.0

  for it in blc.rows:
    rowSpacing += it.space
    points &= @[
      makePos(it.row.indent, rowSpacing),
      makePos(it.row.indent, rowSpacing + it.row.keys[0].key.width)
    ]

    rowSpacing += it.row.width()

  result = points

proc getRightPoints*(blc: Block, noFirstRow = false): seq[Pos] =
  var points: seq[Pos]
  var rowSpacing = 0.0

  for idx, it in blc.rows:
    rowSpacing += it.space
    let rowLength = it.row.totalLength()

    if not noFirstRow or idx != 0:
      points &= @[
        makePos(rowLength, rowSpacing),
        makePos(rowLength, rowSpacing + it.row.keys[^1].key.width)
      ]

    rowSpacing += it.row.width()

    decho points
    result = points


proc getFitPoints(
  pivots: tuple[upper, lower: Pos],
  pointsIn: seq[Pos],
  isLeft: static[bool],
  lineAngle: float,
  xOffset: float,
  gridSnap: float = 1
     ): (tuple[s, e: Pos], seq[Pos]) =
  ## Get control points for fitting line into. If offset is not `none`
  ## it will be added to resulting points in direction dependent on
  ## `isLeft` (if left then x will be subtracted, otherwise added).
  ## Return fit point to fit line into and all other points excluding
  ## pivots.

  let points = pointsIn

  decho &"Pivots: {pivots}"
  decho &"Points: {points}"

  let startP = # Find correct pivot point
    if (lineAngle > 90 and isLeft) or (lineAngle < 90 and not isLeft):
      pivots.lower
    else:
      pivots.upper

  # Find point whose coordinates have maximum x value for right (or
  # minimum x value for left). Then sort all points with equal (max
  # ones or min ones) `x` values and find the one with smallest `y`
  # value - it will be used as starting point for line.
  let sortedFitPoints = points.twoPassSortByIt(it.x, it.y)
  let maxPoint = sortedFitPoints[when isLeft: 0 else: ^1][0]

  decho &"Sorted points: {sortedFitPoints}"
  decho &"Max point is {maxPoint}"

  let endP = Pos( # Position for the line endpoint. It does not have
                  # to account for coorrect line length: it will be
                  # fixed later when required y coordinates will be
                  # available
    x: maxPoint.x + cos(lineAngle), #
    y: maxPoint.y + sin(lineAngle)
  )


  var fit: tuple[s, e: Pos] = (maxPoint, endP)

  decho fit
  fit.s.x += xOffset * tern(isLeft, -1, 1)
  fit.e.x += xOffset * tern(isLeft, -1, 1)

  decho &"control: {fit}"


  result = (fit, points)



proc fitLine(
  pivots: tuple[upper, lower: Pos],
  pointsIn: seq[Pos],
  isLeft: static[bool],
  targetAngle: float,
  xOffset: float
     ): Line =
  ##[

Find line that passes through one of the pivot points and have all of
the other on one side of the plane.

:pivots:

  Line will pass through one of the points. Wich one is determined
  based on `isLeft` value

:isLeft:

  If `true` line will pass through lower pivot, otherwise upper one
  will be used

:targetAngle: Angle in **radians** for angle between line and x-axis
:xOffset: additional offset for line from it's calculated position.

  ]##

  decho "---"

  let (fit, points) = getFitPoints(
    pivots,
    pointsIn,
    isLeft,
    targetAngle,
    xOffset
  )

  decho &"Line angle: {targetAngle.radToDeg()}"
  let maxY: float = points.mapIt(it.y).max()

  decho &"fit: {fit}"

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

  decho &"Fit line: ({result.x1} {result.y1}) ({result.x2} {result.y2})"

proc shiftLines(blc: Block, left, right: Line): (Line, Line, Pos) =
  let
    width = blc.dimensions.width
    leftAngle = blc.angles.left
    rightAngle = blc.angles.right

  let lowerLen = right.x1 - left.x1

  if lowerLen > blc.dimensions.lowerLen:
    raise newException(Exception, "targeted lower len is smaller than fitting one")
  else:
    dlog &"lower len is ok ({lowerLen})"

  let shiftedLeft = Line(
    x1: left.x1,
    y1: left.y1,
    x2: left.x1 + cos(leftAngle) * width,
    y2: left.y1 + sin(leftAngle) * width
  )

  let shiftedRight = Line(
    x1: left.x1 + lowerLen,
    y1: left.y1,
    x2: (left.x1 + lowerLen) + cos(rightAngle) * width,
    y2: left.y1 + sin(rightAngle) * width
  )

  let startShift = Pos(
    x: (shiftedRight.x1 - right.x1) / 2,
    y: (shiftedRight.y2 - right.y2) / 2
  )

  result = (shiftedLeft, shiftedRight, startShift)


proc getFitLines*(blc: Block): (Line, Line, Pos) =
  ## Calculate coordinates of the left and right edge of the block
  ## boundary

  let
    width = blc.dimensions.width
    leftAngle = blc.angles.left
    rightAngle = blc.angles.right

  if width < blc.width():
    raise newException(Exception, "targeted width is smaller than block width")
  else:
    dlog "width ok"

  let row0 = blc.rows[0]
  let rowN = blc.rows[^1]
  let left =
    fitLine((
        makePos(0.0, row0.space + row0.row.width),
        makePos(0.0, row0.space)
    ),
    blc.getLeftPoints(),
    isLeft = true,
    targetAngle = leftAngle,
    xOffset = blc.offsets.left
    )

  let right =
    fitLine((
        makePos(row0.row.totalLength(), row0.space + row0.row.width),
        makePos(row0.row.totalLength(), row0.space)
    ),
    blc.getRightPoints(),
    isLeft = false,
    targetAngle = rightAngle,
    xOffset = blc.offsets.right
    )




  result = shiftLines(blc, left, right)
  # result = (left, right, Pos())

proc addTable[K, V](t: var Table[K, seq[V]], key: K, val: V) =
  if t.hasKey(key):
    t[key].add(val)
  else:
    t[key] = @[val]

proc arrangeBlocks*(kbd: Keyboard): seq[PositionedBlock] =
  # var relPositions: Table[int, seq[(RelPos, int, float)]]

  # for current in kbd.blocks:
  #   let curr = current.positioning
  #   relPositions.addTable curr.id, (curr.pos.invert, curr.id, curr.offset)

  let (start, other) =
    block:
      let tmp = kbd.blocks.mapIt(PositionedBlock(
        blc: it,
        hull: it.getFitLines()))

      tmp.sortedByIt(it.blc.positioning.id)
      .splitList()

  var unarranged: Table[int, PositionedBlock]
  for blc in other:
    unarranged[blc.blc.positioning.id] = blc

  var arranged = {start.blc.positioning.id : start}.newTable()
  for blId in toSeq(unarranged.keys()):
    dlog "Arranging block ", blId
    var movedBlock = unarranged[blId]
    let putAround = movedBlock.blc.positioning.relativeTo
    if not arranged.hasKey(putAround):
      dlog "Block with id", putAround, "has not been arranged yet"
      continue
    else:
      dlog "Putting it relative to id", putAround

    let stationary = arranged[putAround]
    case movedBlock.blc.positioning.pos:
      of rpLeft: discard
      of rpRight: discard
      of rpTop:
        let movedLine = (
          movedBlock.hull.left.begin,
          movedBlock.hull.right.begin
        ).toLine()

        let stationLine = (
          stationary.hull.left.final,
          stationary.hull.right.final
        ).toLine()

        let stationVec = stationLine.toPos()

        discard
      of rpBottom: discard

    dlog "Arranged id", blId
    arranged[blId] = movedBlock
    unarranged.del blId

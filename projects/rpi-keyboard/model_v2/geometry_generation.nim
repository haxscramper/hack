## This module is used for generation of boundary boxes, separation
## lines etc. It is used in `svg_generation` and `scad_generation` to
## calculate positions of the boundary edges etc.

import geom_operations
import keyboard
import sequtils
import hmisc/[halgorithm, helpers]
import math
import strformat
import common
import tables
import macros


proc getLeftPoints*(blc: Block): seq[Vec] =
  var points: seq[Vec]
  var rowSpacing = 0.0

  for it in blc.rows:
    rowSpacing += it.space
    points &= @[
      makeVec(it.row.indent, rowSpacing),
      makeVec(it.row.indent, rowSpacing + it.row.keys[0].key.width)
    ]

    rowSpacing += it.row.width()

  result = points

proc getRightPoints*(blc: Block, noFirstRow = false): seq[Vec] =
  var points: seq[Vec]
  var rowSpacing = 0.0

  for idx, it in blc.rows:
    rowSpacing += it.space
    let rowLength = it.row.totalLength()

    if not noFirstRow or idx != 0:
      points &= @[
        makeVec(rowLength, rowSpacing),
        makeVec(rowLength, rowSpacing + it.row.keys[^1].key.width)
      ]

    rowSpacing += it.row.width()

    decho points
    result = points


proc getFitPoints(
  pivots: tuple[upper, lower: Vec],
  pointsIn: seq[Vec],
  isLeft: static[bool],
  lineAngle: float,
  xOffset: float,
  gridSnap: float = 1
     ): (tuple[s, e: Vec], seq[Vec]) =
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

  let endP = Vec( # Position for the line endpoint. It does not have
                  # to account for coorrect line length: it will be
                  # fixed later when required y coordinates will be
                  # available
    x: maxPoint.x + cos(lineAngle), #
    y: maxPoint.y + sin(lineAngle)
  )


  var fit: tuple[s, e: Vec] = (maxPoint, endP)

  result = (fit, points)



proc fitLine(
  pivots: tuple[upper, lower: Vec],
  pointsIn: seq[Vec],
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

  decho &"Fit line: ({result.x1} {result.y1}) ({result.x2} {result.y2})"

proc shiftLines(blc: Block, left, right: Line): (Line, Line, Vec) =
  ## Correct line positions to match expected sizes (width of the
  ## block, it's origin position etc.). Fitting line sizes are mostly
  ## discarded and only used to calculate shift of the block core.
  let
    width = blc.dimensions.width
    leftAngle = blc.angles.left
    rightAngle = blc.angles.right

  let
    lowerLen = right.x1 - left.x1
    upperLen = right.x2 - left.x2
    targetLen = blc.dimensions.lowerLen
    upperTarget = targetLen - width / tan(leftAngle) - width / tan(rightAngle)

  # TODO provide fix in error message (i.e. increase lower len or
  # increase angles (for upper len))
  if lowerLen > targetLen:
    raise newException(Exception,
                       &"targeted lower len ({targetLen}) is smaller than fitting one ({lowerLen})")

  if upperLen > upperTarget:
    raise newException(Exception,
                       &"target upper len ({upperTarget}) is smaller than fitting one ({upperLen})")

  let shiftedLeft = Line(
    x1: 0.0,
    y1: 0.0,
    x2: width * tan(PI/2 - leftAngle),
    y2: width
  )

  let shiftedRight = Line(
    x1: targetLen,
    y1: 0.0,
    x2: targetLen - cos(rightAngle) * width,
    y2: width
  )

  let startShift = Vec(
    x: (shiftedRight.x1 - right.x1) / 2,
    y: (shiftedRight.y2 - right.y2 - blc.row0Space()) / 2
  )

  result = (shiftedLeft, shiftedRight, startShift)
  # result = (left, right, Vec(x: 0, y: 0))


proc getFitLines*(blc: Block): (Line, Line, Vec) =
  ## Calculate coordinates of the left and right edge of the block
  ## boundary

  let
    width = blc.dimensions.width
    leftAngle = blc.angles.left
    rightAngle = blc.angles.right

  if width < blc.width():
    raise newException(Exception, "targeted width is smaller than block width")

  let row0 = blc.rows[0]
  let rowN = blc.rows[^1]
  let left =
    fitLine((
        makeVec(0.0, row0.space + row0.row.width),
        makeVec(0.0, row0.space)
    ),
    blc.getLeftPoints(),
    isLeft = true,
    targetAngle = leftAngle,
    xOffset = blc.offsets.left
    )

  let right =
    fitLine((
        makeVec(row0.row.totalLength(), row0.space + row0.row.width),
        makeVec(row0.row.totalLength(), row0.space)
    ),
    blc.getRightPoints(),
    isLeft = false,
    targetAngle = rightAngle,
    xOffset = blc.offsets.right
    )

  result = shiftLines(blc, left, right)

proc addTable[K, V](t: var Table[K, seq[V]], key: K, val: V) =
  if t.hasKey(key):
    t[key].add(val)
  else:
    t[key] = @[val]


func moveTopOf(movedBlock, stationary: PositionedBlock): PositionedBlock =
  let movedLine = (
    movedBlock.hull.left.begin,
    movedBlock.hull.right.begin
  ).makeLine()

  let stationLine = (
    stationary.hull.left.final,
    stationary.hull.right.final
  ).makeLine()

  let stationVec = stationLine.toVec()
  let shift = (stationLine.magnitude() - movedLine.magnitude())
  let originPos =
    stationary.position +
    stationary.hull.left.toVec() +
    (shift / 2) * stationLine.toVec().norm() +
    stationLine.toVec().perp().norm() * movedBlock.blc.positioning.offset

  result = movedBlock
  result.position = originPos
  result.rotation = stationary.rotation


func moveLeftOf(movedBlock, stationary: PositionedBlock): PositionedBlock =
  let movedLine = movedBlock.hull.right
  let stationLine = stationary.hull.left

  de "--"
  de "id:", movedBlock.blc.positioning.id
  de stationary.blc.angles.left.radToDeg()
  de movedBlock.blc.angles.right.radToDeg()
  de stationary.rotation.radToDeg()
  de "--"

  # FIXME for some unknow reason (I was unable to figure it out even
  # after manually calculating positions) this generates perfectly
  # correct values for angles, but they are displayed incorrecly on
  # the openscad preview.
  let origRotation =
    -PI +
    stationary.blc.angles.left +
    movedBlock.blc.angles.right +
    stationary.rotation # - 4.5.degToRad()

  # for right angle of 165: (-31.0).degToRad()

  de 31.0.degtorad()
  de origrotation.radToDeg()
  de origrotation

  let movedBottom = movedBlock.hull.bottom()
  let stationLeft = stationary.hull.left

  let originPos =
    block:
      let offsetVector =
        stationary.hull.left.nperp()
          .rotate(stationary.rotation) *
        movedBlock.blc.positioning.offset

      let justificationOffset =
        stationLine
          .tovec()
          .rotate(-stationary.rotation)
          .norm() *
          ((stationLine.len() - movedLine.len()) / 2)

      # de "offset vector:        ", offsetvector
      # de "justification offset: ", justificationoffset
      # de "just + bottom:        ", justificationoffset + movedBottom.toVec().flip().rotate(origRotation)

      stationary.position +
      movedBottom.toVec().flip().rotate(origRotation) +
      offsetVector +
      justificationOffset

  # block:
  #   let bott = movedBottom.tovec().flip()
  #   let bl = bott.magnitude()
  #   de "origin pos:              ", originPos
  #   de "bottom length:           ", bott.magnitude()
  #   de "origin rotation:         ", origRotation.radToDeg()
  #   de "bottom vector:           ", bott.flip()
  #   de &"manually rotated values: x: {bl * sin(origRotation)}, y: {bl * cos(origRotation)}"
  #   de "moved bottom:            ", bott.rotate(origRotation)

  result = movedBlock
  result.position = originPos
  result.rotation = origRotation

func moveRightOf(movedBlock, stationary: PositionedBlock): PositionedBlock =
  let movedLine = movedBlock.hull.left
  let stationLine = stationary.hull.right


  let originPos =
    block:
      let offsetVector =
        stationLine.toVec().perp().norm().flip() *
        movedBlock.blc.positioning.offset

      let justificationOffset =
        stationLine.tovec().norm() *
        ((stationLine.magnitude() - movedLine.magnitude()) / 2)

      stationary.position +
      stationary.hull.bottom().toVec().rotate(stationary.rotation) +
      offsetVector +
      justificationOffset

  let origRotation =
    stationary.rotation +
    PI -
    (stationary.blc.angles.right + movedBlock.blc.angles.left)

  result = movedBlock
  result.position = originPos
  result.rotation = origRotation

func moveBottomOf(movedBlock, stationary: PositionedBlock): PositionedBlock =
  let movedLine = movedBlock.hull.top()
  let stationLine = stationary.hull.bottom()

  let stationVec = stationLine.toVec()
  let originPos =
    block:
      let offsetVector =
        stationLine.toVec().perp().norm().flip() *
        movedBlock.blc.positioning.offset

      let justificationOffset =
        stationLine.tovec().norm() *
        ((stationLine.magnitude() - movedLine.magnitude()) / 2)

      stationary.position +
      movedBlock.hull.left.toVec().flip() +
      offsetVector +
      justificationOffset

  result = movedBlock
  result.position = originPos
  result.rotation = stationary.rotation

func moveRelativeTo(movedBlock, stationary: PositionedBlock): PositionedBlock =
  case movedBlock.blc.positioning.pos:
    of rpLeft: movedBlock.moveLeftOf stationary
    of rpRight: movedBlock.moveRightOf stationary
    of rpTop: movedBlock.moveTopOf stationary
    of rpBottom: movedBlock.moveBottomOf stationary

proc generateInterlocks(
  upperLine, lowerLine: Line,
  offset: float,
  height, depth: float,
  conf: InterlockConf,
  outerDirection: bool
     ): tuple[upper, lower: Interlock] =


  let
    interlockWidth = (upperLine.len() + lowerLine.len()) * 0.5 * globalLockConf.widthMultiplier
    upperShift = (upperLine.magnitude() - interlockWidth) / 2
    lowerShift = (lowerLine.magnitude() - interlockWidth) / 2
    interlockBBox = Size3(h: height, w: interlockWidth, d: depth)
    blockPlungeDepth = (depth - offset) / 2

    upperOrigShift = upperLine.begin()
    lowerOrigShift = lowerLine.begin()

  let
    upperLock = Interlock(
      position:
        upperOrigShift +
        upperLine.norm() * upperShift -
        upperLine.nperp() * offset / 2,
      rotation: upperLine.arg(),
      size: interlockBBox,
      oddHoles: true,
      outerDirection: outerDirection
    )

  let
    lowerLock = Interlock(
      position:
        lowerOrigShift +
        lowerLine.norm() * lowerShift # -
        # lowerLine.nperp() * offset / 2
      ,
      rotation: lowerLine.arg(),
      size: interlockBBOx,
      oddHoles: false,
      outerDirection: outerDirection
    )

  result = (upperLock, lowerLock)

proc addInterlocks(
  inMovedBlock, inStationary: PositionedBlock,
  conf: InterlockConf
     ): tuple[moved, stationary: PositionedBlock] =
  ## Add interlocks for two blocks and return modified versions
  var
    moved = inMovedBlock
    stationary = inStationary

  let
    interlockDepth = globalLockConf.depth + 2
    interlockHeight = globalLockConf.height
    offset = moved.blc.positioning.offset

  case moved.blc.positioning.pos:
    of rpLeft:
      let (upperLock, lowerLock) = generateInterlocks(
        upperLine = moved.hull.right,
        lowerLine = stationary.hull.left,
        offset = offset,
        height = interlockHeight,
        depth = interlockDepth,
        conf = conf,
        outerDirection = false
      )

      moved.interlocks.left = upperLock
      stationary.interlocks.right = lowerLock
    of rpRight:
      let (upperLock, lowerLock) = generateInterlocks(
        upperLine = stationary.hull.right,
        lowerLine = moved.hull.left,
        offset = offset,
        height = interlockHeight,
        depth = interlockDepth,
        conf = conf,
        outerDirection = false
      )

      moved.interlocks.right = lowerLock
      stationary.interlocks.left = upperLock
    of rpTop:
      let (upperLock, lowerLock) = generateInterlocks(
        upperLine = moved.hull.bottom(),
        lowerLine = stationary.hull.top(),
        offset = offset,
        height = interlockHeight,
        depth = interlockDepth,
        conf = conf,
        outerDirection = false
      )

      moved.interlocks.top = upperLock
      stationary.interlocks.bottom = lowerLock
    of rpBottom:
      let (lowerLock, upperLock) = generateInterlocks(
        upperLine = stationary.hull.bottom(),
        lowerLine = moved.hull.top(),
        offset = offset,
        height = interlockHeight,
        depth = interlockDepth,
        conf = conf,
        outerDirection = false
      )

      moved.interlocks.bottom = upperLock
      stationary.interlocks.top = lowerLock

  result = (moved, stationary)

proc arrangeBlocks*(kbd: Keyboard): seq[PositionedBlock] =
  ## Correctly position block in absolute coordinates, add rotation
  ## and configure interlocks.
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
    var movedBlock = unarranged[blId]
    let stationary = arranged[movedBlock.blc.positioning.relativeTo]

    let positioned = movedBlock.moveRelativeTo stationary
    # Add interlocks to block
    let (interlockMoved, interlockStatinary) =
      addInterlocks(positioned, stationary, globalLockConf)

    # Add/replace newly arranged block
    arranged[blId] = interlockMoved

    # Replace old stationary block with new one with updated
    # interlocks
    arranged[stationary.blc.positioning.id] = interlockStatinary
    unarranged.del blId

  for id, blc in arranged:
    result &= blc

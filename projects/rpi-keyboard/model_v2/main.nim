import shell
import options
import algorithm
import math
import strformat
import sequtils
import xmltree
import colechopkg/lib
import os
import strtabs
import hmisc/helpers
import hmisc/halgorithm

import geometry
import svg_generation
import keyboard

# TODO implement
# template maxIt(sequence, operation: untyped): untyped  =
#   assert sequence.len > 0, "cannot find max of empty sequence"
#   var result: type(sequence[0]) = sequence[0]
#   for i in 1..<sequence.len:
#     let
#       lhs {.inject.} = result
#       rhs {.inject.} = sequence[i]

#     if operation: # lhs is 'smaller' than rhs
#       result = lhs
#     else:
#       result = rhs

#   return result



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


proc getLeftPoints(blc: Block): seq[Pos] =
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

proc getRightPoints(blc: Block, noFirstRow = true): seq[Pos] =
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



proc getFitLines(blc: Block): (Line, Line) =
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


proc makeControlPoints(blc: Block): seq[XmlNode] =

  let (left, right) = getFitLines(blc)

  result &=
    <-> "left bounding line" &
    @[
      left.toSVG(),
      right.toSVG(),
      Line(x1: left.x1, x2: right.x1, y1: left.y1, y2: right.y1).toSVG(),
      Line(x1: left.x2, x2: right.x2, y1: left.y2, y2: right.y2).toSVG()
    ] &
      <-> "Right points" &
      (
        block:
          let points = blc.getRightPoints()
          echo &"Debug points {points}"
          points.mapIt(it.toSVG('r', makeStyle({"fill" : "red"})))
      )
      # <-> "Left points" &
      # blc.getLeftPoints()[1..^1].mapIt(it.toSVG('l'))


proc toSVG(blc: Block): XmlNode =
  let row0 = blc.rows[0]
  var shift = row0.space
  let rows: seq[XmlNode] =
    blc.rows.mapIt(
      block:
        var rowXml = it.row.toSVG()
        shift += it.space
        rowXml.attrs = {
          "transform" : &"translate(0 {toInt(shift * svgMulti)})"
          }.toXmlAttributes()
        shift += it.row.width()
        rowXml
    )


  newXmlTree("g",
    newComment("block start") &
      rows &
      blc.makeControlPoints() &
      newComment("block end")
  )

proc toSVGImage(
  body: seq[XmlNode],
  width: int = 480,
  height: int = 480): XmlNode =
    newXmlTree(
      "svg",
      [
        newXmlTree("style", @[newText("\n" & """
.key-box {
  fill : rgb(255,255,255);
  stroke-width:3;
  stroke:rgb(0,0,0)
}
.base-control-dot {
  fill : rgb(255,0,0);
}
.coordinate {
  font: bold 16px sans-serif;
}
""")]),
        newXmlTree(
          "g", body,
          {
            # Since 2d will be mapped to 3d it is better to worh in
            # coordinate system that won't melt your brain when you
            # try to think how one thing maps to another.
            "transform" : &"""
scale(1, -1)
translate(100, 100)
translate(0, -{height})
"""
          }.toXmlAttributes())
      ],
      {
        "width" : $width,
        "height" : $height,
        "version" : "1.1",
        "xmlns" : "http://www.w3.org/2000/svg"
      }.toXmlAttributes()
    )

let test = Block(
  rows: @[
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0), 0.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0)
    ]), 0.0),
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0), 3.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
    ]), 1.0),
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0), -1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
    ]), 1.0),
],
  angles: (PI/2 + PI/18, PI/2 - PI/18),
  offsets: (0.2, 0.2)
)

let imgWidth = svgMulti * 40
let imgHeight = svgMulti * 30

let svgBody = @[test.toSVG()].toSVGImage(
  width = imgWidth,
  height = imgHeight
)

let fileName = "outy.tmp.svg"
let outFile = "out.tmp.png"
fileName.writeFile(xmlHeader & $svgBody)

let convertRes = shellVerbose:
  inkscape -z -e ($outFile) -w ($imgWidth) -h ($imgHeight) ($fileName)

if convertRes[1] == 0:
  ceUserLog0("Conversion ok")
  copyFile(outFile, "res.tmp.png")

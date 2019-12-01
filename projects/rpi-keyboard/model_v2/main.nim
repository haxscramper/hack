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





# proc `+`(a: Pos, shift: (int, int) | (float, float)): Pos =
#   let b: Pos =
#     when shift[0] is int: makePos(
#       shift[0].toFloat,
#       shift[1].toFloat)
#     else: makePos(shift[0], shift[1])

# return a + b

proc fitLine(
  pivots: tuple[upper, lower: Pos],
  pointsIn: seq[Pos],
  isLeft: static[bool]
     ): Line =
  ## Find line that passes through one of the pivot points and have
  ## all of the other on one side of the plane.
  echo "---"

  let points =
    block:
      let tmp = pointsIn.filterIt(
        pivots.upper != it and
        pivots.lower != it
      )

      tmp.sortedByIt(it.arg())

  echo &"Pivots: {pivots}"
  echo &"Points: {points}"
  let endP = tern(isLeft, points[^1], points[0])

  echo &"End point argument: {endP.arg().radToDeg()} ({endP})"
  let fit: tuple[s, e: Pos] = (
    tern(
      endP.arg() > 90,
      pivots.lower,
      pivots.upper
    ), endP)

  echo fit

  let lineAngle = (fit.e - fit.s).arg()
  echo lineAngle.radToDeg()
  let maxY: float = points.mapIt(it.y).max()

  echo &"fit: {fit}"

  result =
    Line(
      x1: fit.s.x + fit.s.y * tan(lineAngle - PI / 2),
      y1: 0.0,
      x2: fit.e.x - (maxY - fit.e.y) * tan(lineAngle - PI / 2),
      y2: maxY
  )

  let xShift = tern(isLeft, 0.5, -0.5)
  let yShift = 0.5

  result.x1 -= xShift
  result.x2 -= xShift
  result.y1 -= yShift
  result.y2 += yShift

  echo result

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
    ), blc.getLeftPoints(), true)

  let right =
    fitLine((
        makePos(row0.row.totalLength(), row0.space + row0.row.width),
        makePos(row0.row.totalLength(), row0.space)
    ), blc.getRightPoints(), false)

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
      (Key(width: 1.5, length: 2.0), 1.0)
    ]), 1.0),
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0), -1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
    ]), 1.0),
])

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

import shell
import algorithm
import math
import strformat
import sequtils
import xmltree
import colechopkg/lib
import os
import strtabs
import hmisc/helpers

const svgMulti = 50


#[

Even thougs SVG has coordinate systemd that places origin on upper
left corner and points x, y right and downwards respectively it is
easier to work with regulard coordinate system, especially when
mapping 2d image to 3d model. All coordinates, calculations, notation
etc. in this code assumes that origin is placed on lower left corner,
starts with 0,0. X axis points right and Y axis points upwards

]#

type
  Key = object
    length: float  ## On keyboard this is horizontal dimension of key
                   ## cap. Whitespace is the longest key on keyboard

    width: float ## On keyboard this is the vertical dimension of the
                 ## key cap. Most keys have equal width, only a few
                 ## ones on numpad have different dimensions


  Row = object
    ## Space before the each key and the key itself
    keys: seq[tuple[key: Key, space: float]]
    ## Rotation of the whole row around lower left corner

  Block = object
    ## Space before each row and the row itself
    rows: seq[tuple[row: Row, space: float]]
    rotation: int

  Keyboard = object
    blocks: seq[tuple[blc: Block, pos: (float, float)]]

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


proc width(row: Row): float =
  row.keys.mapIt(it.key.width).max()

proc totalLength(row: Row): float =
  row.keys.mapIt(it.space + it.key.length).sum()

proc indent(row: Row): float = row.keys[0].space

proc width(blc: Block): float =
  blc.rows.mapIt(it.space + it.row.width).sum()

proc flipUp(node: XmlNode): XmlNode =
  newXmlTree(
    "g", [node],
    {
      "transform" : "scale(1, -1)"
    }.toXmlAttributes())

proc newXmlTree(
  node: string,
  children: openarray[XmlNode],
  attributes: varargs[tuple[key, val: string]]
     ): XmlNode =
    newXmlTree(node, children, attributes.toXmlAttributes())

proc svgScale(node: XmlNode, scale: (float, float)): XmlNode =
  newXmlTree(
    "g", [node], {
      "transform" : &"scale({scale[0]}, {scale[1]})"
    })

proc toSVG(key: Key): XmlNode =
  newXmlTree(
    "rect", [],
    {
      "width" : $(key.length * svgMulti).toInt(),
      "height" : $(key.width * svgMulti).toInt(),
      "class" : "key-box"
    }.toXmlAttributes()
  )

proc `&`(attrs, addition: XmlAttributes): XmlAttributes =
  # var kvPairs: seq[tuple[key, val: string]]
  # for attr, val in attrs:
  #   kvPairs[attr] = val

  for attr, val in addition:
    attrs[attr] = val

  return attrs

proc makeSVG(
  name: string,
  attributes: varargs[tuple[key, val: string]]
     ): XmlNode =
    newXmlTree(name, [], attributes.toXmlAttributes())

proc toSVG(row: Row): XmlNode =
  var shift = 0.0
  let keys: seq[XmlNode] =
      row.keys.mapIt(
        block:
          var keyXml = it.key.toSVG()
          shift += it.space
          keyXml.attrs = keyXml.attrs &
            {"x" : $(shift * svgMulti).toInt() }.toXmlAttributes()
          shift += it.key.length
          keyXml
      )

  newXmlTree("g",
    newComment("row start") & keys & newComment("row end")
  )

proc `<->`(comm: string): XmlNode = newComment(comm)
proc toSVGsize(num: float): string = $(num * svgMulti).toInt()

type
  Line = object
    x1, x2, y1, y2: float

proc toSVG(line: Line): XmlNode =
  makeSVG("line", {
      "x1" : line.x1.toSVGsize(),
      "y1" : line.y1.toSVGsize(),
      "x2" : line.x2.toSVGsize(),
      "y2" : line.y2.toSVGsize(),
      "stroke" : "green",
      "stroke-width" : "3"
    })

proc fitLine(
  pivots: tuple[upper, lower: (float, float)],
  pointsIn: seq[(float, float)],
  mode: static[string]
     ): Line =
  ## Find line that passes through one of the pivot points and have
  ## all of the other on one side of the plane.

  let points =
    block:
      let tmp = pointsIn.filterIt(
        pivots.upper != it and pivots.lower != it)
      tmp.sortedByIt(arctan(it[1] / it[0]))

  let endP = points[^1]
  echo points

  defer:
    echo result

  let fit: tuple[s, e: (float, float)] = (
    tern(
      arctan(endP[1] / endP[0]) > 90,
      pivots.lower,
      pivots.upper
    ), endP)

  let lineAngle =
    arctan((fit.e[1] - fit.s[1]) / (fit.s[0] - fit.e[0]))

  let maxY: float = points.sortedByIt(it[1])[^1][1]

  result =
    Line(
      x1: fit.s[0] - fit.s[1] * tan(lineAngle - PI / 2),
      y1: 0.0,
      x2: fit.e[0] + (maxY - fit.e[1]) * tan(lineAngle - PI / 2),
      y2: maxY
  )

  let xShift = tern(mode == "left", 0.5, -0.5)
  let yShift = 0.5

  result.x1 -= xShift
  result.x2 -= xShift
  result.y1 -= yShift
  result.y2 += yShift


proc getFitLines(blc: Block): (Line, Line) =
  let row0 = blc.rows[0]
  let rowN = blc.rows[^1]
  let left =
    block:
      var points: seq[(float, float)]
      let pivots = (
          (0.0, row0.space + row0.row.width),
          (0.0, row0.space))

      var rowSpacing = 0.0

      for it in blc.rows:
        rowSpacing += it.space
        let rowWidth = it.row.width()
        points &= @[
          (it.row.indent, rowSpacing),
          (it.row.indent, rowSpacing + rowWidth)
        ]

        rowSpacing += rowWidth

      echo points
      fitLine(pivots, points, "left")


  let right =
    block:
      var points: seq[(float, float)]
      let pivots = (
          (row0.row.totalLength(), row0.space + row0.row.width),
          (rowN.row.totalLength(), row0.space))

      var rowSpacing = 0.0

      for it in blc.rows:
        rowSpacing += it.space
        let rowLength = it.row.totalLength()
        let rowWidth = it.row.width()
        points &= @[
          (rowLength, rowSpacing),
          (rowLength, rowSpacing + rowWidth)
        ]

        rowSpacing += rowWidth

      echo points
      fitLine(pivots, points, "right")

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
    ]


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
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0),
      (Key(width: 1.5, length: 2.0), 1.0)
    ]), 1.0),
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0), 3.0),
      (Key(width: 1.5, length: 2.0), 1.0),
    ]), 1.0),
])

let imgWidth = svgMulti * 40
let imgHeight = svgMulti * 30

let svgBody = @[test.toSVG()].toSVGImage(
  width = imgWidth,
  height = imgHeight
)

let fileName = "svg_body.tmp.svg"
let outFile = "svg_body.tmp.png"
fileName.writeFile(xmlHeader & $svgBody)

let convertRes = shellVerbose:
  inkscape -z -e ($outFile) -w ($imgWidth) -h ($imgHeight) ($fileName)

if convertRes[1] == 0:
  ceUserLog0("Conversion ok")
  copyFile(outFile, outFile & "res.png")

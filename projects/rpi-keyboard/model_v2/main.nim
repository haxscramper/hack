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


proc fitLineLeft(
  pivots: tuple[upper, lower: (float, float)],
  pointsIn: seq[(float, float)]
     ): tuple[startP, endP: (float, float)] =
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

  result = (
    (fit.s[0] - fit.s[1] * tan(lineAngle - PI / 2), 0.0),
    (fit.e[0] + (maxY - fit.e[1]) * tan(lineAngle - PI / 2), maxY),
  )

  let xShift = 0.5
  let yShift = 0.5

  result[0][0] -= xShift
  result[1][0] -= xShift

  result[0][1] -= yShift
  result[1][1] += yShift


proc fitLineRight(
  pivots: tuple[upper, lower: (float, float)],
  pointsIn: seq[(float, float)]
     ): tuple[startP, endP: (float, float)] =

    return fitLineLeft(pivots, pointsIn)

proc makeControlPoints(blc: Block): seq[XmlNode] =
  let row0 = blc.rows[0]
  let rowN = blc.rows[^1]

  let ctrlLeft: seq[(float, float)] =
    @[
      (0.0, row0.space + row0.row.width),
      (rowN.row.keys[0].space, blc.width())
    ]


  let ctrlRight: seq[(float, float)] =
    @[
      (rowN.row.totalLength(), blc.width()),
      (row0.row.totalLength(), row0.space + row0.row.width)
    ]

  var points: seq[(float, float)]
  let leftFitLine =
    block:
      let pivots = (
          (0.0, row0.space + row0.row.width),
          (0.0, row0.space)
      )

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
      fitLineLeft(pivots, points)

  result &=
    <-> "base control points start" &
    toSeq((ctrlLeft & ctrlRight).mapIt(
          makeSVG("ellipse", {
            "class" : "base-control-dot",
            "rx" : $5,
            "ry" : $5,
            "cx" : it[0].toSVGsize(),
            "cy" : it[1].toSVGsize()
          }))) &
    <-> "left bounding line" &
    makeSVG("line", {
      "x1" : leftFitLine.startP[0].toSVGsize(),
      "y1" : leftFitLine.startP[1].toSVGsize(),
      "x2" : leftFitLine.endP[0].toSVGsize(),
      "y2" : leftFitLine.endP[1].toSVGsize(),
      "stroke" : "green",
      "stroke-width" : "3"
    }) &
    toSeq(points.mapIt(
      block:
        var txt = makeSVG("text", {
          "x" : $1,
          "y" : $(-5),
          "transform" : &"""
translate ({it[0].toSVGsize()}, {it[1].toSVGsize()})
scale(1, -1)
"""
        })
        txt.add newText &"{it[0]} {it[1]}"
        txt
    )) &
    <-> "base control points end"


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

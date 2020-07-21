## THis module is used to generate 2d svg image of the keyboard. It is
## mostly indended to be used for debugging purposes

import geom_operations
import keyboard
import geometry_generation
import options
import hmisc/helpers

proc makeControlPoints*(blc: Block): seq[XmlNode] =

  let (left, right, centerShift) = getFitLines(blc)

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
          points.mapIt(it.toSVG('r', makeStyle({"fill" : "red"})))
      ) &
      <-> "Left points" &
      (
        block:
          let points = blc.getLeftPoints()
          points.mapIt(it.toSVG('r', makeStyle({"fill" : "blue"})))
      )


proc toSVG*(blc: Block): XmlNode =
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

import geometry
import keyboard
import sequtils, xmltree, strtabs, strformat, strutils
import options
import hmisc/helpers

const svgMulti* = 50

proc toSVGsize*(num: float): string = $(num * svgMulti).toInt()

proc `<->`*(comm: string): XmlNode = newComment(comm)

proc newXmlTree*(
  node: string,
  children: openarray[XmlNode],
  attributes: varargs[tuple[key, val: string]]
     ): XmlNode =
    newXmlTree(node, children, attributes.toXmlAttributes())

proc flipUp*(node: XmlNode): XmlNode =
  newXmlTree(
    "g", [node],
    {
      "transform" : "scale(1, -1)"
    }.toXmlAttributes())

# TODO proc for condensing transformations into single <g> tag

proc svgScale*(node: XmlNode, scale: (float, float)): XmlNode =
  newXmlTree(
    "g", [node], {
      "transform" : &"scale({scale[0]}, {scale[1]})"
    })

proc svgScale*(node: XmlNode, x, y: float | int): XmlNode =
  node.svgScale(
    when x is float: (x,y)
    else: (x.toFloat(), y.toFloat()))

proc svgRotate*(node: XmlNode, deg: int | float): XmlNode =
  newXmlTree(
    "g", [node], {
      "transform" : &"rotate({deg})"
    })

proc svgTranslate*(
  node: XmlNode,
  x, y: int | float | string): XmlNode =
  when (x is int) or (x is float):
    newXmlTree(
      "g", [node], {
      "transform" : &"translate({x.toSVGsize()}, {y.toSVGsize()})"
    })
  elif x is string:
    newXmlTree(
      "g", [node], {
      "transform" : &"translate({x}, {y})"
    })

proc makeSVG*(
  name: string,
  attributes: varargs[tuple[key, val: string]],
  text: Option[string] = none(string)
     ): XmlNode =
    newXmlTree(
      name,
      tern(
        text.isSome,
        @[newText(text.get())],
        @[],
      ),
      attributes.toXmlAttributes())

proc makeText*(text: string, p: Pos, textClass = "coordinate"): XmlNode =
  ## Create text at position `p`
  makeSVG(
    "text",
    {"x" : "0", "y" : "0", "class" : textClass},
    text)
  .svgTranslate(p.x, -p.y)
  .svgScale(1, -1)

proc `&`*(attrs, addition: XmlAttributes): XmlAttributes =
  # var kvPairs: seq[tuple[key, val: string]]
  # for attr, val in attrs:
  #   kvPairs[attr] = val

  for attr, val in addition:
    attrs[attr] = val

  return attrs

proc toSVG*(key: Key): XmlNode =
  newXmlTree(
    "rect", [],
    {
      "width" : $(key.length * svgMulti).toInt(),
      "height" : $(key.width * svgMulti).toInt(),
      "class" : "key-box"
    }.toXmlAttributes()
  )

proc toSVG*(row: Row): XmlNode =
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



proc toSVG*(p: Pos, annotate = 'n'): XmlNode =
  let circle = makeSVG("circle", {
    "cx" : p.x.toSVGsize(),
    "cy" : p.y.toSVGsize(),
    "r" : $8
  })
  case annotate:
    of 'n': circle
    of 'r': newXmlTree(
      "g",
      [circle, makeText(&"{p.x} {p.y}", p + (0.0, 0.1))])
    else: circle

proc toSVG*(line: Line): XmlNode =
  let a: Pos = (0,0)
  makeSVG("line", {
      "x1" : line.x1.toSVGsize(),
      "y1" : line.y1.toSVGsize(),
      "x2" : line.x2.toSVGsize(),
      "y2" : line.y2.toSVGsize(),
      "stroke" : "green",
      "stroke-width" : "3"
    })

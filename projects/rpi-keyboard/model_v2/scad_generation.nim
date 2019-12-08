## This module is used to generate openscad files for keyboard block.

import geometry
import geometry_generation
import keyboard
import strtabs
import strformat
import sequtils
import strutils
import hmisc/helpers

type
  ScadNodeType = enum
    sntInvoke
    sntComment
    sntInclude
    sntGroup

  ScadNode = object
    case kind: ScadNodeType
    of sntInvoke:
      name: string
      params: StringTableRef
      children: seq[ScadNode]
    of sntComment:
      text: string
    of sntInclude:
      path: string
    of sntGroup:
      elements: seq[ScadNode]

proc toString(node: ScadNode): string =
  case node.kind:
    of sntInvoke:
      result = node.name
      result &= "(" &
        toSeq(node.params.pairs).mapIt(&"{it.key} = {it.value}").join(",") &
        ")"

      if node.children.len == 0: result &= ";\n"
      else:
        result &= "{\n" &
          node.children.mapIt(it.toString).join("\n") &
          "}\n";
    of sntComment:
      result = &"//{node.text}\n"
    of sntInclude:
      result = &"""
// clang-format off
use <{node.path}>;
// clang-format on
"""
    of sntGroup:
      result = node.elements.map(toString).join("\n")

proc makeScadComment(text: string): ScadNode =
  ScadNode(text: text, kind: sntComment)

proc makeScadInclude(path: string): ScadNode =
  ScadNode(path: path, kind: sntInclude)

proc makeGroupWith(
  node: ScadNode,
  other: openarray[ScadNode],
  reverse: bool = false,
     ): ScadNode =
  ScadNode(elements:
    reverse.tern(
      toSeq(other) & @[node], @[node] & toSeq(other)
    ),
    kind: sntGroup)

proc addComment(node: ScadNode, comment: string): ScadNode =
  makeScadComment(comment).makeGroupWith([node])

proc makeScad(
  name: string,
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
  ScadNode(
    name: name,
    params: newStringTable(params),
    kind: sntInvoke
  )

proc makeScadTree(
  name: string,
  children: openarray[ScadNode],
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
  ScadNode(
    name: name,
    params: newStringTable(params),
    children: children.toSeq(),
    kind: sntInvoke
  )

proc setColor(
  node: ScadNode, r = 0.0, g = 0.0, b = 0.0, a = 1.0
     ): ScadNode =
  makeScadTree(
    "color",
    [node],
    {"c" : &"[{r}, {r}, {b}]", "alpha" : $a}
  )


proc setColor(node: ScadNode, colorname: string,  a = 1.0): ScadNode =
  makeScadTree("color", [node], {"c" : &"\"{colorname}\"", "alpha" : $a})



proc scadOperator(
  node: ScadNode,
  name: string,
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
    makeScadTree(name, [node], params)

proc scadTranslate(node: ScadNode, x = 0.0, y = 0.0, z = 0.0): ScadNode =
  makeScadTree("translate", [node], {"v" : &"[{x}, {y}, {z}]"})

proc scadTranslate(node: ScadNode, pos: Pos3): ScadNode =
  scadTranslate(node, x = pos.x, y = pos.y, z = pos.z)

proc scadRotate(
  node: ScadNode,
  angle: float,
  x = 0.0, y = 0.0, z = 0.0
     ): ScadNode =
  makeScadTree("rotate", [node], {
    "a" : $angle,
    "v" : &"[{x}, {y}, {z}]"
  })

proc scadSubtract(node: ScadNode, subtract: varargs[ScadNode]): ScadNode =
    makeScadTree(name = "difference", children = @[node] & subtract.toSeq())

proc scadUnion(node: ScadNode, subtract: varargs[ScadNode]): ScadNode =
    makeScadTree(name = "union", children = @[node] & subtract.toSeq())

proc `$`(size: Size3): string = &"[{size.w}, {size.d}, {size.h}]"

proc makeCube(size: Size3, center: bool = false): ScadNode =
  makeScad("cube", {
    "size" : $size,
    "center": $center
  })

proc makeCube(size: Size3, center: Pos3): ScadNode =
  makeCube(size, false).scadTranslate(center)


proc makeCube(w,d,h: float): ScadNode =
  makeCube(makeSize3(w = w, d = d, h = h), false)

proc toSCAD(key: Key): tuple[core, boundary: ScadNode] =
  ## Generate two scad notes for the key. First `core` is for placing
  ## key switches in. `boundary` is to be subtracted from row model.
  let core = makeScad(
    name = "key_core",
    params = {
      "width" : $key.width,
      "length" : $key.length,
      "height" : $key.height
      })

  let boundary = makeScad(
    name = "key_boundary",
    params = {
      "width" : $key.width,
      "length" : $key.length,
      "height" : $key.height
      })

  result = (core, boundary)

proc toSCAD(row: Row): tuple[core, boundary: ScadNode] =
  var spacing = 0.0
  let keys: seq[tuple[
    shift: Pos3, core: ScadNode, boundary: ScadNode
  ]] = row.keys.mapIt(
    block:
      let (core, boundary) = it.key.toSCAD()
      let tmp = (makePos3(x = spacing), core, boundary)
      spacing += it.key.length + it.space
      tmp
  )

  result.core =
    makeCube(d = row.width, w = row.length, h = 1.0).
    scadSubtract(
      keys.mapIt(it.boundary.scadTranslate(it.shift))).
    scadUnion(
      keys.mapIt(it.core.scadTranslate(it.shift))).
    addComment("Row core").
    setColor("Cyan", 0.3)

  result.boundary =
    makeScad(
      "row_boundary",
      {
        "width": $row.width,
        "height": $1.0,
        "length": $row.length
      }).
    setColor("Red")

proc toSCAD*(blc: Block): string =
  let (left, right) = blc.getFitLines()
  var spacing = 0.0
  let rows: seq[tuple[
    shift: Pos3, core, boundary: ScadNode
  ]] = blc.rows.mapIt(
    block:
      let (core, boundary) = it.row.toSCAD()
      let tmp = (
        makePos3(x = it.row.indent, y = spacing),
        core,
        boundary
      )
      spacing += it.space + it.row.width
      tmp
  )

  let polygonPoints: seq[Pos] =
    @[left.begin, left.final, right.final, right.begin]

  let blockBody =
    makeScad(
      "polygon", {
        "points" :
        "[" & polygonPoints.mapIt(&"[{it.x}, {it.y}]").join(",") & "]"
      }).
    scadOperator("linear_extrude", {"height" : "1"}).
    setColor(b = 1.0, a = 0.3)

  result =
    blockBody.
    scadSubtract(
      rows.mapIt(it.boundary.scadTranslate(it.shift))).
    scadUnion(
      rows.mapIt(it.core.scadTranslate(it.shift))).
    makeGroupWith(
      [ makeScadInclude("keyboard.scad") ],
      reverse = true).
    toString()

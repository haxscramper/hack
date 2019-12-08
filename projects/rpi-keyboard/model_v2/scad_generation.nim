## This module is used to generate openscad files for keyboard block.

import geometry
import geometry_generation
import keyboard
import strtabs
import strformat
import sequtils
import strutils

type
  ScadNode = object
    name: string
    params: StringTableRef
    children: seq[ScadNode]

proc toString(node: ScadNode): string =
  result = node.name
  result &= "(" &
    toSeq(node.params.pairs).mapIt(&"{it.key} = {it.value}").join(",") &
    ")"

  if node.children.len == 0: result &= ";\n"
  else:
    result &= "{\n" &
      node.children.mapIt(it.toString).join("\n") &
      "}\n";

proc makeScad(
  name: string,
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
  ScadNode(
    name: name,
    params: newStringTable(params)
  )

proc makeScadTree(
  name: string,
  children: openarray[ScadNode],
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
  ScadNode(
    name: name,
    params: newStringTable(params),
    children: children.toSeq()
  )

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
      spacing += it.key.length + it.space
      (makePos3(x = spacing), core, boundary)
  )

  result.core =
    makeCube(d = row.width, w = row.length, h = 1.0).
    scadSubtract(
      keys.mapIt(it.boundary.scadTranslate(it.shift))).
    scadUnion(
      keys.mapIt(it.core.scadTranslate(it.shift)))

  result.boundary =
    makeCube(d = row.width, w = row.length, h = 1.0)

proc toSCAD*(blc: Block): string =
  let (left, right) = blc.getFitLines()
  var spacing = 0.0
  let rows: seq[tuple[
    shift: Pos3, core, boundary: ScadNode
  ]] = blc.rows.mapIt(
    block:
      let (core, boundary) = it.row.toSCAD()
      spacing += it.space + it.row.width
      (makePos3(x = it.row.width, y = spacing), core, boundary)
  )

  let polygonPoints: seq[Pos] =
    @[left.begin, left.final, right.final, right.begin]

  let blockBody =
    makeScad(
      "polygon", {
        "points" :
        "[" & polygonPoints.mapIt(&"[{it.x}, {it.y}]").join(",") & "]"
      }).
    scadOperator("linearExtrude", {"height" : "1"})

  result =
    blockBody.
    scadSubtract(
      rows.mapIt(it.boundary.scadTranslate(it.shift))).
    scadUnion(
      rows.mapIt(it.core.scadTranslate(it.shift))).
    toString()

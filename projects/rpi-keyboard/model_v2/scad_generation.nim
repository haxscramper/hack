## This module is used to generate openscad files for keyboard block.

import geom_operations
import geometry_generation
import keyboard
import strtabs
import strformat
import sequtils
import strutils
import hmisc/helpers
import math
import common

type
  ScadNodeType = enum
    sntInvoke
    sntComment
    sntInclude
    sntGroup

  GroupModeType = enum
    gmtRegular
    gmtDebug
    gmtBackground
    gmtRoot

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
      groupMod: GroupModeType

proc toString*(node: ScadNode): string =
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
      case node.groupMod:
        of gmtRegular: discard
        of gmtDebug, gmtBackground, gmtRoot:
          let symbol =
            case node.groupMod:
              of gmtRegular: ""
              of gmtDebug: "# union()"
              of gmtBackground: "% union()"
              of gmtRoot: "! union()"

          result = &"""
// clang-format off
{symbol}{{
  {result}
}}
// clang-format on
"""

proc makeScadComment(text: string): ScadNode =
  ScadNode(text: text, kind: sntComment)

proc makeScadInclude(path: string): ScadNode =
  ScadNode(path: path, kind: sntInclude)


proc makeGroup(
  elements: openarray[ScadNode],
  gType: GroupModeType =  gmtRegular
     ): ScadNode =
  ## Make group node
  ScadNode(elements: toSeq(elements), kind: sntGroup, groupMod: gType)

proc makeGroupWith(
  node: ScadNode,
  other: openarray[ScadNode],
  reverse: bool = false,
     ): ScadNode =
  ## Add scad node to group with `other` elements. It will be added as
  ## first or last based on `reverse` value: `true` to add as last,
  ## `false` otherwise
  ScadNode(elements:
    reverse.tern(
      toSeq(other) & @[node], @[node] & toSeq(other)
    ),
    kind: sntGroup)

proc addComment(node: ScadNode, comment: string): ScadNode =
  makeScadComment(comment).makeGroupWith([node])

proc wrapComment(node: ScadNode, comment: string): ScadNode =
  if comment.find("\n") == -1:
    makeScadComment(" --- begin " & comment & " ---").makeGroupWith([
      node,
      makeScadComment(" --- end " & comment & " ---")
    ])
  else:
    let commLines = comment.split("\n")
    let commHead = " --- begin " & commLines[0] & "\n" &
      commLines[1..^1].mapIt("// " & it).join("\n") & "---"

    makeScadComment(commHead).makeGroupWith([
      node,
      makeScadComment(" --- end " & commLines[0] & "---")
    ])

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

proc scadTranslate(node: ScadNode, pos: Vec3): ScadNode =
  scadTranslate(node, x = pos.x, y = pos.y, z = pos.z)

proc scadTranslate(node: ScadNode, pos: Vec): ScadNode =
  scadTranslate(node, x = pos.x, y = pos.y, 0)

proc scadRotate(
  node: ScadNode,
  angle: float,
  x = 0.0, y = 0.0, z = 1.0
     ): ScadNode =
  makeScadTree("rotate", [node], {
    "a" : $(angle.radToDeg()),
    "v" : &"[{x}, {y}, {z}]"
  })

proc scadSubtract(node: ScadNode, subtract: varargs[ScadNode]): ScadNode =
  makeScadTree(name = "difference", children = @[node] & subtract.toSeq())


proc scadSubtract(
  node: ScadNode,
  subtract: seq[ScadNode],
  traceColor: string
     ): ScadNode =
  @[
    scadSubtract(node, subtract.toSeq()),
    [makeScadTree(name = "union", children = subtract)
      .setColor(colorname = traceColor, a = 0.3)]
      .makeGroup(gType = gmtDebug)
  ].makeGroup()


proc scadSubtract(
  node: ScadNode,
  subtract: ScadNode,
  traceColor: string
     ): ScadNode =
    scadSubtract(node, @[subtract], traceColor)

proc scadUnion(node: ScadNode, subtract: varargs[ScadNode]): ScadNode =
    makeScadTree(name = "union", children = @[node] & subtract.toSeq())

proc `$`(size: Size3): string = &"[{size.w}, {size.d}, {size.h}]"

proc makeCube(size: Size3, center: bool = false): ScadNode =
  makeScad("cube", {
    "size" : $size,
    "center": $center
  })

proc makeCube(size: Size3, center: Vec3): ScadNode =
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
  var xOffset = -row.indent # negative to ignore indent on the first
                            # key, it will be added on the block level
  let keys: seq[tuple[
    shift: Vec3, core: ScadNode, boundary: ScadNode
  ]] = row.keys.mapIt(
    block:
      let (core, boundary) = it.key.toSCAD()
      xOffset += it.space
      let tmp = (makeVec3(x = xOffset), core, boundary)
      xOffset += it.key.length
      tmp
  )

  result.core =
    makeCube(d = row.width, w = row.length, h = 1.0)
    .scadSubtract(
      keys.mapIt(it.boundary.scadTranslate(it.shift)))
    .setColor("Green")
    .scadUnion(
      keys.mapIt(it.core.scadTranslate(it.shift)))
    .wrapComment("Row core")

  result.boundary =
    makeScad(
      "row_boundary",
      {
        "width": $row.width,
        "height": $1.0,
        "length": $row.length
      })
    .setColor("Red", 0.01)
    .wrapComment("row boundary")


proc makeSCADPolygon(points: seq[Vec]): ScadNode =
  makeScad(
      "polygon", {
        "points" :
        "[" & points.mapIt(&"[{it.x}, {it.y}]").join(",") & "]"
    })

proc makeBlockTop(blc: PositionedBlock, topThickness: float = 1): ScadNode =
  ## Generate upper part of the block including mounting holes for key
  ## switches and screw holes.
  let (left, right, coreShift) = blc.hull
  var spacing = 0.0
  let rows: seq[tuple[
    shift: Vec3, core, boundary: ScadNode
  ]] = blc.blc.rows.mapIt(
    block:
      let (core, boundary) = it.row.toSCAD()
      spacing += it.space
      let tmp = (
        makeVec3(x = it.row.indent, y = spacing),
        core,
        boundary
      )
      spacing += it.row.width
      tmp
  )

  let polygonPoints: seq[Vec] =
    @[left.begin, left.final, right.final, right.begin]

  let blockBody =
    makeSCADPolygon(polygonPoints)
    .scadOperator("linear_extrude", {"height" : $topThickness})

  result =
    blockBody
    .scadSubtract(
      rows.mapIt(it.boundary.scadTranslate(it.shift + coreShift.toVec3())))
    .scadUnion(
      rows.mapIt(it.core.scadTranslate(it.shift + coreShift.toVec3())))


proc getSCADInterlocks(
  blc: PositionedBlock
     ): tuple[cutouts, bodies: ScadNode] =
  ## Generate scad code for all interlocks in the block
  let interlocks = @[
    blc.interlocks.left,
    blc.interlocks.right,
    blc.interlocks.top,
    blc.interlocks.bottom
  ].filterIt(it.isSome()).mapIt(it.get())

  let hulls =
    interlocks
    .mapIt((
      makeCube(w = it.size.w, d = it.size.d, h = it.size.h + 0.1)
      .scadRotate(it.rotation)
      .scadTranslate(it.position)
      .scadTranslate(z = -0.1)
    ))

  let bodies =
    interlocks
    .mapIt(
      makeScad("interlock", {
        "oddHoles" : $it.oddHoles,
        "width" : $it.size.w,
        "depth" : $it.size.d,
        "height" : $it.size.h})
      .scadRotate(it.rotation)
      .scadTranslate(it.position))


  result = (
    hulls.makeGroup()
    .wrapComment(&"""
Interlock cutouts block id {blc.blc.positioning.id}
Is centeral block?: {blc.blc.positioning.id == 0}
Interlocks present:
left   : {blc.interlocks.left.isSome()}
right  : {blc.interlocks.right.issome()}
top    : {blc.interlocks.top.issome()}
bottom : {blc.interlocks.bottom.issome()}
Positioned relative to {blc.blc.positioning.relativeTo}.
Relative position is {blc.blc.positioning.pos}
"""),
    bodies.makeGroup()
  )

proc makeBlockBottom(
  blc: PositionedBlock,
  baseHeight: float = 1.0,
  shellHeight: float = 4.0,
  shellThickness: float = 0.5,
     ): ScadNode =
  ## Generate bottom part of the block including interlocks, wiring
  ## and screw holes etc.
  let (left, right, coreShift) = blc.hull
  let blockHeight = baseHeight + shellHeight

  let polygonPoints: seq[Vec] =
    @[left.begin, left.final, right.final, right.begin]

  let innerPoints: seq[Vec] =
    block:
      let inLines: seq[Line] = @[
        left.shiftNormal(-shellThickness),
        makeLine(left.final, right.final).shiftnormal(-shellthickness),
        right.shiftnormal(shellthickness),
        makeLine(right.begin, left.begin).shiftnormal(-shellthickness)
      ]

      @[(0,1), (1,2), (2,3), (3,0)]
        .mapIt((inlines[it[0]], inlines[it[1]]).intersect().get())



  let blockShell =
    block:
      let outer = makeSCADPolygon(polygonPoints)
        .scadOperator("linear_extrude", {"height" : $shellHeight})

      let inner = makeSCADPolygon(innerPoints)
        .scadOperator("linear_extrude", {"height" : $(shellHeight + 0.1)})
        .scadTranslate(z = -0.05)

      let res = outer
        .scadSubtract(inner)
        .scadTranslate(z = baseHeight)

      res.wrapComment("Block shell")

  let blockBase =
    makeSCADPolygon(polygonPoints)
    .scadOperator("linear_extrude", {"height" : $baseHeight})
    .wrapComment("Block base")

  let (interlockCutouts, interlockBodies) = blc.getSCADInterlocks()

  # result = blockShell
  #   .scadUnion(blockBase)
  #   .scadSubtract(interlockCutouts)
  #   .scadUnion(interlockBodies)

  result = blockBase
    .scadSubtract(interlockCutouts)
    .wrapComment("Interlock cutouts")
    .scadUnion(interlockBodies)
    .wrapComment("Interlock block bodies")

proc toSCAD*(blc: PositionedBlock): ScadNode =
  let
    baseHeight = 1.0
    shellHeight = 4.0
    topThickness = 1.0
    shellThickness = 0.6
    bottomHeight = shellHeight + baseHeight

  let top = blc.makeBlockTop(topThickness)
  let bottom = blc.makeBlockBottom(
    shellHeight = shellHeight,
    baseHeight = baseHeight,
    shellThickness = shellThickness
  )

  let body = @[top.scadTranslate(z = bottomHeight), bottom].makeGroup()

  result =
    body
    # bottom
    .wrapComment("Block bottom")
    .scadRotate(blc.rotation)
    .scadTranslate(blc.position)
    .wrapComment("block body")

  result = result.wrapComment(&"""
block id {blc.blc.positioning.id}
id:  blc.blc.positioning.id
rot: {blc.rotation.radToDeg():3.1f} deg ccw
pos: blc.position
hull: {blc.hull.left}
    : {blc.hull.right}
""")


proc toSCAD*(kbd: Keyboard): ScadNode =
  kbd.arrangeBlocks().map(toSCAD).makeGroup()

proc addSCADImports*(body: ScadNode): ScadNode =
    body.makeGroupWith(
      [ makeScadInclude("keyboard.scad") ],
      reverse = true)

proc toScadModules*(
  blc: Block,
  screwHoles: bool = true,
                  ): tuple[top, bottom: string] =
  ## Generate 3d mode for top and bottom parts of the block.
  ## :screwHoles: Add screw holes for connecting top and bottom parts
  result = ("test", "test")

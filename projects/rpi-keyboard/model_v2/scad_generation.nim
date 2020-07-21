## This module is used to generate openscad files for keyboard block.

import geom_operations
import geometry_generation
import keyboard

proc toSCAD(key: Key): tuple[core, boundary: ScadNode] =
  ## Generate two scad notes for the key. First `core` is for placing
  ## key switches in. `boundary` is to be subtracted from row model.
  let core = makeScad(
    name = "key_core",
    params = {
      "width" : $key.width,
      "length" : $key.length,
      "height" : $key.height,
      "innerWidth" : $key.innerWidth,
      "innerLength" : $key.innerLength,
      })

  let boundary = makeScad(
    name = "key_boundary",
    params = {
        "width" : $key.width,
        "length" : $key.length,
        "height" : $key.height,
        "innerWidth" : $key.innerWidth,
        "innerLength" : $key.innerLength,
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
    makeCube(d = row.width, w = row.length, h = blockConf.topThickness)
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
    ).wrapComment("Interlock cutout block"))

  let bodies =
    interlocks
    .mapIt(
      makeScad("interlock", {
        "lockWidth" : $globalLockConf.lockWidth,
        "sectionWidth" : $globalLockConf.sectionWidth,
        "oddHoles" : $it.oddHoles,
        "width" : $it.size.w,
        "depth" : $it.size.d,
        "height" : $it.size.h,
        "offsetSize": $globalLockConf.offsetSize,
        "outerDirection": $it.outerDirection,
      })
      .wrapComment("Interlock body")
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

  when generateWhat == wholeKeyboard:
    result = blockShell
      .scadUnion(blockBase)
      .wrapComment("Block base")
      .scadSubtract(interlockCutouts)
      .wrapComment("Interlock cutouts")
      .scadUnion(interlockBodies)
      .wrapComment("Interlock block bodies")

  else:
    result = blockBase
      .scadSubtract(interlockCutouts)
      .wrapComment("Interlock cutouts")
      .scadUnion(interlockBodies)
      .wrapComment("Interlock block bodies")

proc toSCAD*(blc: PositionedBlock): ScadNode =
  ## Convert positioned block into openscad node. If `asModule` is
  ## true then generate module with `xPos`, `yPos` and `rotation`
  ## arguments. Current positioning arguments of the input block will
  ## be used as default values for arguments. Generated module name is
  ## `positioned_block_id_<block-id>`
  let exMode = defaultConf.exportMode

  proc position(node: ScadNode): ScadNode =
    if exMode == emModular:
      node
        .scadRotate("rotation")
        .scadTranslate("[xPos, yPos]")
        .wrapComment("positioning")
    else:
      node
        .scadRotate(blc.rotation)
        .scadTranslate(blc.position)
        .wrapComment("positioning")


  let top =
    block:
      let tmp = blc
        .makeBlockTop(blockConf.topThickness)
        .wrapComment("Block top")

      let defZ = blockConf.shellHeight +
        blockConf.baseHeight + blockConf.lidElevation

      if exMode == emModular:
        makeScadModule(
          name = &"block_top_id_{blc.blc.positioning.id}",
          args = {
              "xPos" : $blc.position.x, "yPos" : $blc.position.y,
              "zPos" : $defZ, "rotation" : $blc.rotation
            },
          body = @[tmp.scadTranslate("[xPos, yPos, zPos]").position()]
        )
      else:
        tmp.scadTranslate(z = defZ).position()

  let bottom =
    block:
      let tmp =
        blc.makeBlockBottom(
        shellHeight = blockConf.shellHeight,
        baseHeight = blockConf.baseHeight,
        shellThickness = blockConf.shellThickness)

      if exMode == emModular:
        makeScadModule(
              name = &"block_bottom_id_{blc.blc.positioning.id}",
              args = {
                "xPos" : $blc.position.x,
                "yPos" : $blc.position.y,
                "rotation" : $blc.rotation
                },
              body = @[tmp.position()]
            )
      else:
        tmp.position()

  result = @[top.wrapComment("top"), bottom.wrapComment("bottom")].makeGroup()

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

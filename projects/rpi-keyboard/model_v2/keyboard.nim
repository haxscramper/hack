## Keyboard components

import sequtils
import math
import options
import geometry
import geom_operations

export geometry

type
  Key* = object
    ##[

:length:

  On 2d keyboard this is horizontal dimension of key cap. Whitespace
  is the longest key on keyboard

:width:

  On 2d keyboard this is the vertical dimension of the key cap. Most
  keys have equal width, only a few ones on numpad have different
  width

:height:

  On 3d keyboard this is vertical dimension of the key cap. Most keys
  have equal height. Not present on 2d layout

    ]##

    length*: float
    width*: float
    height*: float


  Row* = object
    ## Space before the each key and the key itself
    keys*: seq[tuple[key: Key, space: float]]
    ## Rotation of the whole row around lower left corner

  InterlockConf* = object
    depth*: float
    sectionWidth*: float
    lockWidth*: float
    offsetSize*: float
    widthMultiplier*: float
    height*: float

  Interlock* = object
    ##[

Parameters for the block connectors

:position: Coordinates of the *bottom right* corner of the block
:direction: Postor placed at `position`
:size: Bounding box for interlocing part
:oddHoles: if `true` then holes start at 'bump'.

  If `true` then box is facing 'counterclocwise', otherwise it is
  facing 'clockwise'

    ]##

    position*: Vec
    rotation*: float
    size*: Size3
    oddHoles*: bool
    # holeAngles*: float
    # holeUpperWidth*: float
    outerDirection*: bool

  Block* = object
    ##[

:dimension: Targeted dimensions for the block.

  If it is possible to fit all rows in constraints given by the
  dimensions and angles block is consdered correct. Otherwise the
  block is malformed and function for generating block fitting lines
  should throw an error. Upper length is calculated based on angles,
  width and lowerLength.

:interlocks: Interlocks for connecting to the adjacent blocks
:rows: Space before each row and the row itself
:positioning: Block's position in relation to other block

  :id: mandatory field, used as unique identifier for block
  :offset: for each relative position - offset between blocks.

:offsets: minimal distance between keys and hull
    ]##

    rows*: seq[tuple[row: Row, space: float]]

    angles*: tuple[left, right: float]
    offsets*: tuple[left, right: float]
    dimensions*: tuple[width, lowerLen: float]
    positioning*: tuple[id: int, pos: RelVec, offset: float, relativeTo: int]

  PositionedBlock* = object
    ##[
:blc: Body of the positioned block
:rotation: rotation angle (in radians) around origin of the block.

  Origin of the block is lower left corner.

:position: Absolute corrdinates of the block origin
:hull: Block body sizes
  :left: Left bounding line
  :right: Right bounding line
  :coreShift: Relative shift of the block core from block origin
    This field accounts for difference between **bounding** lines
    and adjusted ones which provide correct dimensions for final
    block
:interlocks: Interlocks for connecting with adjacent blocks
    ]##
    blc*: Block
    rotation*: float
    position*: Vec
    hull*: tuple[left, right: Line, coreShift: Vec]
    interlocks*: tuple[
      left: Option[Interlock],
      right: Option[Interlock],
      top: Option[Interlock],
      bottom: Option[Interlock],
    ]



  Keyboard* = object
    interlockConf*: InterlockConf
    blocks*: seq[Block]

proc toRadianAngles*(blc: Block): Block =
  ## Convert left and right angle of the block from degree angles to
  ## radian angles.
  result = blc
  result.angles.left = result.angles.left.degToRad()
  result.angles.right = result.angles.right.degToRad()

proc toRadianAngles*(kbd: Keyboard): Keyboard =
  ## Convert left and right angle of all blocks in keyboard from
  ## degree angles to radian angles.
  result = kbd
  result.blocks = kbd.blocks.mapIt(it.toRadianAngles)

proc width*(row: Row): float =
  row.keys.mapIt(it.key.width).max()

proc totalLength*(row: Row): float =
  row.keys.mapIt(it.space + it.key.length).sum()

proc indent*(row: Row): float = row.keys[0].space
proc length*(row: Row): float = row.totalLength - row.indent
func row0Space*(blc: Block): float = blc.rows[0].space

proc width*(blc: Block): float =
  ## Return total width of the block including spaces between rows.
  blc.rows.mapIt(it.space + it.row.width).sum()

func top*(hull: tuple[left, right: Line, coreShift: Vec]): Line =
  makeLine(hull.left.final, hull.right.final)

func bottom*(hull: tuple[left, right: Line, coreShift: Vec]): Line =
  makeLine(hull.left.begin, hull.right.begin)

var globalLockConf*: InterlockConf

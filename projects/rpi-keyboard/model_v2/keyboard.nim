## Keyboard components

import sequtils
import math
import options
import geometry

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


  Interlock* = object
    ##[

Parameters for the block connectors

:position: Coordinates of the *bottom right* corner of the block
:direction: Vector placed at `position`
:size: Bounding box for interlocing part
:oddHoles: if `true` then holes start at 'bump'.

:positive: Direction for the bounding box

  If `true` then box is facing 'counterclocwise', otherwise it is
  facing 'clockwise'

    ]##

    position*: Pos
    direction*: Pos
    size*: Size3
    positive*: bool
    oddHoles*: bool
    # holeAngles*: float
    # holeUpperWidth*: float

  Block* = object
    ##[

:dimension: Targeted dimensions for the keyboard.

  If it is possible to fit all rows in constraints given by the
  dimensions and angles block is consdered correct. Otherwise the
  block is malformed and function for generating block fitting lines
  should throw an error. Upper length is calculated based on angles,
  width and lowerLength.

:interlocks: Interlocks for connecting to the adjacent blocks
:rows: Space before each row and the row itself
    ]##

    rows*: seq[tuple[row: Row, space: float]]

    angles*: tuple[left, right: float]
    offsets*: tuple[left, right: float]
    rotation*: int
    dimensions*: tuple[width, lowerLen: float]

  Keyboard* = object
    interlockConf*: tuple[
      depth: float
    ]
    blocks*: seq[tuple[blc: Block, pos: Pos]]

proc toRadianAngles*(blc: Block): Block =
  result = blc
  result.angles.left = result.angles.left.degToRad()
  result.angles.right = result.angles.right.degToRad()

proc toRadianAngles*(kbd: Keyboard): Keyboard =
  result.blocks = kbd.blocks.mapIt((
    blc: it.blc.toRadianAngles,
    pos: it.pos
  ))

proc width*(row: Row): float =
  row.keys.mapIt(it.key.width).max()

proc totalLength*(row: Row): float =
  row.keys.mapIt(it.space + it.key.length).sum()

proc indent*(row: Row): float = row.keys[0].space

proc length*(row: Row): float = row.totalLength - row.indent

proc width*(blc: Block): float =
  ## Return total width of the block including spaces between rows.
  blc.rows.mapIt(it.space + it.row.width).sum()

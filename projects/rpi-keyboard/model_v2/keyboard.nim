## Keyboard components

import sequtils
import math
import options

type
  Key* = object
    length*: float ## On 2d keyboard this is horizontal dimension of
                   ## key cap. Whitespace is the longest key on
                   ## keyboard

    width*: float ## On 2d keyboard this is the vertical dimension of
                  ## the key cap. Most keys have equal width, only a
                  ## few ones on numpad have different width

    height*: float ## On 3d keyboard this is vertical dimension of the
                   ## key cap. Most keys have equal height. Not
                   ## present on 2d layout




  Row* = object
    ## Space before the each key and the key itself
    keys*: seq[tuple[key: Key, space: float]]
    ## Rotation of the whole row around lower left corner

  Block* = object
    rows*: seq[tuple[row: Row, space: float]
    ] ## Space before each row and the row itself

    angles*: tuple[left, right: float]

    offsets*: tuple[left, right: float]

    rotation*: int

    dimensions*: tuple[width, lowerLen: float]

   ## :dimension: Targeted dimensions for the keyboard.
   ##
   ##   If it is possible to fit all rows in constraints given by the
   ##   dimensions and angles block is consdered correct. Otherwise
   ##   function for generating block fitting lines should throw an
   ##   error. Upper lenght is calculated based on angles, width and
   ##   lowerLength.

  Keyboard* = object
    blocks*: seq[tuple[blc: Block, pos: (float, float)]]

proc width*(row: Row): float =
  row.keys.mapIt(it.key.width).max()

proc totalLength*(row: Row): float =
  row.keys.mapIt(it.space + it.key.length).sum()

proc indent*(row: Row): float = row.keys[0].space

proc length*(row: Row): float = row.totalLength - row.indent

proc width*(blc: Block): float =
  ## Return total width of the block including spaces between rows.
  blc.rows.mapIt(it.space + it.row.width).sum()

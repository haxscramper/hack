## Keyboard components

import sequtils
import math
import options

type
  Key* = object
    length*: float  ## On keyboard this is horizontal dimension of key
                   ## cap. Whitespace is the longest key on keyboard

    width*: float ## On keyboard this is the vertical dimension of the
                 ## key cap. Most keys have equal width, only a few
                 ## ones on numpad have different dimensions


  Row* = object
    ## Space before the each key and the key itself
    keys*: seq[tuple[key: Key, space: float]]
    ## Rotation of the whole row around lower left corner

  Block* = object
    ## Space before each row and the row itself
    rows*: seq[tuple[row: Row, space: float]]
    angles*: tuple[left, right: float]
    offsets*: tuple[left, right: float]
    rotation*: int

  Keyboard* = object
    blocks*: seq[tuple[blc: Block, pos: (float, float)]]

proc width*(row: Row): float =
  row.keys.mapIt(it.key.width).max()

proc totalLength*(row: Row): float =
  row.keys.mapIt(it.space + it.key.length).sum()

proc indent*(row: Row): float = row.keys[0].space

proc width*(blc: Block): float =
  blc.rows.mapIt(it.space + it.row.width).sum()

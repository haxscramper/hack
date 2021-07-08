import benchy
import std/[sequtils, macros]


type Res = object
  a: char
  b: int


template loop(): untyped {.dirty.} =
  for i in 0 .. 10000:
    yield Res(b: 32)


var
  value = 0
  maxv = 0
  cnt = 0



block:
  iterator regularIter(): Res = loop()

  timeIt "Regular iterator":
    value = 0; maxv = 0; cnt = 0
    for i in regularIter():
      value += i.b
      maxv = max(maxv, i.b)
      inc cnt

  echo value, " ", maxv, " ", cnt

block:
  timeIt "Closure iterator":
    let closureIter = iterator(): Res {.closure.} = loop()
    value = 0; maxv = 0; cnt = 0
    for i in items(closureIter):
      value += i.b
      maxv = max(maxv, i.b)
      inc cnt

  echo value, " ", maxv, " ", cnt

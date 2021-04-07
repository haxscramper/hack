import benchy
import std/sequtils

const size = 100


proc varIndex[T](tmp: seq[T], shape: seq[int], i: varargs[int]): int =
  var idx = 0
  for dimension in 1 ..< len(i):
    idx = i[dimension - 1] * shape[dimension] + i[dimension]

  return tmp[idx]

doAssert varIndex(@[1], @[1], 0) == 1
doAssert varIndex(@[1, 2, 3, 4], @[2, 2], 1, 1) == 4
doAssert varIndex(@[1], @[1, 1, 1], 0, 0, 0) == 1


block:
  var tmp: seq[seq[int]]
  timeIt "Create new":
    for row in 0 ..< size:
      tmp.add @[]
      for col in 0 ..< size:
        tmp[^1].add col

  timeIt "Create nested using neqSeqWith":
    tmp = newSeqWith(size, newSeqWith(size, 0))

  timeIt "Access index":
    for row in 0 ..< size:
      for col in 0 ..< size:
        let a = tmp[row][col]
        keep(a)

block:
  var tmp: seq[int]
  timeIt "NewSeq of cap":
    tmp = newSeqOfCap[int](size * size)
    tmp.setLen(size * size)

  timeIt "Access using index math":
    for row in 0 ..< size:
      for col in 0 ..< size:
        let a = tmp[row * size + col]
        keep(a)

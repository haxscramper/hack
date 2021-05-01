import rtree
import std/bitops

type
  Pos = object
    line: uint32
    column: uint32

  PosInt = distinct uint64

  PosSlice = object
    start: Pos
    finish: Pos

  Code = object
    slice: PosSlice

proc toPosInt(pos: Pos): PosInt =
  PosInt(uint64(pos.column) + (uint64(pos.line) shl 32))

proc line(pos: PosInt): uint32 = uint32(pos.uint64 and 0x0000FFFF'u64)
proc column(pos: PosInt): uint32 = uint32(pos.uint64 shr 32)

proc toSizeInt(pos: PosInt | uint64): PosInt =
  when pos is uint64:
    result = PosInt(pos)
  else:
    result = pos

  setBit(result.uint64, 63)
  setBit(result.uint64, 31)

proc `<=`(a, b: PosInt): bool = a.uint64 <= b.uint64
proc `<`(a, b: PosInt): bool = a.uint64 < b.uint64


proc `+=`(a: var PosInt, b: PosInt) =
  a = ((a.uint64 and 0x7FFF_7FFF'u64) + (a.uint64 and 0x7FFF_7FFF'u64)) or
  0x8000_8000'u64

proc `-`(a, b: PosInt): PosInt =
  var res: uint64
  result = toSizeInt(0)
  if a.line == b.line:
    result += b.column - a.column

  toSizeInt(a.uint64 - b.uint64)

proc dist(center: BoxCenter[1, PosInt], l: L[1, PosInt, Code]): float =
  discard

proc box(code: Code): Box[1, PosInt] =
  result[0].a = code.slice.start.toPosInt()
  result[0].b = code.slice.finish.toPosInt()

when isMainModule:
  var tree = newRStarTree[8, 2, PosInt, Code]()

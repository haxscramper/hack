import hmisc/helpers


import std/[packedsets, intsets]


block:
  const cnt = 1_000_000

  timeIt "Packed set include":
    var s: PackedSet[BiggestInt]
    for i in 0 ..< cnt:
      s.incl i

  timeIt "Packed set float":
    var s: PackedSet[int32]
    for i in 0 ..< int32(cnt):
      s.incl i

  timeIt "Regular set":
    var s: set[int16]
    const loops = 40
    for _ in 0 ..< loops:
      for i in 0 ..< int16(cnt div loops):
        s.incl int16(i)

  timeIt "Intset include":
    var s: IntSet
    for i in 0 ..< cnt:
      s.incl i


import std/[setutils]
import std/[macros]

import fusion/[smartptrs]

{.compile: "clib.c".}

type
  Node {.importc, header: "clib.h".} = object
  PtrNode = ptr Node

proc newNodeRaw(): PtrNode {.importc: "newNode".}
proc freeNodeRaw(node: PtrNode) {.importc: "freeNode".}

proc `=destroy`(p: var Node) =
  echo "Custom destroy for shared pointer object"
  freeNodeRaw(addr p)

proc ptrTest() =
  // "Create new pointer"
  var newPtr: UniquePtr[Node] # = newUniquePtr(newNodeRaw())
  for val in fields(newPtr):
    val = newNodeRaw()

  echo newPtr.isNil
  echo typeof(newPtr)
  var ptr1 = newPtr
  # Error: '=copy' is not available for type <UniquePtr>; requires a copy
  # because it's not the last read of 'newPtr'; another read is done here
  # [<line/number>]

  # echo newPtr[].data

ptrTest()

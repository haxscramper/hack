import macros

type
  QGenModifiers* = enum
    qtconst
    qtoverride

macro qmethod*(body: untyped): untyped =
  echo "Registering qmethod"
  return body

macro qconst*(body: untyped): untyped =
  echo "registering qconst"
  return body

macro qoverride*(body: untyped): untyped =
  echo "registering qoverride"
  return body


macro parametric*(head: openarray[QGenModifiers], body: untyped): untyped =
  echo "parametric, head is: ", head.toStrLit()
  return body

when isMainModule:
  proc test() {.qmethod, qconst, qoverride.} =
    echo "dd"

  proc test1() {.parametric([qtconst, qtoverride]).} =
    echo "111"

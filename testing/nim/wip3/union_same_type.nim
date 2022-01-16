import pkg/union
import std/macros

macro exp(body: typed): typed =
  echo body.repr()
  result = body

expandMacros:
  var uq1: union(int | string)
  var uq2: union(int | string)

  uq1 = uq2

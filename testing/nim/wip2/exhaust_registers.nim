import std/macros

macro gen(i: static[int]): untyped =
  result = newStmtList()
  for i in 0 ..< i:
    result.add newVarStmt(ident("a" & $i), newLit(i))

macro use(i: static[int]): untyped =
  result = newStmtList()
  for i in 0 ..< i:
    result.add newCall("echo", ident("a" & $i))

static:
  const val = 10000
  gen(val)
  use(val)

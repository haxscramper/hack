import std/macros

const libgit2Static* {.booldefine.}: bool = false
const libgit2Dl* {.strdefine.}: string = "libgit2.so"

macro gitProc*(a: untyped): untyped =
  result = a
  result.addPragma(ident"importc")
  when not libgit2Static:
    result.addPragma(nnkExprColonExpr.newTree(ident"dynlib", ident"libgit2Dl"))

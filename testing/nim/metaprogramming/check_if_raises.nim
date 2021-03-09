import std/[effecttraits, macros]

macro raisesDefectAux(expr: typed): untyped =
  let raises = getRaisesList(expr)
  echo raises.treeRepr()

macro raisesDefect(expr: untyped): untyped =
  let tmpName = genSym(nskProc, "tmpName")
  quote do:
    block:
      proc `tmpName`(): auto =
        `expr`

      raisesDefectAux(`tmpName`)

type Ex = ref object of CatchableError

type
  A = object
    case test: bool
      of true:
        fld1: int
      of false: discard

raisesDefect(block: raise Ex())
raisesDefect(A().fld1)

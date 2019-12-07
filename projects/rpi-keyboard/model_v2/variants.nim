import macros

type
  VarKind* = enum
    v2type1,
    v2type2

  Var2*[T1, T2] = object
    case t*: VarKind:
      of v2type1: t1*: T1
      of v2type2: t2*: T2

macro dumpSignature(arg: typed) = echo getImpl(arg.symbol).treeRepr
  # echo getType(arg).toStrLit()

when isMainModule:
  proc acceptVariant(input: Var2[int, string]) = discard
  dumpSignature(acceptVariant)

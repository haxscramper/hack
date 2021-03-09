import macros

macro factorial(a: typed): untyped =
  a.expectKind(nnkIntLit)
  proc aux(arg: int64): int64 =
    if arg < 2:
      return arg
    else:
      return aux(arg - 2) + aux(arg - 1)

  return newLit(aux(a.intVal))

echo factorial(20)

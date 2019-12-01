type
  VarKind* = enum
    v2type1,
    v2type2

  Var2*[T1, T2] = object
    case t*: VarKind:
      of v2type1: t1*: T1
      of v2type2: t2*: T2

converter toVar2*[T1, T2](val: T1): Var2[T1, T2] =
  Var2[T1, T2](t: v2type1, t1: val)

converter toVar2*[T1, T2](val: T2): Var2[T1, T2] =
  Var2[T1, T2](t: v2type2, t2: val)

# converter toVar2*[T1, T2](val: T1 | T2): Var2[T1, T2] =
#   when val is T1:
#     result = Var2[T1, T2](t: v2type1, t1: val)
#   else:
#     result = Var2[T1, T2](t: v2type2, t2: val)

when isMainModule:
  proc acceptVariant(input: Var2[int, string]) = discard
  acceptVariant(12)

  # # Error: cannot instantiate: 'toVar2[int, string]'; got 2 type(s)
  # # but expected 3
  # acceptVariant(toVar2[int, string](12))

  # # Error: type mismatch: got <int literal(12)> but expected one of:
  # # proc acceptVariant(input: Var2[int, string])
  # acceptVariant(12)

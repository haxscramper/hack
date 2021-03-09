# HOWTO: cast derived object to parent type

type
  Base = ref object of RootObj
    f1: int

  Derived = ref object of Base
    f2: int

func toBase(d: Derived): Base = d.Base

type
  Res[V, E] = object
    case kind: bool
    of true: ok: V
    of false: err: E

# func makeResult[V, E](val: V | E): Res[V, E] =
#   when val is V:
#     result = Res[V, E](kind: true, val: val)
#   else:
#     result = Res[V, E](kind: false, val: val)

# func makeResult[T: V | E](val: T): Res[V, E] =
#     when T is V:
#       result = Res[V, E](kind: true, ok: val)
#     else:
#       result = Res[V, E](kind: false, err: val)

func makeResult[V, E](val: V): Res[V, E] = result.ok = val
func makeResult[V, E](val: E): Res[V, E] = result.err = val


echo makeResult[int, string]("12")

# proc test1(val: int | string) = echo typeof val
# test1(12)

# proc test[E, V](val: int | string) =
#   echo typeof val

# test[int, string](12)

# type
#   Res[V, E] = object
#     case kind: bool
#     of true: ok: V
#     of false: err: E

# func makeResultImpl[V, E, T](val: T): Res[V, E] =
#   when T is V:
#     result = Res[V, E](kind: true, ok: val)
#   else:
#     result = Res[V, E](kind: false, err: E)

# func makeResult[V, E](val: typed): Res[V, E] =
#    makeResultImpl[V, E, typeof(val)]

# echo makeResult[int, string](12)

import macros

macro testgen(): untyped =
  let argid = ident("arg")
  quote do:
    proc generate(`argid`: int): int =
      arg + 12

testgen()


#================  generate enumeration and case object  =================#

dumpTree:
  type
    En = enum
      enF1
      enF2

    EnObj = object
      case kind: En
      of enF1: f1: int
      of enF2: f2: string

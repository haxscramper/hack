import macros
import typetraits
import sequtils

type
  Var4[T0, T1, T2, T3] = object
    case idx: range[0 .. 3]:
      of 0: f0: T0
      of 1: f1: T1
      of 2: f2: T2
      of 3: f3: T3

  Var3[T0, T1, T2] = Var4[T0, T1, T2, void]
  Var2[T0, T1] = Var4[T0, T1, void, void]

template get[T0, T1, T2, T3](v: Var4[T0, T1, T2, T3], t: typed): auto =
  var idx: int = 0
  var res: t
  for name, fld in v.fieldPairs():
    when fld is t:
      if v.idx == idx:
        res = fld
      else:
        raiseAssert("Cannot get value for type `" & $typeof(t) &
          "` - current variant index is " & $v.idx & ", but " &
          "value with type `" & $typeof(t) & "` has index " & $idx)

    if name != "idx":
      inc idx

  res


func setv[T0, T1, T2, T3](v: var Var4[T0, T1, T2, T3], val: T0) =
  v = Var4[T0, T1, T2, T3](idx: 0, f0: val)

func setv[T0, T1, T2, T3](v: var Var4[T0, T1, T2, T3], val: T1) =
  v = Var4[T0, T1, T2, T3](idx: 1, f1: val)

func setv[T0, T1, T2, T3](v: var Var4[T0, T1, T2, T3], val: T2) =
  v = Var4[T0, T1, T2, T3](idx: 2, f2: val)

func setv[T0, T1, T2, T3](v: var Var4[T0, T1, T2, T3], val: T3) =
  v = Var4[T0, T1, T2, T3](idx: 3, f3: val)

var test: Var2[int, float]
echo test.get(int)

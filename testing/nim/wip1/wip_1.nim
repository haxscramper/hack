import macros
block:
  var
    max = 0
    min = 10
    a = [1, 0, -5, 100, 8, 16, 10]
    k = 0
    n = 0

  for i in 0 ..< 7:
    if a[i] < min:
      min = a[i]
      k = i

    if a[i] > max:
      max = a[i]
      n = i

  echo min
  echo k + 1
  echo max
  echo n + 1
echo "\e[41m*==========\e[49m  dd  \e[41m==========*\e[49m"
block:
  var
    k, i: int
    x = [2,7,6,1,9,5,8,3,4,0]

  for i in 0 .. 9:
    if x[i] mod 2 == 0:
      echo i + 1, " "

block:
  var
    k = 8
    m = 11
    p = 5
    z = k

  if m > z and m > p:
    z = m
  elif p > z and p > m:
    z = p


  echo "z = ", z


type
  Base = ref object of RootObj
    fld1: int

  Derived = ref object of Base
    fld2: string

when false:
  macro ctor(body: untyped): untyped =
    result = newStmtList(body)
    let
      typeName = ident(body[0].strVal()[3..^1])
      ctorName = body[0]

    result.add quote do:
      proc `ctorName`(): `typeName` =
        `ctorName`(result)

  proc newBase(val: var Base) {.ctor.} =
    if val == nil: new(val)
    val.fld1 = 12

  proc newDerived(val: var Derived) {.ctor.} =
    if val == nil: new(val)
    val.fld2 = "somefield"
    newBase Base(val)

else:
  proc newBase(val: var Base) =
    if val == nil: new(val)
    val.fld1 = 12

  proc newBase(): Base = newBase(result)

  proc newDerived(val: var Derived) =
    if val == nil: new(val)
    val.fld2 = "somefield"
    newBase Base(val)

  proc newDerived(): Derived = newDerived(result)

  echo newDerived()[]
  echo newBase()[]

import macros

macro unshadow(a: typed): typed =
  echo a

let a = 12
a.unshadow
block:
  let a = 123
  a.unshadow
  block:
    let a = 90
    a.unshadow
    echo a
  echo a
echo a

# echo typeof (@"12")

block:
  macro mapItt(expr: untyped, itvar: untyped): untyped =
    defer:
      echo result.toStrLit()
      echo result.treeRepr()

    result = quote do:
      let `itvar` {.inject.} = 12
      `expr`

  echo mapItt(mapItt(it1 + it, it), it1)

block:
  template mapItt(expr: untyped, itvar: untyped): untyped =
    let itvar {.inject.} = 12
    expr

  template mapItt(expr: untyped): untyped =
    mapItt(expr, it)

  echo mapItt(mapItt(it1 + it), it1)

block:
  macro mapItt(expr: untyped, vars: untyped): untyped =
    echo vars.treeRepr()
    let var1 = vars[0]
    result = quote do:
      let `var1` = 12
      `expr`

  template mapItt(expr: untyped): untyped =
    mapItt(expr, (it, itTmp))


  echo mapItt(mapItt(it1 + it), (it1, itTmp))

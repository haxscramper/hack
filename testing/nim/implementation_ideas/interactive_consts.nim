import std/[colors, macros]

type
  ICKind = enum
    ickColor
    ickFloat

  IC = object
    sourceStart: LineInfo
    exprStr: string
    constType: string
    isSet: bool
    case kind*: ICKind
      of ickFloat:
        floatVal: float

      of ickColor:
        colorVal: Color

var usedConsts {.compiletime.}: seq[IC]

macro interactiveConst(
  expr, name: untyped, constKind: static[ICKind]): untyped =

  let icId = newLit(usedConsts.len)

  let valKind = case constKind:
    of ickFloat: ident("floatVal")
    of ickColor: ident("colorVal")

  result = quote do:
    if not isIcSet(`icId`):
      setIc(`icId`, `expr`)

    getIC(`icId`).`valKind`


  usedConsts.add IC(
    kind: constKind,
    sourceStart: expr.lineInfoObj(),
    exprStr: toStrLit(expr).strVal,
    isSet: false
  )

var icMap: seq[IC]

proc isIcSet(id: int): bool = icMap[id].isSet
proc getIc(id: int): IC = icMap[id]
proc setIc[T](id: int, val: T) =
  when val is float:
    icMap[id].floatVal = val

  else:
    icMap[id].colorVal = val

  icMap[id].isSet = true

proc newLit(col: Color): NimNode =
  newCall("Color", newLit(col.int))

macro initICVals(): untyped =
  result = nnkBracket.newTree()
  for ic in usedConsts:
    echo ic
    result.add newLit(ic)

  result = nnkAsgn.newTree(
    ident "icMap",
    nnkPrefix.newTree(ident "@", result)
  )

proc main() =
  let val = interactiveConst(0.2, "Velocity", ickFloat)

initICVals()
main()







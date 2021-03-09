import result, std/[macros, effecttraits, strutils]

type
  Exc* = ref object of CatchableError
    tmp: int

proc raisesSomething(): int =
  let tmp = Exc()
  raise tmp

macro asResultImpl(expr: typed): untyped =
  let
    ex = ident getRaisesList(expr)[0].strVal().split(":")[0]
    call = newCall expr

  result = quote do:
    var res: Result[typeof(`call`), `ex`]
    try:
      res.ok `call`
    except:
      res.err `ex`(getCurrentException())

    res


template asResult(body: typed): untyped =
  proc tmp(): auto = body
  asResultImpl(tmp)


let res = asResult:
  let a = raisesSomething()
  a

echo typeof(res)

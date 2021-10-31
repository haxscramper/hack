import std/[macros, effecttraits, strutils]
import union

type
  Exc* = ref object of CatchableError
    tmp: int

  Exc2* = ref object of CatchableError

proc raisesSomething(): int =
  if true:
    raise Exc()

  else:
    raise Exc2()


macro asUnionImpl(expr: typed, body: untyped): untyped =
  let ex = getRaisesList(expr)
  var etry = newTree(nnkTryStmt, body)

  for err in ex:
    let name = err.strVal().split(":")[0]
    etry.add newTree(
      nnkExceptBranch,
      nnkInfix.newTree(ident"as", ident(name), ident"e"),
      newStmtList(ident"e"))

  result = newCall("makeUnion", etry)


template asUnion(body: typed): untyped =
  proc tmp(): auto = body
  asUnionImpl(tmp, body)


let res = asUnion(raisesSomething())

{.experimental: "caseStmtMacros".}
import fusion/matching

func isNil*(u: union(int | Exc)): bool = false
func isNil*(u: union(int | Exc | Exc2)): bool = false

case res:
  of of int():
    echo "got value"

  of of Exc():
    echo "got error"


echo typeof(res)

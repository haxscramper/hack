import std/[macros, sequtils, lenientops]

type
  TypeKind = enum tkString, tkInt, tkFloat
  VNType = object
    case kind: TypeKind
      of tkString:
        stringVal*: string

      of tkInt:
        intVal*: int

      of tkFloat:
        floatVal*: float


  UnexpectedArgType* = object of CatchableError

func toMapArray*[K, V](map: openarray[(K, V)]): array[K, V] =
  for (k, v) in map:
    result[k] = v

const identMap: array[TypeKind, string] = toMapArray {
  tkString: "stringVal",
  tkInt: "intVal",
  tkFloat: "floatVal"
}

macro lift(
    op: static[string],
    kindMap: static[openarray[(seq[TypeKind], TypeKind)]],
    identMap: static[array[TypeKind, string]],
    outType: static[string],
    args: varargs[untyped]
  ): untyped =

  let
    inKind = genSym(nskLet)
    getKind = nnkBracket.newTree(args.mapIt(nnkDotExpr.newTree(it, ident"kind")))

  var match = nnkIfStmt.newTree()
  for (inKinds, outKind) in kindMap:
    let
      check = nnkInfix.newTree(ident"==", inKind, nnkBracket.newTree(
        inKinds.mapIt(ident($it))))

      body = nnkReturnStmt.newTree(nnkObjConstr.newTree(
        ident(outType),
        nnkExprColonExpr.newTree(ident"kind", ident($outKind)),
        nnkExprColonExpr.newTree(
          ident identMap[outKind],
          newCall(op, zip(toSeq(args), inKinds).mapIt(
            nnkDotExpr.newTree(it[0], ident identMap[it[1]]))))))


    match.add nnkElifBranch.newTree(check, newStmtList(body))

  match.add nnkElse.newTree((
    quote do:
      raise newException(
        UnexpectedArgType,
        "Could not apply procedure " & `op` & " to arguments to types " &
          $inKind)))

  result = quote do:
    let `inKind` = `getKind`
    `match`

  echo result.repr()

proc `-`(a, b: VnType): VnType =
  lift("-", {
    @[tkInt, tkInt]: tkInt,
    @[tkInt, tkFloat], @[tkFloat, tkFloat]: tkFloat
  }, identMap, "VnType", a, b)

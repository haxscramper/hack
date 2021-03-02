import fusion/matching
import std/[macros, options]

{.experimental: "caseStmtMacros".}

macro matchCall(procs: untyped): untyped =
  var patterns: seq[tuple[pattern: NimNode, funcName: string]]

  result = newStmtList()

  var topParams: tuple[name: string, arg0, type0Name, returnType: NimNode]

  for idx, pr in pairs(procs):
    pr.assertMatch:
      ProcDef:
        Ident(strVal: @name)
        @trmTemplate # Term rewriting template
        @genParams # Generic params
        FormalParams:
          @returnType
          all @arguments

        @pragmas
        _ # Reserved
        @implementation

    topParams.name = name

    arguments[0].assertMatch: # FIXME handles only one-argument functions
      IdentDefs:
        @arg0Name
        CurlyExpr[@type0Name, @pattern]
        _

    topParams.returnType = returnType
    topParams.type0Name = type0Name
    topParams.arg0 = arg0Name

    let funcName = name & "impl" & $idx
    result.add nnkProcDef.newTree(
      ident(funcName),
      trmTemplate,
      genParams,
      nnkFormalParams.newTree(@[
        returnType, nnkIdentDefs.newTree(arg0Name, type0Name, newEmptyNode())
      ]),
      pragmas,
      newEmptyNode(),
      implementation
    )


    patterns.add((pattern, funcName))

  var dispatchImpl = nnkCaseStmt.newTree(topParams.arg0)
  for (patt, funcName) in patterns:
    dispatchImpl.add nnkOfBranch.newTree(
      patt, nnkReturnStmt.newTree(newCall(funcName, topParams.arg0)))

  result.add nnkProcDef.newTree(
    ident(topParams.name),
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(@[
      topParams.returnType,
      nnkIdentDefs.newTree(topParams.arg0, topParams.type0Name, newEmptyNode())
    ]),
    newEmptyNode(),
    newEmptyNode(),
    dispatchImpl
  )

  echo result.repr




matchCall:
  proc pattern(a: NimNode{Infix[Ident(strVal: == "+"), .._]}): NimNode =
    echo "Matches plus"
    result = newEmptyNode()

  proc pattern(a: NimNode{Infix[Ident(strVal: == "-"), .._]}): NimNode =
    echo "Matches minus"
    result = newEmptyNode()


macro usesPattern(body: untyped): untyped =
  return pattern(body)

usesPattern(12 + 2)
usesPattern(2 - 2)

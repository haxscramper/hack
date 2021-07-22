import fusion/matching, std/[macros, sequtils]

{.experimental: "caseStmtMacros".}

func toPattern(ast: NimNode): NimNode =
  case ast.kind:
    of nnkIdent:
      result = nnkObjConstr.newTree(
        ident("Ident"),
        nnkExprColonExpr.newTree(
          ident("strVal"),
          newLit(ast.strVal())))

    of nnkPrefix:
      result = ast

    else:
      result = nnkBracketExpr.newTree(ident(($ast.kind)[3 ..^ 1]))
      for sub in ast:
        result.add sub.toPattern()



macro matchAst(head: untyped, arms: varargs[untyped]): untyped =
  result = nnkCaseStmt.newTree(head)
  for arm in arms:
    result.add nnkOfBranch.newTree(
      arm[0 ..^ 2].mapIt(it.toPattern()).foldl(
        nnkInfix.newTree(ident"|", a, b)), arm[^1])

  echo result.repr


macro withmatch(head: untyped): untyped =
  matchAst head:
    of @lhs + @rhs:
      echo "matched addition"

    of @lhs - @rhs:
      echo "matched substraction"

    of @head[@body]:
      echo "matched array access with one element"

    of @head[@head1, @head2]:
      echo "matched array access with two elements"

    of @call(@head), @call():
      echo "matched call with arguments"
      if head.isSome():
        echo "had one argument"

withMatch(1 + 2)
withMatch(1 - 2)
withMatch(a[b])
withMatch(a[4, 5])
withMatch(call(12))
withMatch(call())

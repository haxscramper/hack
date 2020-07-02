import macros

import hmisc/hterms_nimast

macro derive(args: varargs[untyped]): untyped =
  # echo args.treeRepr()
  for a in args:
    echo a.toStrLit()

  echo args[^1].treeRepr()

macro typesection(a: untyped): untyped =
  let rewrite = makeNodeRewriteSystem:
    rule:
      patt: IdentDefs([[idName]], [[idType]], [[idVal]])
      outp:
        # echo "reduce"
        nnkIdentDefs.newTree(
          idName,
          idType,
          newEmptyNode()
        )

  let term = a.toTerm()
  # let nodeTree = proc(n: NimNode): string = n.treeRepr()
  let reduced = reduce(
    term, rewrite, nimAstImpl
  )

  if reduced.ok:
    result = reduced.term.fromTerm()


typesection:
  type
    Hello* = object
      f1: int = 1
      f2: float = 1

proc param(a: int, b: int = a + 1): void =
  echo a, b

param(12)

type
  AstNode[Node] = object
    when Node is NimNode:
      node: Node
    else:
      discard

    f1: int

static:
  echo AstNode[NimNode]()

echo AstNode[int]()

# echo [1,2,3,4,5][1..3]
# echo [1,2,3,4,5][3..1]

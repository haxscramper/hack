type
  XXXNodeKind = enum
    xxxStr
    xxxInt
    xxxIdent
    xxxInfix

const
  xxxIntNodes = { xxxInt }
  xxxStrNodes = { xxxStr, xxxIdent }
  xxxLiteralNodes = xxxIntNodes + xxxStrNodes

type
  XXXNode = object
    case kind: XXXNodeKind
      of xxxIntNodes: intVal: int
      of xxxStrNodes: strVal: string
      else:
        subnodes: seq[XXXNode]

func newTree(kind: XXXNodeKind, subnodes: varargs[XXXNode]): XXXNode =
  result = XXXNode(kind: kind)
  if kind notin xxxLiteralNodes:
    result.subnodes = @subnodes

func newXxxNode(strVal: string): XXXNode =
  XXXNode(kind: xxxStr, strVal: strVal)

func newXxxIdent(strVal: string): XXXNode =
  XXXNode(kind: xxxIdent, strVal: strVal)

func newXxxNode(intVal: int): XXXNode =
  XXXNode(kind: xxxInt, intVal: intVal)

func lispRepr(node: XXXNode): string =
  result = ($node.kind)[3..^1]
  case node.kind:
    of xxxIntNodes: result.add " " & $node.intVal
    of xxxStrNodes: result.add " " & node.strVal
    else:
      result.add "["
      for idx, sub in node.subnodes:
        if idx > 0: result.add ", "
        result.add lispRepr(sub)

      result.add "]"

echo lispRepr(newTree(
  xxxInfix,
  newXxxIdent("-"),
  newXxxNode(12),
  newXxxNode(12)
))

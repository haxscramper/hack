type
  Token[C, L] = object
    cat: C
    lex: L

  AstKind = enum
    akList
    akToken
    akNterm

  Ast[Node; Lex; Kind] = object
    case tkind: AstKind
      of akList, akNterm:
        node: Node
        subnodes: seq[Ast[Node, Lex, Kind]]
      of akToken:
        token: Token[Kind, Lex]

  AAstKind = enum
    aakLex
    aakNode

  AAstLex = object
    case kind: AAstKind
      of aakLex:
        discard
      else:
        discard

  AAst = object
    case kind: AAstKind
      of aakNode:
        discard
      else:
        discard

  CAst = Ast[AAst, AAstLex, AAstKind]

func kind[N, L, K](aa: Ast[N, L, K]): K =
  case aa.tkind:
    of akList:
      raiseAssert("#[ IMPLEMENT ]#")
    of akNTerm:
      aa.node.kind
    of akToken:
      aa.token.cat

# func `[]`[N, L, K](aa: Ast[N, L, K]):

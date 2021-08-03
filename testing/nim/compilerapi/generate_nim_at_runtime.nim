import
  compiler/[ast, renderer, lineinfos, idents]

proc newPIdent*(str: string): PNode =
  newIdentNode(PIdent(s: str), TLineInfo())


func newPLit*(i: BiggestInt): PNode =
  newIntTypeNode(i, PType(kind: tyInt))

let node = nkForStmt.newTree(
  newPIdent("test"),
  newPIdent("other"),
  nkStmtList.newTree(
    nkCall.newTree(newPident("echo"), newPLit(12))))

echo node

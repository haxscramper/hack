type
  Wrap[N, K; S: static[set[uint8]]] = object
    item: N

  AstKind = enum
    akInt
    akString
    akFloat
    akForStmt
    akIfStmt

  Ast = object
    kind: AstKind
    strVal: string

  AstWrap[S: static[set[uint8]]] = Wrap[Ast, AstKind, S]

proc toSet*[K](kind: set[K]): set[uint8] =
  for node in kind:
    result.incl node.uint8

proc onlyLits(arg: AstWrap[toSet({akInt, akString, akFloat})]) =
  discard


onlyLits(AstWrap[toSet({akInt})]())

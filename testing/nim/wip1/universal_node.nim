import compiler/ast
import macros, sugar, macroutils

macro makeKindConverter(): untyped =
  block:
    let branches = collect(newSeq):
      for kind in TNodeKind:
        OfBranch([Ident($kind)], Ident("n" & $kind))

    var tonnk = StmtList(CaseStmt(Ident("kind"), branches))
    let kindSym = Ident("kind")

    result = quote do:
      proc toNnk(`kindSym`: TNodeKind): NimNodeKind =
        `tonnk`

  block:
    let branches = collect(newSeq):
      for kind in TNodeKind:
        OfBranch([Ident("n" & $kind)], Ident($kind))

    var fromnnk = StmtList(CaseStmt(Ident("kind"), branches))
    let kindSym = Ident("kind")

    fromnnk = quote do:
      proc fromNnk(`kindSym`: NimNodeKind): TNodeKind =
        `fromnnk`

    result = quote do:
      `result`
      `fromnnk`

makeKindConverter()

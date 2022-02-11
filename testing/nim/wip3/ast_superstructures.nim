import std/macros

macro struct(body: untyped): untyped =
  echo body.treeRepr()

struct:
  let a = 12

# StmtList
#   LetSection
#     IdentDefs
#       Ident "a"
#       Empty
#       IntLit 12

struct:
  let a, b = 12

# StmtList
#   LetSection
#     IdentDefs
#       Ident "a"
#       Ident "b"
#       Empty
#       IntLit 12

struct:
  let
    a = 1
    b = 2

# StmtList
#   LetSection
#     IdentDefs
#       Ident "a"
#       Empty
#       IntLit 1
#     IdentDefs
#       Ident "b"
#       Empty
#       IntLit 2

struct:
  type
    A = int

# StmtList
#   TypeSection
#     TypeDef
#       Ident "A"
#       Empty
#       Ident "int"


struct:
  proc a() = discard

# StmtList
#   ProcDef
#     Ident "a"
#     Empty
#     Empty
#     FormalParams
#       Empty
#     Empty
#     Empty
#     StmtList
#       DiscardStmt
#         Empty

struct:
  proc a*() = discard

# StmtList
#   ProcDef
#     Postfix
#       Ident "*"
#       Ident "a"
#     Empty
#     Empty
#     FormalParams
#       Empty
#     Empty
#     Empty
#     StmtList
#       DiscardStmt
#         Empty

struct:
  proc a*[T]() = discard

# StmtList
#   ProcDef
#     Postfix
#       Ident "*"
#       Ident "a"
#     Empty
#     GenericParams
#       IdentDefs
#         Ident "T"
#         Empty
#         Empty
#     FormalParams
#       Empty
#     Empty
#     Empty
#     StmtList
#       DiscardStmt
#         Empty

struct:
  let
    a = 1
    a* = 1
    a {.mark.} = 1
    a* {.mark.} = 1

# Ident "a"
#
# Postfix
#   Ident "*"
#   Ident "a"
#
# PragmaExpr
#   Ident "a"
#   Pragma
#     Ident "mark"
#
# PragmaExpr
#   Postfix
#     Ident "*"
#     Ident "a"
#   Pragma
#     Ident "mark"

struct:
  let
    a {.annot.}, b {.annot.} = 12

struct:
  type
    Type = object

    Type[T] = object

    Type[T] {.mark.} = object

    Type[Q] {.mark.} = object
      field: int

    Type = object
      case a: int
        of b:
          c: d

        else:
          f: g



struct:
  proc a() {.B: C, C, D, E.} = discard
  # Pragma
  #   ExprColonExpr
  #     Ident "B"
  #     Ident "C"
  #   Ident "C"
  #   Ident "D"
  #   Ident "E"

  let q {.A, B: C.} = 12
  # PragmaExpr
  #   Ident "q"
  #   Pragma
  #     Ident "A"
  #     ExprColonExpr
  #       Ident "B"
  #       Ident "C"

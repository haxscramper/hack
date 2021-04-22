import std/[macros]
import fusion/[matching]

macro typedTree(arg: typed): untyped =
  echo arg.treeRepr()
  result = arg

type
  Lex = object
    str: string
    pos: int

proc `[]`(lex: Lex, offset: int = 0): char = lex.str[lex.pos + offset]
proc `[]`(lex: Lex, ch: set[char]): bool = lex[] in ch
proc `[]`(lex: Lex, offset: int, ch: set[char]): bool = lex[offset] in ch

proc `not`[N](lex: Lex, arr: array[N, set[char]]): bool =
  for idx, value in pairs(arr):
    if lex[idx] in value:
      return false

  return true


proc `not`[N](lex: Lex, charset: set[char]): bool = lex[] in charset
proc `not`[N](lex: Lex, ch: char): bool = lex[] == ch

macro optNot*{not a}(a: bool{nkCall}): untyped =
  if a.matches(Call[
    Sym(strVal: "[]"),
    Sym(getTypeInst: Sym(strVal: "Lex")),
    .._
  ]):
    warning("Deprecated use of `not lex[]`, convert to `lex.not []`", a)

  result = nnkPragmaBlock.newTree(
    nnkPRagma.newTree(ident "noRewrite"),
    newCall("not", a)
  )



let lex = Lex(str: "1234")

typedTree:
  echo lex.not [{'0'}]

typedTree:
  echo not lex[{'0'}]

typedTree:
  {.noRewrite.}:
    echo 1

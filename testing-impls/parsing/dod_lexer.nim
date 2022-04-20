import ./dod_helpers
import std/strscans

declareIdType(Syntax)

type
  Syntax = object
    kind*: int
    start*, finish*: int

declareStoreType(Syntax, isRef = true)

type
  Lexer = object
    pos: int
    text: string
    store: SyntaxStore

proc initLexer(text: string, store: SyntaxStore = nil): Lexer =
  result.text = text
  result.store = store
  if result.store.isNil():
    new(result.store)

func next*(l: var Lexer, step: int = 1) =
  l.pos += step

func `[]`*(l: var Lexer, offset: int = 0): char =
  l.text[l.pos + offset]


template atom*(lex: Lexer; idx: int; c: char): bool =
  lex[idx] == c

template atom*(input: Lexer; idx: int; s: set[char]): bool =
  input[idx] in s

proc hasNxt*(lex: Lexer; idx: int): bool =
  let pos = lex.pos + idx
  return 0 <= pos and pos < lex.text.len

proc finished*(str: Lexer): bool =
  not str.hasNxt(0)

template scan*[E](lex: var Lexer, newKind: E, body: untyped): SyntaxId =
  let start = lex.pos
  block:
    body
  let finish = lex.pos
  lex.store.add Syntax(start: start, finish: finish, kind: newKind.int)

template tryScan*[E](lex: var Lexer, newKind: E, body: untyped): SyntaxId =
  let start = lex.pos
  if (body):
    let finish = lex.pos
    lex.store.add Syntax(start: start, finish: finish, kind: newKind.int)

  else:
    lex.pos = start
    EmptySyntaxId

type
  TokenKind = enum
    tkInt
    tkString
    tkIdent

  NodeKind = enum
    nkInt
    nkString
    nkIdent
    nkCall

proc scanDigits*[I](lex: var Lexer, iId: I): SyntaxId =
  lex.scan(iId):
    while lex[] in {'0' .. '9'}:
      lex.next()

import std/re

proc scanRe*[I](lex: var Lexer, iId: I, rx: Regex): SyntaxId =
  lex.tryScan(iId):
    let len = matchLen(lex.text.cstring, rx, lex.pos, lex.text.len)
    if len == -1:
      false

    else:
      lex.next(len - 1)
      true

block:
  var l = initLexer("1234")
  discard l.scanRe(nkInt, re"123")

  echo l.store[]

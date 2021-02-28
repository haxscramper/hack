import std/[strformat, strutils, unittest, algorithm]
import fusion/[matching]
import hmisc/[base_errors, hdebug_misc]

startHax()

{.experimental: "caseStmtMacros".}

type
  SKind = enum
    skAtom
    skCons

  S = object
    ch: char
    case kind*: SKind
      of skCons:
        sub: seq[S]

      else:
        discard

  TokenKind = enum
    tkAtom
    tkOp
    tkEof


  Token = object
    case kind*: TokenKind:
      of tkAtom, tkOp:
        ch: char

      else:
        discard

  Lexer = object
    tokens: seq[Token]


proc `$`(s: S): string =
  case s.kind:
    of skAtom:
      result = $s.ch

    of skCons:
      result = "(" & $s.ch
      for sub in s.sub:
        result &= " " & $sub

      result &= ")"


proc initLexer(str: string): Lexer =
  for ch in str:
    case ch:
      of IdentChars:
        result.tokens.add Token(kind: tkAtom, ch: ch)

      of Whitespace:
        discard

      else:
        result.tokens.add Token(kind: tkOp, ch: ch)

  reverse(result.tokens)

proc next(lex: var Lexer): Token =
  if lex.tokens.len > 0:
    lex.tokens.pop

  else:
    Token(kind: tkEof)

proc peek(lex: var Lexer): Token =
  if lex.tokens.len > 0:
    lex.tokens[^1]

  else:
    Token(kind: tkEof)


proc prefix_binding_power(op: char): int =
  case op:
    of '+', '-': 9
    else: raiseImplementError("")

proc postfix_binding_power(op: char): Option[int] =
  case op:
    of '!': some(11)
    of '[': some(11)
    else: none(int)

proc infix_binding_power(op: char): Option[(int, int)] =
  case op:
    of '=': some (2, 1)
    of '?': some (4, 3)
    of '+', '-': some (5, 6)
    of '*', '/': some (7, 8)
    of '.': some (14, 13)
    else: none (int, int)


proc exprBp(lexer: var Lexer, min_bp: int): S =
  var lhs = case lexer.next():
    of Atom(ch: @ch):
      S(kind: skAtom, ch: ch)

    of Op(ch: '('):
      let lhs = exprBp(lexer, 0)
      assertMatch(lexer.next(), Op(ch: ')'))
      lhs

    of Op(ch: @op):
      let r_bp = prefix_binding_power(op);
      let rhs = expr_bp(lexer, r_bp);
      S(kind: skCons, ch: op, sub: @[rhs])

    else:
      raiseImplementError("")

  while true:
    var op: char
    case lexer.peek():
      of Eof():
        break

      of Op(ch: @opCh):
        op = opCh

      else: raiseImplementError($lexer.peek())

    if Some(@l_bp) ?= postfix_binding_power(op):
      if l_bp < min_bp:
        break

      discard lexer.next()

      if op == '[':
        let rhs = expr_bp(lexer, 0)
        discard lexer.next()
        lhs = S(kind: skCons, ch: op, sub: @[lhs, rhs])

      else:
        lhs = S(kind: skCons, ch: op, sub: @[lhs])

      continue

    if Some((@l_bp, @r_bp)) ?= infix_binding_power(op):
      if l_bp < min_bp:
          break

      discard lexer.next()

      if op == '?':
        let mhs = expr_bp(lexer, 0);
        discard lexer.next()
        let rhs = expr_bp(lexer, r_bp);
        lhs = S(kind: skCons, ch: op, sub: @[lhs, mhs, rhs])

      else:
        let rhs = expr_bp(lexer, r_bp);
        lhs = S(kind: skCons, ch: op, sub: @[lhs, rhs])

      continue

    break

  return lhs

proc expr(str: string): S =
  var lexer = initLexer(str)
  return exprBp(lexer, 0)

import hpprint

when isMainModule:
  check $expr("1") == "1"
  check $expr("1 + 2 * 3") == "(+ 1 (* 2 3))"
  check $expr("a + b * c * d + e") == "(+ (+ a (* (* b c) d)) e)"
  check $expr("f . g . h") == "(. f (. g h))"
  check $expr("1 + 2 + f . g . h * 3 * 4") == "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))"
  check $expr("--1 * 2") == "(* (- (- 1)) 2)"
  check $expr("--f . g") == "(- (- (. f g)))"
  check $expr("-9!") == "(- (! 9))"
  check $expr("f . g !") == "(! (. f g))"
  check $expr("(((0)))") == "0"
  check $expr("x[0][1]") == "([ ([ x 0) 1)"
  check $expr("a ? b : c ? d : e") == "(? a b (? c d e))"
  check $expr("a = 0 ? b : c = d") == "(= a (= (? 0 b c) d))"

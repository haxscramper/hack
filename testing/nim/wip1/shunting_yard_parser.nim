import std/[strutils, sequtils, strformat]

# Nim code is mostly adapted from
# https://rosettacode.org/wiki/Parsing/Shunting-yard_algorithm#Nim Ast
# construction added using logic from
# https://www.klittlepage.com/2013/12/22/twelve-days-2013-shunting-yard-algorithm/

type
  Lexer = object
    buf: seq[string]
    idx: int


func `[]`(l: Lexer, idx: int = 0): string =
  result = l.buf[l.idx + idx]

func next(l: var Lexer) = inc l.idx
func finished(l: Lexer): bool = l.idx == l.buf.len
func pop(l: var Lexer): string =
  result = l[]
  l.next

type
  Ast = object
    head: string
    subn: seq[Ast]

  TokKind = enum
    tkOpen ## Open group in expression
    tkClose ## Close group in expression
    tkOp ## Operator token
    tkValue ## Start of value
    tkExprEnd ## End expression parsing before lexer is explicitly
              ## finished

  ClassifyCb = proc(tk: string): TokKind
  TokGroup = object
    case isToken: bool
      of true:
        tok: string

      of false:
        groups: seq[TokGroup]

func initTokGroup(groups: seq[TokGroup]): TokGroup =
  TokGroup(isToken: false, groups: groups)

func initTokGroup(tok: string): TokGroup =
  TokGroup(isToken: true, tok: tok)

func ptree(ast: Ast, level: int = 0): void =
  let pref = "  ".repeat(level)
  debugecho pref, ast.head
  for sn in ast.subn:
    ptree(sn, level + 1)

func `$`(group: TokGroup): string =
  if group.isToken:
    group.tok

  else:
    "〈" & group.groups.mapIt($it).join(" ") & "〉"

func addBinaryNode*(st: var seq[Ast], op: string) =
  st.add Ast(subn: @[st.pop, st.pop], head: op)

func addUnaryNode*(st: var seq[Ast], op: string) =
  st.add Ast(subn: @[st.pop], head: op)

func isAtPrefixOp*(lex: Lexer, classify: proc(tk: string): TokKind): bool =
  classify(lex[]) == tkOp and
  (lex.idx == 0 or classify(lex[-1]) in {tkOp, tkOpen})

func isAtPostfixOp*(lex: Lexer, classify: proc(tk: string): TokKind): bool =
  classify(lex[]) == tkOp and
  classify(lex[+1]) in {tkClose}

proc toGroups(lex: var Lexer, classify: ClassifyCb): seq[TokGroup] =
  echo lex[]
  if isAtPrefixOp(lex, classify):
    let prefix = lex.pop()
    # if classify(lex[]) == tkOpen:
    result.add initTokGroup(@[
      initTokGroup(prefix),
      initTokGroup(toGroups(lex, classify))
    ])

  elif classify(lex[]) == tkOpen:
    lex.next()
    while classify(lex[]) != tkClose:
      result.add toGroups(lex, classify)

    lex.next()

  elif classify(lex[]) == tkValue:
    result.add initTokGroup(lex.pop())

  elif classify(lex[]) == tkOp:
    result.add initTokGroup(lex.pop())
    result.add toGroups(lex, classify)


proc shuntAst(
    lex: var Lexer,
    classify: proc(tk: string): TokKind,
    prec: proc(op1: string): int,
    isRassoc: proc(op: string): bool,
    parseValue: proc(lex: var Lexer): Ast
  ): Ast =
  ## :parseValue: Custom parser logic for syntax parts that are not
  ##   expressions. When encountering `tkValue` elemen in token stream
  ##   control loop calls `parseValue` callback which can move lexer
  ##   as far ahead as necessary.
  ## :classify: Determine kind of token at current position;
  ## :prec: Get precedence value for operator
  ## :isRassoc: Whether or not operator is right-associative
  var operatorStack: seq[string]
  var operandStack: seq[Ast]
  while not lex.finished:
    let token = lex[]
    echo &"{token} ; {operatorStack} ; {operandStack.len()}"
    case classify(token)
      of tkOpen:
        # If it is an opening parenthesis, push it onto the stack
        operatorStack.add token
        lex.next

      of tkClose:
        # If it is a closing parenthesis, repeatedly pop operators from the
        # stack into the output stream until an opening parenthesis is
        # encountered. Pop the opening parenthesis off the stack, but do
        # not emit it into the output stream.
        while operatorStack.len > 0:
          let op = operatorStack.pop()
          if classify(op) == tkOpen:
            break

          operandStack.addBinaryNode(op)

        lex.next

      of tkOp:
        if isAtPrefixOp(lex, classify):
          echo "At prefix op"
          operatorStack.add token
          lex.next

        else:
          while operatorStack.len > 0:
            let op = operatorStack[^1]
            if classify(op) != tkOp:
              break

            if prec(token) > prec(op) or
              (isRassoc(token) and prec(token) == prec(op)):
              break
            discard operatorStack.pop()
            operandStack.addBinaryNode(op)
          operatorStack.add token
          lex.next

      of tkValue:
        operandStack.add parseValue(lex)

      of tkExprEnd:
        break

  while operatorStack.len > 0:
    if operatorStack.len > 1:
      operandStack.addBinaryNode(operatorStack.pop)

    else:
      operandStack.addUnaryNode(operatorStack.pop)

  return operandStack.pop


func classify(tk: string): TokKind =
  case tk[0]:
    of '(', '[', '{': tkOpen
    of ')', ']', '}': tkClose
    of '0' .. '9': tkValue
    of '*', '+', '-', '~', '^', '/': tkOp
    # of '[', '{': tkValue
    else: tkValue

func prec(op: string): int =
  case op[0]:
    of '^': 4
    of '*', '/': 3
    of '+', '-': 2
    else: 0

func isRassoc(op: string): bool =
  (op == "^")

func parseValue(lex: var Lexer): Ast =
  let tok = lex[]
  if tok[0] in {'0' .. '9'}:
    lex.next
    return Ast(head: tok)
  elif tok[0] in {'[', '{'}:
    let op = if tok == "[": "]" else: "}"
    result = Ast(head: tok & op)
    lex.next
    while lex[] != op:
      result.subn.add parseValue(lex)

    lex.next

  else:
    raiseAssert(&"Invalid value: {lex[]}")

# let input = "~ 3 + 4 * 2 / ( 1 - 5 ^ ~ 8 ) ^ 2 ^ [ 3 4 5 ]"
let input = "~ 3 + 3 * 8 ^ 7"
var lex = Lexer(buf: input.strip.split)
var groups: seq[TokGroup]
# while not lex.finished():
#   groups.add initTokGroup(lex.toGroups(classify))

# echo groups

# if false:
ptree shuntAst(
  lex,
  classify = classify,
  isRassoc = isRassoc,
  parseValue = parseValue,
  prec = prec
)

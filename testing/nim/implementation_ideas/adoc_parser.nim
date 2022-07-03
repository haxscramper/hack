import std/[strutils, parseutils, strformat, sequtils]
import hmisc/core/[all]
startHax()

type
  AdocTokenKind = enum
    akPlainBoldOpen
    akPlainBoldClose
    akInlineBold
    akPlainItalicOpen
    akPlainItalicClose
    akInlineItalic
    akPlainMonospaceOpen
    akPlainMonospaceClose
    akPlainMonospace
    akHighlight
    akHeadingPrefix
    akSpace
    akWord
    akIdent
    akColon
    akRaw
    akDoubleColon
    akListColon ## Trailing token for the delimiter list
    akCustomRoleStart
    akCustomRoleEnd
    akNewline
    akAngledReference
    akCallout
    akTableDelimiter
    akTablePipe
    akPlusJoin
    akPlaceholder
    akBlockDelimiter
    akPlassDelimiter
    akCaptionStart
    akDotListStart
    # akSuperScriptStart
    # akSuperScriptEnd
    # akSubScriptStart
    # akSubScriptEnd
    akBracketOpen
    akBracketClose
    akBracketArgKey
    akBracketArgValue
    akBracketArg

  AdocToken = object
    kind*: AdocTokenKind
    slice*: Slice[int]

  AdocNodeKind = enum
    anNone
    anParagraph
    anEmpty
    anHeading
    anBold
    anWord
    anItalic
    anHighlight
    anBlock
    anSourceBlock
    anTable
    anRow
    anBracket
    anCaption
    anParam ## Either single parameter or two tokens - start and end
    anSpace
    anUndecided ## Temporary node kind, added into the tree when 'maybe'
                ## node is used

  AdocNodeTuple = tuple[left, right: AdocNodeIdx]
  AdocNodeIdx = distinct int
  AdocNode = object
    kind: AdocNodeKind
    left, right: AdocNodeIdx ## Depending on the node kind these values can be
    ## interpreted as (1) token index, (2) start and end of the subnode
    ## range.

  AdocParser = object
    l*: AdocLexer
    nodes*: seq[AdocNode]
    backpatchStack*: seq[tuple[idx: int, alts: set[AdocNodeKind]]]
    pos*: int

  AdocLexer* = object
    pos*: int
    tokens*: seq[AdocToken]
    str*: string

func node*(kind: AdocNodeKind): AdocNode = AdocNode(kind: kind)

iterator items*(lexer: AdocLexer, slice: Slice[int]): (int, AdocToken) =
  for idx in slice:
    yield (idx, lexer.tokens[idx])


func maybeNode*(p: var AdocParser, alts: set[AdocNodeKind]) =
  ## Create new 'undecided' node in the parser and push it's information on
  ## the backpatch stack.
  p.backpatchStack.add((p.nodes.len, alts))
  p.nodes.add node(anUndecided)

func maybeNode*(p: var AdocParser, alt: AdocNodeKind) =
  p.maybeNode({alt})

func clarifyLast*(p: var AdocParser, alts: set[AdocNodeKind]) =
  ## Narrow down the set of possible node kinds that are expected for
  ## backpatch node.
  p.backpatchStack[^1].alts = alts

func finalizeLast*(p: var AdocParser, kind: AdocNodeKind) =
  ## Finalize the last backpatch node in the parser: pop it from the
  ## 'undecided' stack and replace it with the concrete node.
  let last = p.backpatchStack.pop()
  assert kind in last.alts
  p.nodes[last.idx] = AdocNode(
    kind: kind,
    left: AdocNodeIdx(last.idx + 1),
    right: AdocNodeIdx(p.nodes.high))

func token*(kind: AdocTokenKind, slice: Slice[int]): AdocToken =
  AdocToken(kind: kind, slice: slice)

func add*(l: var AdocLexer, token: AdocToken) =
  l.tokens.add token



func `[]`*(p: var AdocParser, idx: AdocNodeIdx): var AdocNode =
  p.nodes[idx.int]

func `[]`*(p: AdocParser, diff: int = 0): AdocToken = p.l.tokens[p.pos + diff]
func `[]`*(p: AdocParser, kind: AdocTokenKind): bool = p[].kind == kind
func atEnd*(p: AdocParser): bool = p.l.tokens.len <= p.pos
func next*(p: var AdocParser) = inc p.pos
func skip*(p: var AdocParser, kind: AdocTokenKind) =
  assert p[].kind == kind, $p[].kind
  p.next()

func skip*(p: var AdocParser, kind: set[AdocTokenKind]) =
  assert p[].kind in kind, $p[].kind
  p.next()

func hasBehind*(l: AdocParser, skippable, chars: set[AdocTokenKind]): bool =
  var idx = -1
  while (-l.pos + 1) < idx and l[idx].kind in skippable:
    dec idx

  return (-l.pos + 1) < idx and l[idx].kind in chars


  
func hasAhead*(l: AdocParser, skippable, chars: set[AdocTokenKind]): bool =
  var idx = 1
  while l[idx].kind in skippable:
    inc idx

  return l[idx].kind in chars

func `[]`*(l: AdocLexer, b: int = 0): char = l.str[l.pos + b]
func `[]`*(l: AdocLexer, slice: Slice[int]): string =
  l.str[l.pos + slice.a .. l.pos + slice.b]

func `[]`*(l: AdocLexer, ch: char): bool = l[] == ch
func `[]`*(l: AdocLexer, chars: set[char]): bool = l[] in chars
func `[]`*(l: AdocLexer, chars1, chars2: set[char]): bool =
  l[+0] in chars1 and l[+1] in chars2

func `[]`*(l: AdocLexer, c1, c2: char): bool =
  l[+0] == c1 and l[+1] == c2

func atEnd*(l: AdocLexer): bool = l.str.len <= l.pos
func hasNext*(l: AdocLexer, pos: int): bool =
  l.pos + pos < l.str.len

func `[]`*(l: AdocLexer, t: AdocToken): string = l.str[t.slice]
func `[]`*(l: AdocLexer, str: string): bool =
  var idx = 0
  result = true
  while idx < str.len and l.hasNext(idx):
    if l[idx] != str[idx]:
      return false

    inc idx





func `$`*(l: AdocToken): string =
  &"{substr($l.kind, 2)}: {l.slice.a}..{l.slice.b}"

func `$`*(l: AdocLexer, tok: AdocToken): string = l[tok] & ":" & $tok

func `$`*(node: AdocNode): string =
  &"{substr($node.kind, 2)}: {node.left.int}..{node.right.int}"

func `$`*(p: AdocParser): string = p.l $ p[]

func fail*(l: AdocLexer) {.noreturn.} =
  assert false, &"l: '{l[]}', p: {l.pos}, s: '{l.str[l.pos ..< min(l.str.len, l.pos + 5)]}'"

func fail*(p: AdocParser) {.noreturn.} =
  assert false, $p[]

func hasIdx*(l: AdocLexer, idx: int): bool =
  let real = l.pos + idx
  return 0 <= real and real <= l.str.high


func hasBehind*(l: AdocLexer, skippable, chars: set[char]): bool =
  var idx = -1
  echov l.pos
  assert globalTick() < 400
  while l.hasIdx(idx) and l[idx] in skippable:
    dec idx

  return l.hasIdx(idx) and l[idx] in chars

func atBol*(l: AdocLexer): bool =
  hasBehind(l, {}, {'\n'}) or l.pos == 0

func hasAhead*(l: AdocLexer, skippable, chars: set[char]): bool =
  var idx = 1
  while l[idx] in skippable:
    inc idx

  return l[idx] in chars


func next*(l: var AdocLexer, change: int = 1) =
  l.pos += change


func token*(l: var AdocLexer, kind: AdocTokenKind, final: int) =
  l.add token(kind, l.pos ..< l.pos + final)
  l.next(final)

template popToken*(l: var AdocLexer, kind: AdocTokenKind, body: untyped): untyped =
  block:
    let start = l.pos
    body
    let final = l.pos
    token(kind, start ..< final)

template addToken*(l: var AdocLexer, kind: AdocTokenKind, body: untyped): untyped =
  l.add popToken(l, kind, body)

const
  EmphChars = {'*', '_', '`'}
  NonWordChars = {' ', '.'}

proc lex*(l: var AdocLexer) =
  while not l.atEnd():
    case l[]:
      of IdentChars:
        var token = popToken(l, akWord):
          while l[] in IdentChars + {'\'', '-'}:
            l.next()

        if l[] == ':' and l[token].allIt(it in IdentStartChars):
          if l.hasAhead({':'}, Whitespace):
            l.add token
            addToken(l, akListColon):
              while l[] in {':'}:
                l.next()

          else:
            token.kind = akIdent
            l.add token

            if l[':', ':']:
              l.token(akDoubleColon, 2)

            else:
              l.token(akColon, 1)

            addToken(l, akRaw):
              while l[] notin {' ', '['}:
                l.next()

        else:
          l.add token

      of '[':
        l.token(akBracketOpen, 1)

        while l[] notin {']'}:
          addToken(l, akBracketArg):
            while l[] notin {']', ','}:
              l.next()

          if l[] == ',':
            l.next()

        l.token(akBracketClose, 1)

      of '{':
        addToken(l, akPlaceholder):
          while l[] notin {'}'}:
            l.next()

          l.next()

      of ' ':
        addToken(l, akSpace):
          while l[] == ' ':
            l.next()

      of '?', ',':
        l.token(akWord, 1)

      of '.':
        if l.atBol():
          if l[{'.'}, Whitespace + {'.'}]:
            addToken(l, akDotListStart):
              while l['.']:
                l.next()

          else:
            l.token(akCaptionStart, 1)

        else:
          l.token(akWord, 1)

      of '<':
        if l[+1] in {'<'}:
          addToken(l, akAngledReference):
            while not l[{'>'}, {'>'}]:
              l.next()

            l.next(2)

        elif l[+1] in Digits:
          addToken(l, akCallout):
            while not l[{'>'}]:
              l.next()

            l.next()

        else:
          l.fail()

      of '*':
        if l.hasBehind(EmphChars - {'*'}, NonWordChars):
          l.token(akPlainItalicOpen, 1)

        elif l.hasAhead(EmphChars - {'*'}, NonWordChars):
          l.token(akPlainItalicClose, 1)

        else:
          l.fail()

      of '+':
        if l["++++"] and l.atBol():
          l.token(akBlockDelimiter, 4)
          addToken(l, akRaw):
            while not (l["++++"] and l.atBol()):
              l.next()

        else:
          l.token(akPlusJoin, 1)

      of '`':
        if l.hasBehind(EmphChars - {'`'}, NonWordChars):
          l.token(akPlainMonospaceOpen, 1)

        elif l.hasAhead(EmphChars - {'`'}, NonWordChars):
          l.token(akPlainMonospaceClose, 1)

        else:
          l.fail()

      of '|':
        if l[+1] == '=' and l[+2] == '=' and l[+3] == '=':
          l.token(akTableDelimiter, 4)

        else:
          l.token(akTablePipe, 1)

      of '-':
        if l["----"] and l.atBol():
          l.token(akBlockDelimiter, 4)
          addToken(l, akRaw):
            while not (l["----"] and l.atBol()) and not l.atEnd():
              l.next()

        else:
          l.fail()

      of '#':
        if l[+1] == '#':
          l.token(akHighlight, 2)

        elif l[-1] == ']':
          l.token(akCustomRoleStart, 1)

        elif l.hasAhead({}, NonWordChars):
          l.token(akCustomRoleEnd, 1)

        else:
          l.fail()

      of '\n':
        l.token(akNewline, 1)

      else:
        l.fail()


func high*(p: AdocParser): int = p.nodes.high

func node*(p: AdocParser): AdocNodeIdx =
  AdocNodeIdx(p.high())


func tokid*(p: AdocParser, shift: int = 0): AdocNodeIdx =
  AdocNodeIdx(p.pos + shift)

func add*(p: var AdocParser, node: AdocNode): AdocNodeIdx =
  p.nodes.add node
  return p.node()



func pop*(p: var AdocParser): AdocNodeIdx =
  ## Advance parser over a single token and return it as a new node index
  result = p.tokid()
  p.next()

func node*(
    p: var AdocParser,
    kind: AdocNodeKind, left: AdocNodeIdx = p.pop()): AdocNodeIdx =
  result = p.add AdocNode(kind: kind, left: left)


func node*(
    p: var AdocParser,
    kind: AdocNodeKind, left, right: AdocNodeIdx): AdocNodeIdx =
  result = p.add AdocNode(kind: kind, left: left, right: right)


proc parseBrace*(p: var AdocParser) =
  p.maybeNode(anBracket)
  p.skip akBracketOpen
  while not p[akBracketClose]:
    case p[].kind:
      of akBracketArg:
        discard p.node(anParam)

      of akBracketArgKey:
        discard p.node(anParam, p.pop(), p.pop())

      else:
        assert false, $p

  p.skip akBracketClose
  p.finalizeLast(anBracket)

proc foldParagraph*(p: var AdocParser, slice: Slice[int]) =
  # IMPLEMENT full paragraph folding
  p.maybeNode(anParagraph)
  for idx, tok in p.l.items(slice):
    case tok.kind:
      of akSpace:
        discard p.node(anSpace, AdocNodeIdx(idx))

      of akWord:
        discard p.node(anWord, AdocNodeIdx(idx))

      else:
        discard

  p.finalizeLast(anParagraph)

proc parseCaption*(p: var AdocParser) =
  p.skip akCaptionStart
  p.maybeNode(anCaption)

  var slice = p.pos..p.pos
  while not p[akNewline]:
    p.next()

  slice.b = p.pos - 1
  p.foldParagraph(slice)
  p.finalizeLast(anCaption)

proc parseBlock*(p: var AdocParser) =
  p.maybeNode(anBlock)
  p.skip(akBlockDelimiter)
  while not p[akBlockDelimiter]:
    p.next()

  p.skip(akBlockDelimiter)
  p.finalizeLast(anBlock)

proc parse*(p: var AdocParser): AdocNodeTuple =
  echo p
  case p[].kind:
    of akWord: result.left = p.node(anWord)
    of akSpace: result.left = p.node(anSpace)
    of akNewline: p.next()
    of akCaptionStart:
      p.maybeNode({anTable, anSourceBlock})
      p.parseCaption()
      p.skip(akNewline)
      if p[akBracketOpen]:
        p.clarifyLast({anTable, anSourceBlock})
        p.parseBrace()

      p.skip(akNewline)
      if p[akBlockDelimiter]:
        p.parseBlock()
        p.finalizeLast(anSourceBlock)

      else:
        p.fail()

    of akRaw: p.next()
    else:
      p.fail()

const nodeNameLen =
  block:
    var res = 0
    for node in low(AdocNodeKind) .. high(AdocNodeKind):
      res = max(res, len($node))

    res

const anTokenKinds = {
  anWord, anParam, anSpace
}

func tree*(p: AdocParser): string =
  var lines: seq[string]
  var idx = 0
  proc aux(prefix: string, level: int = 0) =
    let node = p.nodes[idx]
    let idxpref = &"[{idx:^3}]"
    proc fmtNode(node: AdocNode): string =
      center(
        substr($node.kind, 2) & &":{node.left.int}:{node.right.int}",
        nodeNameLen, '-')

    case node.kind:
      of anTokenKinds:
        lines.add &"{idxpref}{prefix}+{fmtNode(node)}+{p.l $ p.l.tokens[node.left.int]}"
        inc idx

      else:
        inc idx
        var subId = 0
        while idx in node.left.int .. node.right.int:
          if subId == 0:
            if node.kind notin anTokenKinds:
              lines.add &"{idxpref}{prefix}+{fmtNode(node)}*"

            else:
              aux(prefix & &"-{fmtNode(node)}-+-", level + 1)

          else:
            aux(prefix & repeat(" ", nodeNameLen + 2), level + 1)

          inc subId

  aux("", 0)

  return lines.join("\n")

func lexer*(str: string): AdocLexer =
  AdocLexer(str: str)

func parser*(l: AdocLexer): AdocParser =
  AdocParser(l: l)

var l = lexer("""
.Some Ruby code
[source,ruby]
----
require 'sinatra'
----
""")

echo "executing lexer"
l.lex()
echo "done"
for tok in l.tokens:
  echo l $ tok

var p = parser(l)
while not p.atEnd():
  discard p.parse()

echo p.tree()


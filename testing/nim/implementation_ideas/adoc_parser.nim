import std/[strutils, parseutils, strformat, sequtils]

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
    # akSuperScriptStart
    # akSuperScriptEnd
    # akSubScriptStart
    # akSubScriptEnd
    akBraces

  AdocToken = object
    kind*: AdocTokenKind
    slice*: Slice[int]

  AdocLexer* = object
    pos*: int
    tokens*: seq[AdocToken]
    str*: string

func token*(kind: AdocTokenKind, slice: Slice[int]): AdocToken =
  AdocToken(kind: kind, slice: slice)

func add*(l: var AdocLexer, token: AdocToken) =
  l.tokens.add token

func `[]`*(l: AdocLexer, b: int = 0): char = l.str[l.pos + b]
func `[]`*(l: AdocLexer, slice: Slice[int]): string =
  l.str[l.pos + slice.a .. l.pos + slice.b]

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

func fail*(l: AdocLexer) =
  assert false, &"l: '{l[]}', p: {l.pos}, s: '{l.str[l.pos ..< min(l.str.len, l.pos + 5)]}'"

func hasBehind*(l: AdocLexer, skippable, chars: set[char]): bool =
  var idx = -1
  while (-l.pos + 1) < idx and l[idx] in skippable:
    dec idx

  return (-l.pos + 1) < idx and l[idx] in chars

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
        addToken(l, akBraces):
          while l[] notin {']'}:
            l.next()

          l.next()

      of '{':
        addToken(l, akPlaceholder):
          while l[] notin {'}'}:
            l.next()

          l.next()

      of ' ':
        addToken(l, akSpace):
          while l[] == ' ':
            l.next()

      of '.', '?', ',':
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
        if l["++++"]:
          l.token(akBlockDelimiter, 4)
          addToken(l, akRaw):
            while not l["++++"]:
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
        if l["----"]:
          l.token(akBlockDelimiter, 4)
          addToken(l, akRaw):
            while not l["----"]:
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


func lexer*(str: string): AdocLexer =
  AdocLexer(str: str)

var l = lexer("""
.Some Ruby code
[source,ruby]
----
require 'sinatra'

get '/hi' do
  "Hello World!"
end
----


""")

l.lex()

for t in l.tokens:
  echo "'", l[t], "' ", t

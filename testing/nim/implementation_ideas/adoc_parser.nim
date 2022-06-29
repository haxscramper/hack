import std/[strutils, parseutils, strformat, sequtils]

type
  AdocTokenKind = enum
    akPlainBoldOpen
    akPlainBoldClose
    akInlineBold
    akPlainItalicOpen
    akPlainItalicClose
    akInlineItalic
    akHeadingPrefix
    akTableDelimiter
    akSpace
    akWord
    akTablePipe

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

func atEnd*(l: AdocLexer): bool = l.str.len <= l.pos

func next*(l: var AdocLexer, change: int = 1) =
  debugecho l[]
  l.pos += change


func token*(l: var AdocLexer, kind: AdocTokenKind, final: int) =
  l.add token(kind, l.pos ..< l.pos + final)
  l.next(final)

template addToken*(l: var AdocLexer, kind: AdocTokenKind, body: untyped): untyped =
  block:
    let start = l.pos
    body
    let final = l.pos
    l.add token(kind, start ..< final)


proc lex*(l: var AdocLexer) =
  while not l.atEnd():
    case l[]:
      of IdentChars:
        addToken(l, akWord):
          while l[] in IdentChars:
            l.next()

      of ' ':
        l.token(akSpace, 1)

      of '*':
        if l[+1] in Whitespace: l.token(akPlainItalicClose, 1)
        elif l[-1] in Whitespace: l.token(akPlainItalicOpen, 1)

        else:
          assert false, $l[0 .. 3]

      else:
        assert false, $l[]


func lexer*(str: string): AdocLexer =
  AdocLexer(str: str)

var l = lexer("""
It has *strong* significance to me.

I _cannot_ stress this enough.

Type `OK` to accept.

That *_really_* has to go.

Can't pick one? Let's use them `*_all_*`.
""")

l.lex()

echo "ok"

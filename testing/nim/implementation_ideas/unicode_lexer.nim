import std/[lexbase, unicode, streams]

type
  Lexer = object of BaseLexer

using lexer: var Lexer

func `[]`*(lexer): Rune =
  fastRuneAt(lexer.buf, lexer.bufpos, result, doInc = false)

func advance(lexer) =
  inc lexer.bufpos, lexer.buf.runeLenAt(lexer.bufpos)

var sstream = newStringStream("Текст с не-ASCII сиволами")

var lexer: Lexer
lexer.open(sstream)

for i in 0 .. 20:
  stdout.write " '", lexer[], "' "
  lexer.advance()

{.compile: "parser.c".}
{.passl: "-ltree-sitter".}

import hts_wrapgen_1_wrapper
import hparse/htreesitter/htreesitter
import std/[strformat, strutils]


proc `[]`*(symbols: UncheckedArray[bool], idx: enum): bool =
  symbols[idx.int]

proc contains*(charset: set[char], rune: Rune): bool =
  if rune.int16 in 0 .. 255:
    result = rune.char in charset

proc `[]`*(lex: var TSLexer, charset: set[char]): bool =
  lex[] in charset



proc scan(
    scanner: var int8,
    lexer: var TSLexer,
    validSymbols: ptr UncheckedArray[bool]
  ): Option[HtsWrapgen1ExternalTok] =

  case lexer[].char:
    of '<':
      if validSymbols[htsWrapgen1ExternComment]:
        lexer.markEnd()
        var cnt = 0
        while not lexer.finished():
          if lexer['<']:
            lexer.advance()
            inc cnt

          elif lexer['>']:
            dec cnt
            lexer.advance()

            if cnt == 0:
              lexer.markEnd()
              return some(htsWrapgen1ExternComment)


          else:
            lexer.advance()

    of Whitespace:
      lexer.skip()
      var cnt = 0
      while lexer[Whitespace] and not lexer.finished():
        lexer.skip()
        inc cnt

      return some(htsWrapgen1ExternSpace)


    else:
      discard





discard tsInitScanner(hts_wrapgen_1, void)
var parser = newHtsWrapgen1Parser()


var str = """
func hello() bool {
  <<comment>>
  <comment>
  return 12;
}
"""

let tree = parser.parseString(str)
echo tree.treeRepr(str)

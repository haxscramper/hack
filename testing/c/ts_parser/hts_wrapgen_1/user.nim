{.compile: "parser.c".}
{.passl: "-ltree-sitter".}

import hts_wrapgen_1_wrapper
import hparse/htreesitter/htreesitter
import std/unicode

var parser = newHtsWrapgen1Parser()

proc `[]`*(symbols: UncheckedArray[bool], idx: enum): bool =
  symbols[idx.int]

proc scan(
    scanner: var int8,
    lexer: var TSLexer,
    validSymbols: ptr UncheckedArray[bool]
  ): Option[HtsWrapgen1ExternalTok] =


  if validSymbols[htsWrapgen1ExternComment]:
    var cnt = 0
    if not lexer['#']:
      return

    else:
      while not lexer.finished():
        if lexer['#']:
          lexer.advance()
          if lexer['[']:
            lexer.advance()
            inc cnt

        elif lexer[']']:
          lexer.advance()
          if lexer['#']:
            lexer.advance()
            dec cnt

          if cnt == 0:
            lexer.markEnd()
            return some(htsWrapgen1ExternComment)

        else:
          lexer.advance()

discard tsInitScanner(hts_wrapgen_1, void)

var str = "#[]#"
let tree = parser.parseString(str)
echo tree.treeRepr(str)

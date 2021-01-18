{.compile: "src/parser.c".}
{.passl: "-ltree-sitter".}

import hts_wrapgen_1_wrapper
import hparse/htreesitter/htreesitter


proc `[]`*(symbols: UncheckedArray[bool], idx: enum): bool =
  symbols[idx.int]

proc scan(
    scanner: var int8,
    lexer: var TSLexer,
    validSymbols: ptr UncheckedArray[bool]
  ): Option[HtsWrapgen1ExternalTok] =


  if validSymbols[htsWrapgen1ExternStr]:
    lexer.markEnd()
    var cnt = 0
    if not lexer['{']:
      return

    else:
      while not lexer.finished():
        if lexer['{']:
          lexer.advance()
          if lexer['-']:
            inc cnt
            lexer.advance()

          else:
            if cnt == 0:
              return

        elif lexer['-']:
          lexer.advance()
          if lexer['}']:
            dec cnt
            lexer.advance()

          if cnt == 0:
            lexer.markEnd()
            return some(htsWrapgen1ExternStr)

        else:
          lexer.advance()

discard tsInitScanner(hts_wrapgen_1, void)
var parser = newHtsWrapgen1Parser()


var str = "1 {-{-c-}-} 1"

let tree = parser.parseString(str)
echo tree.treeRepr(str)

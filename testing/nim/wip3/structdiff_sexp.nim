import nimsuggest/sexp
import std/strutils

func treeRepr*(node: SexpNode): string =
  proc aux(n: SexpNode, r: var string, level: int) =
    r.add repeat("  ", level)
    r.add $n.kind
    case n.kind:
      of SString:
        r.add " \""
        r.add n.str
        r.add "\""

      of SSymbol:
        r.add " "
        r.add n.symbol
        r.add " "

      of SInt:
        r.add " "
        r.add $n.num
        r.add " "

      of SFloat:
        r.add " "
        r.add $n.fnum
        r.add " "

      of SList:
        for e in n.elems:
          r.add "\n"
          aux(e, r, level + 1)

      of SCons:
        r.add "\n"
        let (car, cdr) = n.getCons()
        aux(car, r, level + 1)
        r.add "\n"
        aux(cdr, r, level + 1)

      of SNil:
        r.add "nil"

  aux(node, result, 0)

for s in [
  "(a)",
  "(Sem-TypeMismatch)"
]:
  echo s
  echo parseSexp(s).treeRepr()

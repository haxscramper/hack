import times, stats
import fusion/matching

import compiler/[
  parser, llstream, idents,
  options, pathutils, astalgo, ast
]

{.experimental: "caseStmtMacros".}

const
  nkStrKinds* = {
    nkStrLit .. nkTripleStrLit
  } ## Set of all nim node kinds for string nodes

  nkIntKinds* = {
    nkCharLit .. nkUInt64Lit
  } ## Set of all nim node kinds for integer literal nodes

  nkFloatKinds* = {
    nkFloatLit .. nkFloat128Lit
  } ## Set of all nim node kinds for float literal nodes

  nkIdentKinds* = {
    nkIdent, nkSym, nkOpenSymChoice
  } ## Set of all nim node kinds for identifier-like nodes

  nkTokenKinds* =
    nkStrKinds + nkIntKinds + nkFloatKinds +
    nkIdentKinds + {nkEmpty}
    ## Set of all token-like nodes (primitive type literals or
    ## identifiers)

proc parsePNodeStr*(str: string): PNode =
  let cache: IdentCache = newIdentCache()
  let config: ConfigRef = newConfigRef()
  var pars: Parser

  openParser(
    p = pars,
    filename = AbsoluteFile(currentSourcePath()),
    inputStream = llStreamOpen(str),
    cache = cache,
    config = config
  )

  result = parseAll(pars)
  closeParser(pars)

proc check(node: PNode) =
  case node:
    of TypeDef[PragmaExpr[@name, @pragma], _, @body]:
      if pragma.matches([any Ident(ident.s: "pure")]):
        for node in body:
          if node.kind == nkIdent and
             node.ident.s[0] notin {'A' .. 'Z'}:
            echo node.info
            echo "Bad style"

    else:
      if node.kind in nkTokenKinds:
        discard

      else:
        for subnode in node:
          check(subnode)



let node = parsePNodeStr("""
type
  Hello {.pure.} = enum
    Test
    teset23
""")

check(node)


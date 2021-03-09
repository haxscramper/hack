import times, stats

import compiler/[parser, llstream, idents,
                 options, pathutils, astalgo, ast]

#*************************************************************************#
#***************  Parser details, no need to dig too deep  ***************#
#*************************************************************************#

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

#*************************************************************************#
#************************  Actual implementation  ************************#
#*************************************************************************#

let node = parsePNodeStr("inc(12)")

for elem in node:
  if elem.kind == nkCall and elem[0].ident.s == "inc":
    echo "disruptek is angry: rewrite to `inc <arg>`"
    echo elem.info

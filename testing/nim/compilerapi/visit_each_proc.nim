import compiler/[
  parser, llstream, idents, options, pathutils, ast, lineinfos, astalgo]

import
  std/strformat

proc parsePNodeStr*(str: string): PNode =
  let cache: IdentCache = newIdentCache()
  let config: ConfigRef = newConfigRef()
  var pars: Parser

  pars.lex.errorHandler =
    proc(conf: ConfigRef; info: TLineInfo; msg: TMsgKind; arg: string) =
      if msg notin {hintLineTooLong}:
        let file = config.m.fileInfos[info.fileIndex.int32].fullPath.string
        raise newException(
          ValueError, &"{file}:{info.line}:{info.col} {arg}")

  config.verbosity = 0
  config.options.excl optHints
  openParser(
    p = pars,
    filename = AbsoluteFile(currentSourcePath()),
    inputStream = llStreamOpen(str),
    cache = cache,
    config = config
  )

  result = parseAll(pars)
  closeParser(pars)

let r = parsePNodeStr("""
proc a() = discard
proc b() = discard
""")

for node in r:
  debug node

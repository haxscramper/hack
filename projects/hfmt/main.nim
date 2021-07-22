import
  ./parser,
  ./lexer

import
  compiler/[options, idents, lineinfos, ast, pathutils, llstream]

import
  std/[strformat]

import
  hmisc/base_errors

proc parseString1*(str: string): PNode =
  let cache: IdentCache = newIdentCache()
  let config: ConfigRef = newConfigRef()
  var pars: Parser

  pars.lex.errorHandler =
    proc(conf: ConfigRef; info: TLineInfo; msg: TMsgKind; arg: string) =
      if msg notin {hintLineTooLong}:
        let file = config.m.fileInfos[info.fileIndex.int32].fullPath.string
        raise newException(
          ParseError, &"{file}:{info.line}:{info.col} {arg}")

  config.verbosity = 0
  config.options.excl optHints

  openParser(
    p = pars,
    filename = AbsoluteFile(""),
    inputStream = llStreamOpen(str),
    cache = cache,
    config = config
  )

  result = parseAll(pars)
  closeParser(pars)

import
  hnimast/hast_common

var conf = newCOnfigRef()

conf.mainPackageNotes.incl hintMsgOrigin


let node = parseString1("""
match head:
  of 1: echo 2
  of 2: echo 2
  else:
    echo "false"

doAssert (A ⊗ B) ∘ (C + D)

""")

echo node.treeRepr()

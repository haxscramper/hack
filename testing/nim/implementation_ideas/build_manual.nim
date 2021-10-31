import compiler/[
  parser, llstream, idents, options, pathutils, ast, lineinfos,
  renderer
]

import
  packages/docutils/[rstgen, rstast, rst]

import
  std/[strformat, os, strutils, strtabs, strbasics]

type NimParseError* = object of CatchableError

proc rstMessageHandler*(filename: string, line, col: int,
     msgkind: MsgKind, arg: string) =

  let mc = msgkind.whichMsgClass
  if mc == mcError:
    let a = $msgkind % arg
    var message: string
    message.add " $1: $2" % [$mc, a]
    raise newException(EParseError, message)


proc parseRstString*(
    s: string,
    inputPath: AbsoluteFile = AbsoluteFile("")
  ): PRstNode =

  const filen = "input"
  var dummyHasToc = false
  let findFile = proc(path: string): string =
    echo "find file", path

  result = rstParse(
    text     = s,
    filename = filen,
    line     = 0,
    column   = 1,
    options  = {},
    findFile = findFile,
    msgHandler = rstMessageHandler
  ).node

proc htmlFromRst*(rst: PRstNode): string =
  proc findFile(filename: string): string =
    # we don't find any files in online mode:
    result = ""

  const filen = "input"
  var d: RstGenerator
  let config = newStringTable()
  initRstGenerator(
    d, outHtml, config, filen,
    findFile, rstMessageHandler)

  result = ""
  renderRstToOut(d, rst, result)
  strbasics.strip(result)

proc parseFile*(file: AbsoluteFile, doRaise: bool = false): PNode =
  let cache: IdentCache = newIdentCache()
  let config: ConfigRef = newConfigRef()
  var pars: Parser

  pars.lex.errorHandler =
    proc(
      conf: ConfigRef;
      info: lineinfos.TLineInfo;
      msg: TMsgKind; arg: string
    ) =

      if msg notin {hintLineTooLong}:
        let file = config.m.fileInfos[info.fileIndex.int32].fullPath.string
        raise newException(
          NimParseError, &"{file}:{info.line}:{info.col} {arg}")

  config.verbosity = 0
  config.options.excl optHints

  openParser(
    p = pars,
    filename = file,
    inputStream = llStreamOpen(readFile(file.string)),
    cache = cache,
    config = config
  )

  result = parseAll(pars)
  closeParser(pars)


func `[]`*(rst: PRstNode, idx: int): PRstNode = rst.sons[idx]
func `[]`*(rst: var PRstNode, idx: int): var PRstNode = rst.sons[idx]
# func add*(rst: var PRstNode, node: PRstNode): var PRstNode = rst.sons[idx]

func newTree*(kind: RstNodeKind, text: string): PRstNode =
  result = newRstNode(kind)
  case kind:
    of rnLeaf:
      result.text = text

    else:
      assert false, $kind

proc newTree*(kind: RstNodeKind, sub: varargs[PRstNode]): PRstNode =
  result = newRstNode(kind)
  if len(sub) > 0:
    result.sons = @sub

proc treeRepr(r: PRstNode): string =
  var res = addr result
  template add(args: varargs[string, `$`]): untyped =
    for a in args:
      res[].add a

  proc aux(n: PRstNode, level: int) =
    add repeat("  ", level), ($n.kind)[2..^1]
    case n.kind:
      of rnLeaf, rnSmiley:
        let ls = n.text.split("\n")
        add " ", ls[0]
        for line in ls[1..^1]:
          add "\n"
          add repeat(" ", (level * 2) + len($n.kind) - 1)
          add line

      of rnEnumList:
          add  " ", $n.labelFmt

      of rnLineBlockItem:
          add " ", $n.lineIndent

      of rnAdmonition:
          add " ", $n.adType

      of rnOverline, rnHeadline, rnMarkdownHeadline:
          add " ", $n.level

      of rnFootnote, rnCitation, rnOptionListItem:
          add " ", $n.order

      of rnRef, rnSubstitutionReferences, rnInterpretedText,
         rnField, rnInlineCode, rnCodeBlock, rnFootnoteRef:
          add " ", $n.info

      else:
        discard

    for sub in n.sons:
      add "\n"
      aux(sub, level + 1)

  aux(r, 0)

  return res[]



proc toRst(file: AbsoluteFile, level: int = 2): PRstNode =
  proc aux(n: PNode, level: int): PRstNode =
    case n.kind:
      of nkStmtList:
        result = newRstNode(rnInner)
        for item in n:
          let res = aux(item, level)
          if not isNil(res):
            result.add res

      of nkCommentStmt:
        result = parseRstString(n.comment, file)

      of nkDiscardStmt:
        discard

      of nkBlockStmt:
        result = newRstNode(rnHeadline)
        result.level = level

        result.add newRstNode(rnLeaf)
        if n[0].kind == nkIdent:
          result[0].text = n[0].ident.s
          result.add aux(n[1], level + 1)

        else:
          result.add newTree(rnInner, aux(n[1], level))


      else:
        result = newTree(
          rnCodeBlock,
          newTree(
            rnDirArg,
            newTree(rnLeaf, "nim")),
          newTree(
            rnFieldList,
            newTree(
              rnField,
              newTree(rnFieldName, newTree(rnLeaf, "default-language")),
              newTree(rnFieldBody, newTree(rnLeaf, "Nim")))),
          newTree(
            rnLiteralBlock,
            newTree(rnLeaf, $n)))

  return parseFile(file).aux(level)


proc buildManualTree(root: AbsoluteDir): PRstNode =
  result = newRstNode(rnInner)
  for (kind, path) in walkDir(root.string):
    case kind:
      of pcDir:
        result.add buildManualTree(AbsoluteDir(path))

      of pcFile:
        let (dir, name, ext) = path.splitFile()
        if "fail" notin name:
          if ext in [".nim"]:
            result.add toRst(AbsoluteFile(path))

      else:
        discard


proc toPage(name: string, node: PRstNode) =
  writeFile(&"/tmp/{name}.html", &"""
<!DOCTYPE html>
<html>
<body>
{node.htmlFromRst()}
</body>
</html>
""")

toPage("test"):
  parseRstString("""
.. code-block:: nim
    test
    code-block
    123
""")

let node = buildManualTree(AbsoluteDir(getCurrentDir() / ".."))

toPage("page", node)

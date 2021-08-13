import
  compiler/[
    nimeval, passes, ast, astalgo, pathutils, vm, scriptconfig,
    modulegraphs, options, idents, condsyms, sem, modules, llstream,
    lineinfos, astalgo, msgs, parser, idgen
  ]


import
  std/[strutils, strformat, parseutils, os]


let stdlib* = getHomeDir() / ".choosenim/toolchains/nim-1.4.8/lib"

const
  maxPasses = 10

type
  TPassContextArray = array[0..maxPasses - 1, PPassContext]


#*************************************************************************#
#*************************  Setup module graph  **************************#
#*************************************************************************#

let searchPaths = @[
    stdlib,
    stdlib / "pure",
    stdlib / "core",
    stdlib / "pure" / "collections"
]

var conf = newConfigRef()
var cache = newIdentCache()
var graph = newModuleGraph(cache, conf)

for p in searchPaths:
  conf.searchPaths.add(AbsoluteDir p)
  if conf.libpath.isEmpty:
    conf.libpath = AbsoluteDir p

conf.cmd = cmdInteractive
conf.errorMax = high(int)
conf.structuredErrorHook =
  proc (config: ConfigRef; info: TLineInfo; msg: string; severity: Severity) =
    assert false, &"{info.line}:{info.col} {msg}"


initDefines(conf.symbols)

defineSymbol(conf.symbols, "nimscript")
defineSymbol(conf.symbols, "nimconfig")

registerPass(graph, semPass)
registerPass(graph, evalPass)

var m = graph.makeModule(AbsoluteFile"scriptname.nim")
incl(m.flags, sfMainModule)
graph.vm = setupVM(m, cache, "scriptname.nim", graph)
graph.compileSystemModule()

var a: TPassContextArray


#*************************************************************************#
#*******************  Process node from string stream  *******************#
#*************************************************************************#

# Prints `echo 12` as expected
processModule(graph, m, llStreamOpen("echo 12"))
processModule(graph, m, llStreamOpen("echo 123"))

#*************************************************************************#
#*****************  Reimplementation of the passes.nim  ******************#
#*************************************************************************#

#===========================  Directly copied  ===========================#

proc openPasses(g: ModuleGraph; a: var TPassContextArray;
                module: PSym) =
  for i in 0..<g.passes.len:
    if not isNil(g.passes[i].open):
      a[i] = g.passes[i].open(g, module)
    else: a[i] = nil

proc closePasses(graph: ModuleGraph; a: var TPassContextArray) =
  var m: PNode = nil
  for i in 0..<graph.passes.len:
    if not isNil(graph.passes[i].close): m = graph.passes[i].close(graph, a[i], m)
    a[i] = nil                # free the memory here

proc processTopLevelStmt(graph: ModuleGraph, n: PNode, a: var TPassContextArray): bool =
  # this implements the code transformation pipeline
  var m = n
  for i in 0..<graph.passes.len:
    if not isNil(graph.passes[i].process):
      m = graph.passes[i].process(a[i], m)
      if isNil(m): return false
  result = true

proc prepareConfigNotes(graph: ModuleGraph; module: PSym) =
  # don't be verbose unless the module belongs to the main package:
  if module.getnimblePkgId == graph.config.mainPackageId:
    graph.config.notes = graph.config.mainPackageNotes
  else:
    if graph.config.mainPackageNotes == {}: graph.config.mainPackageNotes = graph.config.notes
    graph.config.notes = graph.config.foreignPackageNotes

proc resolveMod(conf: ConfigRef; module, relativeTo: string): FileIndex =
  let fullPath = findModule(conf, module, relativeTo)
  if fullPath.isEmpty:
    result = InvalidFileIdx
  else:
    result = fileInfoIdx(conf, fullPath)

proc processImplicits(graph: ModuleGraph; implicits: seq[string], nodeKind: TNodeKind,
                      a: var TPassContextArray; m: PSym) =
  # XXX fixme this should actually be relative to the config file!
  let relativeTo = toFullPath(graph.config, m.info)
  for module in items(implicits):
    # implicit imports should not lead to a module importing itself
    if m.position != resolveMod(graph.config, module, relativeTo).int32:
      var importStmt = newNodeI(nodeKind, m.info)
      var str = newStrNode(nkStrLit, module)
      str.info = m.info
      importStmt.add str
      if not processTopLevelStmt(graph, importStmt, a): break

#=========================  Copied and modified  =========================#

echo "#==========================  From code string  ===========================#"

proc parseNode(graph: ModuleGraph, module: PSym, s: PllStream): PNode =
  var p: Parser
  openParser(p, module.fileIdx, s, graph.cache, graph.config)

  result = parseTopLevelStmt(p)

  closeParser(p)

proc processModule2(graph: ModuleGraph; module: PSym, s: PLLStream) =
  var a: TPassContextArray
  openPasses(graph, a, module)

  let n = parseNode(graph, module, s)

  discard processTopLevelStmt(graph, n, a)

  closePasses(graph, a)


processModule2(graph, m, llStreamOpen("echo(\"SSSSSSSSSSSSS\")"))

#*************************************************************************#
#************************  Manually process node  ************************#
#*************************************************************************#

echo "#==============================  From node  ==============================#"

const debugTree = on

when debugTree:
  import hnimast/hast_common

proc processModule3(graph: ModuleGraph; module: PSym, n: PNode) =
  var a: TPassContextArray
  openPasses(graph, a, module)

  when debugTree:
    echo treeRepr(n, lineInfo = true, positionIndexed = false)

  discard processTopLevelStmt(graph, n, a)

  closePasses(graph, a)


processModule3(graph, m,
  parseNode(graph, m, llStreamOpen("echo(\"SSSSSSSSSSSSS\")")))

processModule3(graph, m,
  nkCall.newTree(
    newIdentNode(graph.cache.getIdent("echo"), TLineInfo(line: 1, col: 0)),
    newStrNode(nkStrLit, "SSSSSSSSSSSSS")))

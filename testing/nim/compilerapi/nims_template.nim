include compiler/passes

import
  compiler/[
    nimeval, ast, astalgo, pathutils, vm, scriptconfig,
    modulegraphs, options, idents, condsyms, sem, modules, llstream,
    lineinfos, astalgo, msgs, parser, idgen
  ]

import
  std/[strutils, strformat, parseutils, os]


let stdlib* = getHomeDir() / ".choosenim/toolchains/nim-1.4.8/lib"

var
  conf = newConfigRef()
  cache = newIdentCache()
  graph = newModuleGraph(cache, conf)

conf.libpath = AbsoluteDir stdlib

for p in @[
    stdlib,
    stdlib / "pure",
    stdlib / "core",
    stdlib / "pure" / "collections"
  ]:
  conf.searchPaths.add(AbsoluteDir p)

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

proc processModule3(graph: ModuleGraph; module: PSym, n: PNode) =
  var a: TPassContextArray
  openPasses(graph, a, module)

  discard processTopLevelStmt(graph, n, a)

  closePasses(graph, a)

proc newIdent(graph: ModuleGraph, name: string): PNode =
  newIdentNode(graph.cache.getIdent("echo"), TLineInfo())

processModule3(graph, m,
  nkCall.newTree(
    graph.newIdent("echo"),
    newStrNode(nkStrLit, "SSSSSSSSSSSSS")))

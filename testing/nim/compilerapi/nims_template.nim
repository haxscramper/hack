include compiler/passes

import
  compiler/[
    ast, pathutils, vm, scriptconfig,
    modulegraphs, options, idents, condsyms, sem, modules,
    lineinfos, astalgo, vmdef, vmconv
  ]

import
  std/[strutils, strformat, os, macros]


let stdlib* = getHomeDir() / ".choosenim/toolchains/nim-1.4.8/lib"

var
  conf = newConfigRef()
  cache = newIdentCache()
  graph = newModuleGraph(cache, conf)

conf.libpath = AbsoluteDir stdlib

for p in @[
    stdlib,
    stdlib / "pure",
    stdlib / "pure" / "collections",
    stdlib / "pure" / "concurrency",
    stdlib / "impure",
    stdlib / "js",
    stdlib / "packages" / "docutils",
    stdlib / "std",
    stdlib / "core",
    stdlib / "posix",
    stdlib / "windows",
    stdlib / "wrappers",
    stdlib / "wrappers" / "linenoise"
  ]:
  conf.searchPaths.add(AbsoluteDir p)

conf.cmd = cmdInteractive
conf.errorMax = high(int)
conf.structuredErrorHook =
  proc (config: ConfigRef; info: TLineInfo; msg: string; severity: Severity) =
    echo &"{info.line}:{info.col} {msg}"
    assert false
    # if not (
    #   "instantiation from here" in msg
    #   # "type mismatch" in msg
    # ):
    #   assert false


initDefines(conf.symbols)

defineSymbol(conf.symbols, "nimscript")
defineSymbol(conf.symbols, "nimconfig")

registerPass(graph, semPass)
registerPass(graph, evalPass)

var m = graph.makeModule(AbsoluteFile"scriptname.nim")
incl(m.flags, sfMainModule)
graph.vm = setupVM(m, cache, "scriptname.nim", graph)
graph.compileSystemModule()

proc processModule3(
    graph: ModuleGraph; module: PSym, n: PNode, a: var TPassContextArray) =
  discard processTopLevelStmt(graph, n, a)

proc getIdent(graph: ModuleGraph, name: string): PNode =
  newIdentNode(graph.cache.getIdent(name), TLineInfo())


graph.vm.PEvalContext().registerCallback(
  "customProc",
  proc(args: VmArgs) =
    echo "Called custom proc with arg [", args.getString(0), "]"
)

proc empty(): PNode = nkEmpty.newTree()


macro pnodeGenRepr*(node: untyped) =
  proc aux(n: NimNode, res: var string, level: int) =
    res.add "  ".repeat(level)
    case n.kind:
      of nnkIdent:
        res.add "graph.getIdent(\""
        res.add n.strVal()
        res.add "\")"

      of nnkCharLit .. nnkUInt64Lit:
        res.add "newIntNode("
        res.add ($n.kind)[1 .. ^1]
        res.add ", "
        res.add $n.intVal()
        res.add ")"

      of nnkFloatLit .. nnkFloat128Lit:
        res.add "newFloatNode"
        res.add $n.floatVal()
        res.add ", "
        res.add ($n.kind)[1 .. ^1]
        res.add ")"

      of nnkStrLit .. nnkTripleStrLit:
        res.add "newStrNode(\""
        res.add n.strVal()
        res.add "\", "
        res.add ($n.kind)[1 .. ^1]
        res.add ")"

      of nnkEmpty:
        res.add "nkEmpty.newTree()"

      else:
        res.add ($n.kind)[1..^1]
        res.add ".newTree(\n"
        for idx, sub in n:
          aux(sub, res, level + 1)
          if idx < n.len - 1:
            res.add ",\n"

        res.add ")"

  var res: string
  aux(node, res, 1)
  echo res

proc parseNode(graph: ModuleGraph, module: PSym, s: PllStream): PNode =
  var p: Parser
  openParser(p, module.fileIdx, s, graph.cache, graph.config)

  result = parseTopLevelStmt(p)

  closeParser(p)

proc processModule2(
    graph: ModuleGraph; module: PSym,
    s: PLLStream, a: var TPassContextArray) =
  var
    p: Parser
    fileIdx = module.fileIdx

  openParser(p, fileIdx, s, graph.cache, graph.config)

  while true:
    if graph.stopCompile(): break
    var n = parseTopLevelStmt(p)
    if n.kind == nkEmpty: break
    if n.kind in imperativeCode:
      # read everything until the next proc declaration etc.
      var sl = newNodeI(nkStmtList, n.info)
      sl.add n
      var rest: PNode = nil
      while true:
        var n = parseTopLevelStmt(p)
        if n.kind == nkEmpty or n.kind notin imperativeCode:
          rest = n
          break
        sl.add n
      if not processTopLevelStmt(graph, sl, a): break
      if rest != nil:
        if not processTopLevelStmt(graph, rest, a): break

    else:
      if not processTopLevelStmt(graph, n, a): break

var a: TPassContextArray
openPasses(graph, a, m)

processModule2(graph, m, llStreamOpen("""
echo "Evaluated thing with template"
echo "second statement"

template testTemplate(): untyped =
  echo "expanded template"

template templateExecutedInVm*(): untyped =
  result &= "example"   # <-  vm
  echo "RESULT: ", result
  echo "IN macroexpanded code ii: ", ii, " ss: ", ss
  if ii == 1:   # <-  vm
    echo "Added to result"
    result &= ss   # <-  vm
    echo "RESULT: ", result
  var myvar = 1   # <-  vm
  inc(myvar, 1)   # <-  vm

testTemplate()

proc setOutResult[T](arg: T) = discard

"""), a)

processModule2(graph, m, llStreamOpen("testTemplate()"), a)


proc runtimeImplBody(ss: string, ii: int): string =
  var fromVm: string

  graph.vm.PEvalContext().registerCallback(
    "setOutResult",
    proc(args: VmArgs) =
      echo "Setting value from the proc implementation"
      fromVm = args.getString(0)
      echo "Accepted value from the VM")

  processModule3(graph, m,
    nkBlockStmt.newTree(nkEmpty.newTree(),
      nkStmtList.newTree(
        # Pass arguments to the environemtn
        nkLetSection.newTree(
          nkIdentDefs.newTree(graph.getIdent("ss"), nkEmpty.newTree(), vmconv.toLit(ss)),
          nkIdentDefs.newTree(graph.getIdent("ii"), nkEmpty.newTree(), vmconv.toLit(ii))),

        # Prepare pseudo-function environment
        nkVarSection.newTree(
          nkIdentDefs.newTree(
            graph.getIdent("result"),
            graph.getIdent("string"),
            nkEmpty.newTree())),

        # # Execute macro call
        # nkCall.newTree(graph.getIdent("templateExecutedInVm")),
        parseNode(graph, m, llStreamOpen("templateExecutedInVm()")),

        # Actually pass result to compiled part
        nkCall.newTree(
          graph.getIdent("setOutResult"),
          graph.getIdent("result")))), a)


  return fromVm

echo "-------- FIRST CALL ------------"
echo "[== ", runtimeImplBody("123", 12), " ==]"

echo "-------- SECOND CALL ------------"
echo "[== ", runtimeImplBody("123", 1), " ==]"

closePasses(graph, a)

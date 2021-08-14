include compiler/passes

import
  compiler/[
    ast, pathutils, vm, scriptconfig,
    modulegraphs, options, idents, condsyms, sem, modules,
    lineinfos, astalgo, parser, vmdef
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

      else:
        res.add ($n.kind)[1..^1]
        res.add ".newTree(\n"
        for idx, sub in n:
          aux(sub, res, level + 1)
          if idx < n.len - 1:
            res.add ",\n"

          # else:
          #   res.add "\n"

        # res.add "  ".repeat(level)
        res.add ")"

  var res: string
  aux(node, res, 1)
  echo res

processModule3(graph, m,
  nkStmtList.newTree(
    nkProcDef.newTree(
      graph.getIdent("customProc"),
      empty(),
      empty(),
      nkFormalParams.newTree(
        empty(),
        nkIdentDefs.newTree(graph.getIdent("arg"), graph.getIdent("string"), empty())),
      empty(),
      empty(),
      nkStmtList.newTree(nkDiscardStmt.newTree(empty()))),
  nkCall.newTree(graph.getIdent("customProc"), newStrNode(nkStrLit, "SSSSSSSSSSSSS"))))


pnodeGenRepr:
  for i in 0 .. 12:
    echo i

processModule3(graph, m,
  nkStmtList.newTree(
    nkForStmt.newTree(
      graph.getIdent("i"),
      nkInfix.newTree(
        graph.getIdent(".."),
        newIntNode(nkIntLit, 0),
        newIntNode(nkIntLit, 12)),
      nkStmtList.newTree(
        nkCommand.newTree(
          graph.getIdent("echo"),
          graph.getIdent("i"))))))

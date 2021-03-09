import hmisc/other/oswrap
import std/deques
import compiler/[
  ast, astalgo, modules, passes, condsyms,
  options, sem, semdata, llstream, vm, vmdef,
  modulegraphs, idents, pathutils, passaux,
  scriptconfig
]

import common

let searchPaths = toSeqString(@[
    stdlib,
    stdlib / "pure",
    stdlib / "core",
    stdlib / "pure" / "collections"
])


proc newGraphStdin*(
  withEval: bool = true): tuple[graph: ModuleGraph, m: PSym] =
  var conf = newConfigRef()
  var cache = newIdentCache()
  var graph = newModuleGraph(cache, conf)

  for p in searchPaths:
    conf.searchPaths.add(AbsoluteDir p)
    if conf.libpath.isEmpty:
      conf.libpath = AbsoluteDir p

  conf.cmd = cmdInteractive

  conf.errorMax = high(int)

  initDefines(conf.symbols)

  defineSymbol(conf.symbols, "nimscript")
  defineSymbol(conf.symbols, "nimconfig")

  registerPass(graph, semPass)
  if withEval:
    registerPass(graph, evalPass)

  var m = graph.makeStdinModule()
  incl(m.flags, sfMainModule)
  graph.vm = setupVM(m, cache, "stdin", graph)
  graph.compileSystemModule()

  return (graph, m)


# proc readQue(que: var Deque[string], buf: pointer, bufLen: int): int =

proc readLine*(
    lineQue: var Deque[string],
    s: PLLStream, buf: pointer, bufLen: int): int =

  if lineQue.len == 0:
    s.kind = llsNone
    return

  s.s = ""
  s.rd = 0
  var line = lineQue.popFirst()

  add(s.s, line)

  inc(s.lineOffset)
  result = min(bufLen, len(s.s) - s.rd)

  if result > 0:
    copyMem(buf, addr(s.s[s.rd]), result)
    inc(s.rd, result)



when isMainModule:
  var lineQue = initDeque[string]()
  lineQue.addLast "echo 12"
  lineQue.addLast "echo 90-" # Malformed AST
  lineQue.addLast "echo 90"
  lineQue.addLast "proc userProc() = echo 1"
  lineQue.addLast "userProc()"
  lineQue.addLast "proc userProc() = echo 2"
  lineQue.addLast "userProc()"
  lineQue.addLast "echo 100"

  proc llStreamReader(s: PLLStream, buf: pointer, bufLen: int): int =
    # `compiler/passes/processModule()` has two nested `while true` loops -
    # outermost one repeatedly calls `openParser()` on input text, and
    # internal one processes all toplevel statements. After inner loop
    # finishes, input stream is checked for `if s.kind != llsStdIn: break`,
    # meaning it is not possible to break out of the loop freely. It might be
    # possible to implement in async manner though, I'm not entirely sure.
    # other option would be to just copy body of the outer for loop and run
    # it manually.
    result = readLine(lineQue, s, buf, bufLen)

  var (graph, m) = newGraphStdin()
  processModule(graph, m, llStreamOpenStdIn(llStreamReader))
  echo "Finished module processing"

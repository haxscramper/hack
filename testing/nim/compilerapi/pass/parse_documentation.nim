import std/[os, strformat]
import Nim / compiler /
  [ idents, options, modulegraphs, passes, lineinfos, sem, pathutils, ast,
    astalgo, modules, condsyms, passaux, semdata, msgs, rod, renderer
  ]

let file = "/tmp/ee.nim"

file.writeFile("""
template templ1() = echo "test"
template templ2() = templ1()
templ2()
proc proc1 = templ2()
""")


var graph: ModuleGraph = block:
  var
    cache: IdentCache = newIdentCache()
    config: ConfigRef = newConfigRef()

  let path = getCurrentDir() / "Nim/lib"

  config.libpath = AbsoluteDir(path)
  config.searchPaths.add config.libpath
  config.projectFull = AbsoluteFile(file)
  initDefines(config.symbols)

  newModuleGraph(cache, config)

proc sempassHack(context: PPassContext, n: PNode): PNode {.nosinks.} =
  var c = PContext(context)
  if sfMainModule in c.module.flags:
    echo "\e[41m*====\e[49m  sempass start  \e[41m=====*\e[49m"
    echo n

  result = sem.myProcess(context, n)

  if sfMainModule in c.module.flags:
    echo result

registerPass(graph, makePass(
  sem.myOpen,
  sempassHack,
  sem.myClose,
  isFrontend = true
))

compileProject(graph)
echo "Done"

import std/[os, strformat, strtabs]

import Nim / compiler /
  [ idents, options, modulegraphs, passes, lineinfos, sem, pathutils, ast,
    parser, astalgo, modules, nimeval, condsyms, cmdlinehelper, extccomp,
    passaux
  ]

var
  cache: IdentCache = newIdentCache()
  config: ConfigRef = newConfigRef()

let
  path = getCurrentDir() / "Nim/lib"
  file = "/tmp/ee.nim"

file.writeFile("echo 1")

config.libpath = AbsoluteDir(path)
config.searchPaths.add config.libpath
config.projectFull = AbsoluteFile(file)
initDefines(config.symbols)
initVars(config)

var graph: ModuleGraph = newModuleGraph(cache, config)

registerPass(graph, verbosePass)
registerPass(graph, semPass)
compileProject(graph)

for m in graph.modules:
  if m != nil and sfMainModule in m.flags:
    debug m

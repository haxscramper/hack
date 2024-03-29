import hnimast, hnimast/[compiler_aux]

import compiler/[
  ast, passes, sem, llstream, cgen, extccomp,
  pathutils, options, lineinfos
]

import
  std/osproc

import
  hmisc/other/[oswrap]


let inFile = "/tmp/mainModule.nim"

proc main() =

  inFile.writeFile("""
static:
  echo "123"

var str = "123"

str.add 12

echo str

""")

  var graph = newModuleGraph(
    AbsFile(inFile),
    structuredErrorHook =
      proc(config: ConfigRef; info: TLineInfo; msg: string; level: Severity) =
        echo "---------"
        echo msg
        echo "---------"
  )

  var conf = graph.config

  conf.projectPath = AbsoluteDir("/tmp/nimcache")
  conf.nimcacheDir = AbsoluteDir("/tmp")
  conf.outDir = conf.nimcacheDir
  conf.outFile = RelativeFile("outfile")

  extccomp.initVars(conf)

  graph.registerPass(verbosePass)
  graph.registerPass(semPass)
  graph.registerPass(cgenPass)

  compileProject(graph)
  cgenWriteModules(graph.backend, conf)
  callCcompiler(conf)

main()

discard execCmd("/tmp/outfile")

import semhack
import haxdoc/[compiler_aux]
import hmisc/other/[oswrap]

let file = AbsFile("/tmp/infile.nim")

file.writeFile("""
import std/[macros]
macro recursive(arg: static[int]): untyped =
  result = newStmtList()
  if arg > 0:
    let newVal = newLit(arg - 1)

    result.add newCall("echo", newVal)
    result.add newCall("recursive", newVal)

recursive(10)

""")


var graph = newModuleGraph(file, getStdPath())
registerPass(graph, semhack.semPass)
echo "Graph compilation started"
compileProject(graph)
echo "Graph compilation done"

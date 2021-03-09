import compiler/[passes, ast, modulegraphs, semdata]
import hmisc/hexceptions
import hmisc/types/[colortext]
import hnimast
import std/[tables, options, hashes, strutils]

type
  SemLogEntry* = object
    actionName: string
    resultNode: PNode

  SemLogContext* = ref object of PContext
    expansionPairs: Table[PNode, SemLogEntry]

proc hash*(node: PNode): Hash = hash(unsafeAddr node)

proc recordSemExpansion*(
    ctx: PPassContext,
    actionName: string,
    originalNode, resultNode: PNode) =

  printSeparator("IN")
  echo ($originalNode).colorizeToStr("nim").indent(8)
  echo ""
  echo treeRepr(originalNode)
  printSeparator("  vvvvv  ")
  echo ($resultNode).colorizeToStr("nim").indent(8)
  printSeparator("OUT")
  echo "\n\n\n"


  SemLogContext(ctx).expansionPairs[originalNode] = SemLogEntry(
    resultNode: resultNode,
    actionName: actionName
  )

proc newLogContext*(graph: ModuleGraph, module: PSym): PContext =
  result = SemLogContext()
  var context = newContext(graph, module)
  for name, resField, subField in fieldPairs(
    PContext(result)[], context[]
  ):
    resField = subField
  # result = SemLogContext()

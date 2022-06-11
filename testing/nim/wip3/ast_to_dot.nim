import std/[macros, tables, strformat, strutils, sequtils]

proc truncate(value: string, length = 9, ellipsis = "..."): string =
  if value.len > length:
    value[0..length - 1] & ellipsis
  else:
    value

proc surround(input: string, surrWith: (string, string)): string =
  surrWith[0] & input & surrWith[1]

proc surround(input: string, surrWith: string): string =
  surrWith & input & surrWith

proc surround1(input: string, surrWith: (string, string)): string =
  if input.len == 0: ""
  else: input.surround(surrWith)

proc escapeHTML(input: string): string =
  input.multiReplace([
    (">", "&gt;"),
    ("<", "&lt;"),
    ("&", "&amp;"),
    ("\"", "&quot;")
  ])

proc dumpDotAstImpl(file: string, body: NimNode): NimNode =
  var idx = 0
  var descrTable: Table[int, tuple[idx: int, node: NimNode]]
  var graph: seq[string]

  proc toDot(nodeIdx: int, node: NimNode): (int, string) =
    inc idx
    result[0] = idx
    result[1] = "n$# -> " % $idx

    var subnodes: seq[int]
    descrTable[idx] = (nodeIdx, node)

    for subIdx, sub in node:
      let res = toDot(subIdx, sub)
      subnodes.add(res[0])

    result[1] &= "{ $# }" % subnodes.mapIt("n" & $it).join(",")
    graph.add(result[1])

  for subIdx, node in body:
    discard toDot(subIdx, node)

  var resTotal: seq[string]

  for id, pair in descrTable:
    let (nodeIdx, node) = pair
    let label: string = substr($node.kind, 3)
    let text: string =
      case node.kind:
        of nnkIdent, nnkSym, nnkCommentStmt:
          node.strVal.escape()
        of nnkStrLit..nnkTripleStrLit:
          $node.strVal.truncate().surround("\"")
        of nnkCharLit .. nnkUInt64Lit: $node.intVal
        of nnkFloatLit: $node.floatVal
        else: ""

    var color: string =
      case node.kind:
        of nnkStmtList: "azure2"
        of nnkIdent: "brown2"
        of nnkStrLit, nnkCommentStmt: "green"
        of nnkSym: "yellow"
        of nnkVarSection, nnkLetSection, nnkIdentDefs: "magenta2"
        of nnkFloatLit: "lightblue"
        else: "cyan2"

    var res = "n$# [label = <$#, $#<br/>$#>, fillcolor = $#, style = filled];" % [
      $id,
      $nodeIdx,
      label.surround1(("<i>", "</i>")),
      text
        .escapeHTML()
        .surround1(("<b>", "</b>"))
        .surround1(("<font face='courier'>", "</font>")),
      color
    ]

    res = res.replace("\n","")
    resTotal.add(res)


  let resultString = """
digraph G {
rankdir = LR;
// splines = ortho;
node[shape=box];
$#
$#
}
""" %
    [
      resTotal.join("\n"),
      graph.join("\n")]
  if file.len == 0:
    let resStrNode = newStrLitNode(resultString)
    result = quote do:
      echo `resStrNode`
  else:
    file.writeFile(resultString)
    result = nnkStmtList.newTree()

macro dumpDotAst(head: static[string], body: untyped): untyped =
  dumpDotAstImpl(head, body)

macro dumpDotAstTyped(head: static[string], body: typed): untyped =
  dumpDotAstImpl(head, body)

dumpDotAst "test_untyped.dot":
  proc firstProcedure(a: float) =
    ## Documentation for the first procedure
    discard

  proc secondProcedure() =
    firstProcedure(0.3)

  # proc generic{a + b}[T: char | int](a: int = 12, b: float): Return {.pragma.} =
  #   echo 12

dumpDotAstTyped "test_typed.dot":
  proc firstProcedure(a: float) =
    ## Documentation for the first procedure
    discard

  proc secondProcedure() =
    firstProcedure(0.3)

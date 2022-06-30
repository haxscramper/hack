import
  htsparse/cpp/cpp,
  hmisc/hasts/graphviz_ast,
  hmisc/other/oswrap,
  hmisc/core/all,
  std/[strutils, tables, strformat, sets]

type
  ConnectData = object
    source: tuple[objname, methname: string]
    file: AbsFile
    line: int
    case isLambda: bool
      of true:
        closure: string

      of false:
        target: tuple[objname, methname: string]

  Register = object
    active: AbsFile
    connections: seq[ConnectData]

proc add(reg: var Register, line: int, objFrom, methFrom, objTo, methTo: string) =
  reg.connections.add(ConnectData(
    isLambda: false,
    source: (objFrom, methFrom),
    target: (objTo, methTo),
    line: line,
    file: reg.active
  ))


proc add(reg: var Register, line: int, objFrom, methFrom, closure: string) =
  reg.connections.add(ConnectData(
    isLambda: true,
    source: (objFrom, methFrom),
    closure: closure,
    line: line,
    file: reg.active
  ))


proc failNode*(node: CppNode) {.noreturn.} =
  raise newUnexpectedKindError(
    node,
    node.strVal() &
      "\n" &
      $node.treeRepr(
        unnamed = true,
        opts = hdisplay(maxLen = 6, maxDepth = 4)))


proc baseStr*(node: CppNode): string =
  node.getBase()[node.slice()].multiReplace({"\x0D": ""})

proc splitPointerExpr(expr: CppNode): tuple[obj, meth: string] =
  if expr of cppPointerExpression:
    return splitPointerExpr(expr["argument"])

  elif expr of cppQualifiedIdentifier:
    result.obj = expr["scope"].baseStr()
    result.meth = expr["name"].baseStr()

  elif expr of cppIdentifier:
    result.obj = "CONTEXT"
    result.meth = expr.baseStr()

  elif expr of cppBinaryExpression:
    if $expr{1} == "&":
      return splitPointerExpr(expr["right"])

    else:
      echov expr{1}
      failNode expr

  elif expr of cppCallExpression:
    let call = expr["function"].baseStr()
    if "QOverload" in call:
      result = splitPointerExpr(expr["arguments"][0])
      let typ = expr["function"]["scope"]["arguments"][0]["type"]
      result.meth &= "(" & $typ & ")"

    elif call in ["SIGNAL", "SLOT"]:
      result.obj = call
      result.meth = expr["arguments"][0].baseStr()

    else:
      echov call
      failNode expr

  else:
    failNode expr

proc rec(node: CppNode, reg: var Register) =
  case node.kind:
    of cppCallExpression:
      if node["function"].strVal() == "connect":
        let args = node["arguments"]
        let signal = args[1]
        let (osig, msig) = splitPointerExpr(signal)
        if 3 < args.len():
          let slot = args[3]
          if slot of cppPointerExpression or (
            slot of cppCallExpression and
            slot["function"].baseStr() == "SLOT"
          ):
            let (oslot, mslot) = splitPointerExpr(slot)
            reg.add(node.startPoint().row.int, osig, msig, oslot, mslot)

          else:
            failNode args

        elif 2 < args.len():
          let target = args[2]
          if target of cppLambdaExpression:
            reg.add(node.startPoint().row.int,
                    osig, msig, baseStr(target).dedent())

          else:
            failNode target

      else:
        for sub in node:
          rec(sub, reg)

    else:
      for sub in node:
        rec(sub, reg)


startHax()
var reg = Register()


for file in walkDir(
  AbsDir("/home/test/workspace/git-sandbox/merge_of_1_and_2_zips"),
  AbsFile,
  exts = @["hpp", "cpp", "h"],
  recurse = true
):
  reg.active = file
  var content = readFile(file)
  if "connect" in content:
    echov file
    let node = parseCppString(addr content)
    rec(node, reg)

  # if 20 < reg.connections.len:
  #   break

# `Class -> Methods`
var knownTable: Table[string, HashSet[(string, bool)]]
for data in reg.connections:
  knownTable.mgetOrDefault(data.source.objname).incl((data.source.methname, true))
  if not data.isLambda:
    knownTable.mgetOrDefault(data.target.objname).incl((data.target.methname, false))

var
  g = makeDotGraph()
  revPaths: Table[(string, string), (int, int)]
  id = 0

for class, methods in knownTable:
  var signals, slots: seq[RecordField]
  inc id
  let slotNode = id
  inc id
  let signalNode = id
  for (meth, isSignal) in methods:
    if isSignal:
      signals.add makeDotRecord(id, meth)
      revPaths[(class, meth)] = (signalNode, id)

    else:
      slots.add makeDotRecord(id, meth)
      revPaths[(class, meth)] = (slotNode, id)
    inc id

  if not signals.empty():
    echov signals.len
    signals.insert makeDotRecord(id, class & " SIGNALS")
    g.add makeRecordDotNode(signalNode, signals)

  if not slots.empty():
    inc id
    slots.insert makeDotRecord(id, class & " SLOTS")
    g.add makeRecordDotNode(slotNode, slots)

for data in reg.connections:
  let (stop, ssub) = revPaths[data.source]
  if data.isLambda:
    let nid = toDotNodeId(id)
    inc id
    g.add makeDotNode(
      nid, &"anonymous lambda:\n\n{data.closure}")

    g.add makeDotEdge(
      toDotPath(stop, ssub, dppRight),
      nid,
      &"{data.file.name}:{data.line}"
    )

  else:
    let (ttop, tsub) =  revPaths[data.target]
    if ttop == stop:
      var e = makeDotEdge(
        toDotPath(stop, ssub, dppRight),
        toDotPath(ttop, tsub, dppRight),
        &"{data.file.name}:{data.line}"
      )

      g.add e

    else:
      g.add makeDotEdge(
        toDotPath(stop, ssub, dppRight),
        toDotPath(ttop, tsub, dppLeft),
        &"{data.file.name}:{data.line}"
      )

g.rankdir = grdLeftRight
g.toPng(AbsFile("/tmp/res.png"))

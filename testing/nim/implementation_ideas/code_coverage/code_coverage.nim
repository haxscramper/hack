import std/[
  macros, tables, intsets, hashes,
  sets, enumerate, algorithm, strutils
]

import
  hmisc/core/all,
  hmisc/types/colorstring,
  hmisc/algo/hstring_algo

import
  hnimast/hast_common

type
  RevSetTable[K, V] = object
    maxId: int
    keySets: Table[K, int]
    revKeys: Table[int, seq[K]]
    valueTable: Table[int, V]
    revValue: Table[V, int]

proc addPair*[K, V](table: var RevSetTable[K, V], key: K, val: V) =
  var id: int
  if val in table.revValue:
    id = table.revValue[val]

  else:
    id = table.maxId
    inc table.maxId

  table.keySets[key] = id
  table.valueTable[id] = val
  table.revValue[val] = id
  table.revKeys.mgetOrPut(id, @[]).add key

proc getKeys*[K, V](table: RevSetTable[K, V], val: V): seq[K] =
  table.revKeys[val]

proc getValue*[K, V](table: RevSetTable[K, V], key: K): V =
  table.valueTable[table.keySets[key]]

proc otherKeys*[K, V](table: RevSetTable[K, V], key: K): seq[K] =
  table.revKeys[table.keySets[key]]

proc keyValueGroup*[K, V](table: RevSetTable[K, V], key: K):
  tuple[keys: seq[K], value: V] =

  result.keys = table.otherKeys(key)
  result.value = table.getValue(key)

proc `[]`*[K, V](table: RevSetTable[K, V], key: K): V =
  table.getValue(key)

proc `[]=`*[K, V](table: var RevSetTable[K, V], key: K, val: V) =
  table.addPair(key, val)

proc contains*[K, V](table: RevSetTable[K, V], key: K): bool =
  key in table.keySets

var
  covStats: Table[int, HashSet[int]]
  fileTable: Table[int, string]
  covActive: IntSet
  procRanges: RevSetTable[int, string]

proc startPos(node: NimNode): LineInfo =
  case node.kind:
    of AtomicNodes:
      result = node.lineInfoObj()

    else:
      result = node[0].lineInfoObj()

proc finishPos(node: NimNode): LineInfo =
  case node.kind:
    of AtomicNodes:
      result = node.lineInfoObj()
      result.column += len($node) - 1

    else:
      if len(node) > 0:
        var idx = len(node) - 1
        while idx >= 0 and node[idx].kind in {nnkEmpty}:
          dec idx

        if idx >= 0:
          result = node[idx].finishPos()

        else:
          result = node.lineInfoObj()

      else:
        result = node.lineInfoObj()

func addFile(file: static[string]) =
  {.cast(noSideEffect).}:
    const hash = hash(file)
    if hash notin fileTable:
      fileTable[hash] = file

func execNode(hash, line, column: int) =
  {.cast(noSideEffect).}:
    if hash notin covStats:
      covStats[hash] = initHashSet[int]()

    covStats[hash].incl line

func execWithCoverage(
    procname: static[string],
    procnameFull: static[string],
    lineRange: static[Slice[int]]): bool =

  const hash = hash(procname)
  {.cast(noSideEffect).}:
    if lineRange.a notin procRanges:
      for line in lineRange:
        procRanges[line] = procname

    return true

proc pprintCoverage() =
  for id, file in fileTable:
    const w = 5
    var shown: IntSet
    if id in covStats:
      let lines = readFile(file).split("\n")
      for idx, _ in lines:
        let idx = idx + 1
        if idx notin shown and idx in procRanges:
          let other = procRanges.otherKeys(idx).sorted()
          for idx in other:
            shown.incl idx
            let line = lines[idx - 1]
            if idx in covStats[id]:
              echo $idx |<< 4, toGreen(line)

            else:
              echo $idx |<< 4, toRed(line)

proc fileId(file: string): NimNode =
  hash(file).newLit()

proc execCall(node: NimNode): NimNode =
  let iinfo = node.lineInfoObj()
  return newCall(
    bindSym"execNode",
    fileId(iinfo.filename),
    newLit(iinfo.line),
    newLit(iinfo.column))


proc execNode(node: NimNode): NimNode =
  nnkStmtListExpr.newTree(execCall(node), node)

const
  convTable = {
    nnkStmtList, nnkElifBranch, nnkElse,
    nnkDiscardStmt, nnkReturnStmt: 0 .. ^1,
    nnkCall, nnkCommand: 1 .. ^1,
  }

  convRanges = toMapArray convTable
  convKinds = toKeySet convTable


proc transform(node: NimNode): NimNode =
  case node.kind:
    of AtomicNodes:
      result = node

    of convKinds:
      result = newTree(node.kind)
      for sub in node[0 ..< convRanges[node.kind].a]:
        result.add transform(sub)



      for sub in node[convRanges[node.kind]]:
        if sub.kind in convKinds:
          result.add execNode(transform(sub))

        else:
          result.add transform(sub)

      if node.kind in { nnkElse }:
        result[0].add execCall(node)

      if node.kind in { nnkStmtList, nnkIfStmt }:
        result = execNode(result)

    else:
      result = newTree(node.kind)
      for idx, sub in node:
        result.add transform(sub)


proc procFullname(impl: NimNode): string =
  result.add impl.name().strVal()
  result.add "("
  for arg in impl.params[1..^1]:
    for _ in arg[1 .. ^2]:
      result.add arg[^2].toStrLit().strVal()
      result.add ","

  result.add ")"
  result.add impl.params[0].toStrLit().strVal()

proc transformImpl(impl: NimNode): NimNode =
  echov impl.treeRepr1(lineInfo = true)

  result = copyNimTree(impl)

  let
    name = newLit(impl.name().strVal())
    fullname = newLit(impl.procFullname())
    oldBody = impl.body
    transformed = impl.body.transform()
    execCheck = bindSym"execWithCoverage"
    start = impl.startPos()
    finish = impl.finishPos()
    startLine = newLit(start.line)
    finishLine = newLit(finish.line)
    addFile = bindSym"addFile"
    procFile = impl.lineInfoObj().filename
    mainExec = execCall(impl)

  result.body = quote do:
    `addFile`(`procFile`)
    `mainExec`
    if `execWithCoverage`(`name`, `fullname`, `startLine` .. `finishLine`):
      `transformed`

    else:
      `oldBody`

  echov result.repr()



macro astCov*(impl: untyped): untyped = transformImpl(impl)
macro semCov*(impl: typed): untyped = transformImpl(impl)

proc expr(): int = 12

proc testAstStmts(cond: bool) {.astCov.} =
  if cond:
    discard 2

  else:
    discard 2

  discard expr()

proc genericCover[T](arg: T) {.astCov.} =
  when arg is string or arg is cstring or arg is ptr cstring:
    return

  else:
    when arg isnot ptr and arg isnot ref:
      discard sizeof(arg)

    else:
      discard sizeof(arg[])


static:
  startHaxComp()


proc longCondition(arg1, arg2, arg3: bool) {.astCov.} =
  if arg1 and
     arg2 and
     (arg1 xor arg2):
    discard 2

  else:
    discard 1


testAstStmts(false)
testAstStmts(true)

genericCover(12)
genericCover("wer")

longCondition(true, true, true)

pprintCoverage()

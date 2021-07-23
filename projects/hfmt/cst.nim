import
  compiler/[ast, lineinfos, idents]

import
  hnimast/hast_common

import
  std/[macros, with]

import
  hmisc/[helpers, hexceptions],
  hmisc/types/colorstring,
  hmisc/algo/[halgorithm, clformat]

type
  CstPoint* = object
    tokenIdx*: int
    lineInfo*: TLineInfo

  CstRange = object
    startPoint*, endPoint*: CstPoint

  CstComment = object
    rangeInfo*: CstRange
    text*: string

  CstNode* = ref CstNodeObj
  CstNodeObj* = object
    rangeInfo*: CstRange
    comment*: CstComment
    flags*: set[TNodeFlag]

    case kind*: TNodeKind
      of nkCharLit..nkUInt64Lit:
        intVal*: BiggestInt

      of nkFloatLit..nkFloat128Lit:
        floatVal*: BiggestFloat

      of nkStrLit..nkTripleStrLit:
        strVal*: string

      of nkIdent, nkSym:
        ident*: PIdent

      else:
        sons*: seq[CstNode]

macro wrapSeqContainer*(
    main: typed,
    fieldType: typed,
    isRef: static[bool] = false,
    withIterators: static[bool] = true
  ) =

  ## - TODO :: Generate kind using `assertKind`

  let
    mainType = main[0]
    field = main[1]
    mutType = if isRef: mainType else: nnkVarTy.newTree(mainType)

  let
    indexOp = ident("[]")
    indexAsgn = ident("[]=")

  result = quote do:
    proc len*(main: `mainType`): int = len(main.`field`)
    proc add*(main: `mutType`, other: `mainType` | seq[`mainType`]) =
      add(main.`field`, other)

    proc `indexOp`*(main: `mainType`, index: IndexTypes): `fieldType` =
      main.`field`[index]

    proc `indexOp`*(main: `mainType`, slice: SliceTypes): seq[`fieldType`] =
      main.`field`[slice]

    proc `indexAsgn`*(
        main: `mainType`, index: IndexTypes, value: `fieldType`) =

      main.`field`[index] = value

  if withIterators:
    result.add quote do:
      iterator pairs*(main: `mainType`): (int, `fieldType`) =
        for item in pairs(main.`field`):
          yield item

      iterator items*(main: `mainType`): `fieldType` =
        for item in items(main.`field`):
          yield item

      iterator pairs*(main: `mainType`, slice: SliceTypes):
        (int, `fieldType`) =
        var slice = clamp(slice, main.`field`.len)
        for idx in slice:
          yield (idx, main.`field`[idx])

  echo result.repr()

macro wrapStructContainer*(
    main: untyped,
    fieldList: untyped,
    isRef: static[bool] = false
  ): untyped =

  assertKind(main, {nnkDotExpr})

  let
    mainType = main[0]
    structField = main[1]
    mutType = if isRef: mainType else: nnkVarTy.newTree(mainType)

  result = newStmtList()

  var prev: seq[NimNode]
  for field in fieldList:
    if field.kind != nnkExprColonExpr:
      prev.add field

    else:
      for name in prev & field[0]:
        assertNodeKind(name, {nnkIdent})
        let fieldType = field[1]
        assertNodeKind(fieldType, {nnkIdent, nnkBracketExpr})

        let asgn = ident(name.strVal() & "=")

        result.add quote do:
          func `name`*(n: `mainType`): `fieldType` =
            n.`structField`.`name`

          func `asgn`*(n: `mutType`, value: `fieldType`) =
            n.`structField`.`name` = value

      prev = @[]

  echo result.repr

wrapStructContainer(
  CstNode.rangeInfo, { startPoint, endPoint: CstPoint }, isRef = true)

wrapSeqContainer(CstNode.sons, CstNode, isRef = true)


func getStrVal*(p: CstNode, doRaise: bool = true): string =
  ## Get string value from `PNode`
  case p.kind:
    of nkIdent, nkSym: p.ident.s
    of nkStringKinds: p.strVal
    else:
      if doRaise:
        raiseArgumentError(
          "Cannot get string value from node of kind " & $p.kind)

      else:
        ""

func newEmptyCNode*(): CstNode = CstNode(kind: nkEmpty)
func add*(comm: var CstComment, str: string) = comm.text.add str
func newNodeI*(kind: TNodeKind, point: CstPoint): CstNode =
  CstNode(kind: kind, rangeInfo: CstRange(startPoint: point))

proc newTreeI*(
    kind: TNodeKind; info: CstPoint; children: varargs[CstNode]): CstNode =

  result = newNodeI(kind, info)
  if children.len > 0:
    result.startPoint = children[0].startPoint

  result.sons = @children


template transitionNodeKindCommon(k: TNodeKind) =
  let obj {.inject.} = n[]
  n[] = CstNodeObj(
    kind: k,
    rangeInfo: obj.rangeInfo,
    comment: obj.comment,
    flags: obj.flags
  )

  when defined(useNodeIds):
    n.id = obj.id

proc transitionSonsKind*(n: CstNode, kind: range[nkComesFrom..nkTupleConstr]) =
  transitionNodeKindCommon(kind)
  n.sons = obj.sons

proc transitionIntKind*(n: CstNode, kind: range[nkCharLit..nkUInt64Lit]) =
  transitionNodeKindCommon(kind)
  n.intVal = obj.intVal

proc transitionNoneToSym*(n: CstNode) =
  transitionNodeKindCommon(nkSym)

proc newProcNode*(
    kind: TNodeKind, info: CstPoint, body: CstNode,
    params, name, pattern, genericParams, pragmas, exceptions: CstNode
  ): CstNode =

  result = newNodeI(kind, info)
  result.sons = @[
    name, pattern, genericParams, params, pragmas, exceptions, body]

func `$`*(point: CstPoint): string =
  with result:
    add hshow(point.lineInfo.line)
    add ":"
    add hshow(point.lineInfo.col)
    add "@", tcGrey27.fg + tcDefault.bg
    add hshow(point.tokenIdx)

func treeRepr*(
    pnode: CstNode,
    colored: bool = true,
    pathIndexed: bool = false,
    positionIndexed: bool = true,
    maxdepth: int = 120,
    maxlen: int = 30
  ): string =

  var p = addr result
  template res(): untyped = p[]

  proc aux(n: CstNode, level: int, idx: seq[int]) =
    if pathIndexed:
      res &= idx.join("", ("[", "]")) & "    "

    elif positionIndexed:
      if level > 0:
        res &= "  ".repeat(level - 1) & "\e[38;5;240m#" & $idx[^1] & "\e[0m" &
          "\e[38;5;237m/" & alignLeft($level, 2) & "\e[0m" & " "

      else:
        res &= "    "

    else:
      res.addIndent(level)

    if level > maxdepth:
      res &= " ..."
      return
    elif isNil(n):
      res &= toRed("<nil>", colored)
      return

    with res:
      add ($n.kind)[2..^1]
      add " "
      add $n.startPoint()
      add ".."
      add $n.endPoint()

    if n.comment.text.len > 0:
      res.add "\n"
      for line in split(n.comment.text, '\n'):
        res.add "  # " & toCyan(line) & "\n"

    else:
      res.add " "


    case n.kind:
      of nkStringKinds: res &= "\"" & toYellow(n.getStrVal(), colored) & "\""
      of nkIntKinds: res &= toBlue($n.intVal, colored)
      of nkFloatKinds: res &= toMagenta($n.floatVal, colored)
      of nkIdent, nkSym: res &= toGreen(n.getStrVal(), colored)
      of nkCommentStmt: discard
      else:
        if n.len > 0: res &= "\n"
        for newIdx, subn in n:
          aux(subn, level + 1, idx & newIdx)
          if level + 1 > maxDepth: break
          if newIdx > maxLen: break
          if newIdx < n.len - 1: res &= "\n"

  aux(pnode, 0, @[])

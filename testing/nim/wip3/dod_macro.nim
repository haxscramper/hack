
# import hmisc/other/hpprint

# proc toPPrintTree(nim: NimNode, conf: var PPrintConf, path: PPrintPath): PPrintTree =
#   result = newPPrintConst(
#     nim.treeRepr(),
#     "NimNode",
#     conf.getId(nim),
#     fgCyan + bgDefault,
#     path
#   )

#   updateCounts(result)

import std/[macros, strutils, tables, hashes]

proc failNode(node: NimNode) =
  echo node.treeRepr()
  assert false

type
  NimName = object
    name: NimNode
    exported: bool
    pragmas: seq[NimNode]

  CaseContext = object
    case isElse: bool
      of false:
        switchOn: seq[NimNode]

      else:
        discard

  DodFieldKind = enum
    dodfDirectEmbed ## Directly stored in row
    dodfSparseEmbed ## Aux `Table[]`
    dodfKeyInto ## Index into some other table


  DodField = object
    name: NimName
    typ: NimNode
    context: seq[CaseContext]
    kind: DodFieldKind

  DodObj = object
    ## Specification of the single table (object). It can have multiple
    ## directly embedded fields (columns), auxiliary fields (`Table[]`) and
    ## keys into other objects.
    name: NimName
    objFields: seq[DodField]

  DodGroup = object
    objects: seq[DodObj]

func strVal(name: NimName): string = name.name.strVal()

proc unparseName(node: NimNode): NimName =
  case node.kind:
    of nnkPragmaExpr:
      case node[0].kind:
        of nnkPostfix:
          result.name = node[0][1]
          result.exported = true

        of nnkIdent, nnkSym:
          result.name = node[0]

        else:
          failNode node

      for pragma in node[1]:
        result.pragmas.add pragma

    of nnkPostfix:
      result.name = node[1]
      result.exported = true

    of nnkIdent, nnkSym:
      result.name = node

    else:
      failNode node

proc skipNodes(node: NimNode, skip: set[NimNodeKind]): tuple[
  target: NimNode, skipped: seq[NimNode]] =

  result.target = node
  while result.target.kind in skip:
    result.skipped.add result.target
    result.target = result.target[0]

proc unparseIdentDefs(node: NimNode): seq[tuple[name: NimName, typ: NimNode]] =
  let expr = node[^1]
  let typ = node[^2]
  for idx, name in node[0..^3]:
    result.add((unparseName(name), typ))

proc unparseFields*(node: NimNode): seq[DodField] =
  expectKind(node, {nnkRecList})

  proc auxBranches(node: NimNode, context: seq[CaseContext]): seq[DodField]
  proc auxFields(node: NimNode, context: seq[CaseContext]): seq[DodField] =
    case node.kind:
      of nnkIdentDefs:
        for (name, typ) in unparseIdentDefs(node):
          result.add DodField(name: name, typ: typ, context: context)

      of nnkRecList:
        for sub in node:
          result.add auxFields(sub, context)

      of nnkEmpty:
        discard

      else:
        failNode node


  proc auxBranches(node: NimNode, context: seq[CaseContext]): seq[DodField] =
    for branch in node:
      case branch.kind:
        of nnkOfBranch:
          result.add auxFields(branch[^1], context & CaseContext(
            isElse: false, switchOn: branch[0 .. ^2]))

        of nnkElse:
          result.add auxFields(branch[0], context & CaseContext(
            isElse: true))

        else:
          failNode branch

  return auxFields(node, @[])

proc unparseDod(body: NimNode): DodGroup =
  for section in body:
    expectKind(section, {nnkTypeSection})
    for obj in section:
      expectKind(obj, {nnkTypeDef})
      let (body, skipped) = obj[2].skipNodes({nnkRefTy, nnkPtrTy})

      result.objects.add DodObj(
        name: unparseName(obj[0]),
        objFields: body[2].unparseFields()
      )

  var knownObj: seq[string]
  for obj in result.objects:
    knownObj.add obj.name.strVal()

  for obj in mitems(result.objects):
    for field in mitems(obj.objFields):
      if field.typ.kind in {nnkIdent, nnkSym} and
         field.typ.strVal() in knownObj:
        field.kind = dodfKeyInto

      else:
        for pragma in field.name.pragmas:
          if pragma.kind == nnkIdent and pragma.strVal() == "sparse":
            field.kind = dodfSparseEmbed


proc generateFromDod(group: DodGroup): NimNode =
  var
    idDefs       = newStmtList()
    typeDefs     = nnkTypeSection.newTree()
    stateFields = nnkRecList.newTree()
    auxProcs = newStmtList()
    accessProcs = newStmtList()

  let state = ident"State"
  for obj in group.objects:
    let id = ident(obj.name.strVal() & "Id")

    idDefs.add quote do:
      type
        `id` = distinct int

      func hash(id: `id`): Hash = Hash(id)
      func toIdx(id: `id`): int = int(id) - 1

    var fieldList = nnkRecList.newTree()

    let objState = ident("values" & obj.name.strVal())
    stateFields.add newIdentDefs(
      objState,
      nnkBracketExpr.newTree(ident"DodSeq", obj.name.name)
    )

    func rewriteType(t: NimNode): NimNode =
      return t

    for field in obj.objFields:
      let n = field.name.name
      let (getN, setN) = (ident("get" & n.strVal()), ident("set" & n.strVal()))

      let t = rewriteType(field.typ)
      case field.kind:
        of dodfDirectEmbed:
          fieldList.add newIdentDefs(n, t)

          auxProcs.add quote do:
            func `getN`(state: `state`, id: `id`): `t` =
              state.`objState`.getVal(id)

            func `setN`(state: `state`, id: `id`, val: `t`) =
              state.`objState`.setVal(id, val)

        of dodfKeyInto:
          fieldList.add newIdentDefs(
            n, ident(field.typ.strVal() & "Id"))

        of dodfSparseEmbed:
          stateFields.add newIdentDefs(
            ident(n.strVal() & "Of" & obj.name.strVal()),
            nnkBracketExpr.newTree(ident"Table", id, field.typ),
          )

    typeDefs.add nnkTypeDef.newTree(
      ident(obj.name.strVal()),
      newEmptyNode(),
      nnkObjectTy.newTree(
        newEmptyNode(),
        newEmptyNode(),
        fieldList
      )
    )


  typeDefs.add nnkTypeDef.newTree(
    state,
    newEmptyNode(),
    nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode(),
      stateFields
    )
  )


  result = newStmtList()
  result.add idDefs
  result.add typeDefs
  result.add accessProcs
  result.add auxProcs


type
  DodSeq[T] = object
    data: seq[T]

func toIdx(s: string) = discard

func addVal[V](s: DodSeq[V], val: V): int =
  result = s.data.len
  s.data.add val

func getVal[V, Idx](s: DodSeq[V], idx: Idx): V =
  s.data[toIdx(idx)]

func setVal[V, Idx](s: var DodSeq[V], idx: Idx, val: V) =
  s.data[toIdx(idx)] = val

func mgetVal[V, Idx](s: DodSeq[V], idx: Idx): var V =
  s.data[toIdx(idx)]

macro makeDod(body: untyped): untyped =
  let dod = unparseDod(body)
  # pprint dod
  result = generateFromDod(dod)
  echo result.repr()

makeDod:
  type
    Item = object
      other: OtherItem
      name: string
      extraName {.sparse.}: string

    OtherItem = object
      id: int

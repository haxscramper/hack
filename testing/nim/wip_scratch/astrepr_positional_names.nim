
## `treeRepr()` implementation for the `PNode` tree structures

import
  ast/[
    ast,
    reports,
    renderer,
  ],
  front/options,
  lib/experimental/colortext,
  std/[
    tables,
    sets,
    strutils,
    strformat,
    enumerate
  ]

import std/options as std_options

type
  TReprFlags* = enum
    trfPositionIndexed
    trfReportInfo
    trfNodeLineInfo
    trfPackedFields
    trfSkipAuxError
    trfShowNodeIds
    trfSymOptions
    trfNamedPositions

export colortext.`$`, colortext.toHtml

const defaultTreeReprFlags* = {
  trfPositionIndexed,
  trfReportInfo,
  trfSkipAuxError
}

const treeReprCompact* = defaultTreeReprFlags + {trfPackedFields}

const nkAnyKind = { low(TNodeKind) .. high(TNodeKind) }

var namedPositions: Table[
    tuple[path: seq[TNodeKind], idx: int, back: bool],
    tuple[name: string, expected: set[TNodeKind]]
  ] = toTable({
  (@[nkObjConstr], 0, false): ("objName", {nkSym, nkIdent})
  (@[nkCaseStmt], 0, false): ("exprPos", {nkAnyKind}),
  (@[nkWhileStmt], 0, false): ("exprPos", {nkAnyKind}),
  (@[nkCaseStmt], 1, true): ("elsePos", {nkElse})
})

proc getPositionalName(
    path: seq[TNodeKind], idx: int, parentLen: int
  ): Option[tuple[name: string, expected: set[TNodeKind]]] =

  var sliceRange = 0 .. path.high
  while sliceRange.a <= sliceRange.b:
    let suffix = path[sliceRange]
    inc sliceRange.a
    if (suffix, idx, false) in namedPositions:
      return some namedPositions[(suffix, idx, false)]

    elif (suffix, parentLen - idx, true) in namedPositions:
      return some namedPositions[(suffix, parentLen - idx, true)]

proc textRepr*(conf: ConfigRef, typ: PType): ColText =
  result.add ($typ.kind)[2..^1] + fgMagenta
  if not isNil(typ.sym):
    result &= " sk:" & ($typ.sym.kind)[2..^1] + fgCyan

  if not isNil(typ.n):
    let t = $typ.n
    if '\n' notin t:
      result &= " " & t + fgRed

proc treeRepr*(
    conf: ConfigRef,
    pnode: PNode,
    flags: set[TReprFlags] = defaultTreeReprFlags,
    maxDepth: int          = 120,
    maxLen: int            = 30,
    maxPath: int           = 1
  ): ColText =
  coloredResult(1)

  proc format[I](s: set[I], sub: int): string =
    var first = true
    result = "{"
    for item in s:
      if not first: result.add ", "
      result.add substr($item, sub)
      first = false

    result.add "}"



  const setStyle = termFg(2, 1, 3)
  var visited: HashSet[int]
  proc aux(n: PNode, idx: seq[int], parentLen: int, path: seq[TNodeKind]) =
    var indent = 0
    proc hfield(name: string) =
      add " "
      add name
      add ":"

    proc vfield(name: string) =
      res.newline()
      addIndent(indent, 1)
      add name
      add ": "

    proc field(name: string) =
      if trfPackedFields in flags:
        hfield(name)
      else:
        vfield(name)

    if trfPositionIndexed in flags:
      if 0 < idx.len:
        indent = 2
        for idxIdx, pos in idx:
          if idx.len - maxPath < idxIdx:
            indent += 1 + (
              if pos in 0 .. 9: 1
              elif pos in 10 .. 100: 2
              else: 3
            )

            add "." + termFg(15)
            add $pos + termFg(15)

          else:
            indent += 2
            add "  "

        add " "
        inc indent

    else:
      addIndent(idx.len)

    if isNil(n):
      add "<nil>" + fgRed
      return

    elif cast[int](n) in visited:
      add " <visited>"
      return

    elif idx.len > maxDepth:
      add " ..."
      return

    visited.incl cast[int](n)

    add substr($n.kind, 2) + fgCyan
    if trfNamedPositions in flags and 0 < idx.len:
      let pos = idx[^1]
      let name = getPositionalName(path, pos, parentLen)


    proc addComment(sep: bool = true) =
      if n.comment.len > 0:
        add "\n"
        for idx, line in enumerate(
          split(n.comment.strip(leading = false), '\n')
        ):
          if idx > 0: add "\n"
          addi indent, "  # " & line + fgCyan

      elif sep:
        add " "

    proc addFlags() =
      if not isNil(n.typ):
        add " "

      if n.flags.len > 0:
        field("nflags")
        add format(n.flags, 2) + setStyle

      if not n.typ.isNil():
        field("typ")
        add conf.textRepr(n.typ)

    if trfNodeLineInfo in flags:
      field("pos")
      add $n.info.fileIndex.int + fgBlue
      add "/"
      add $n.info.line + fgCyan
      add ":"
      add $n.info.col + fgCyan
      add " "

    case n.kind:
      of nkStrKinds:
        add " "
        add "\"" & n.strVal + fgYellow & "\""
        addFlags()
        addComment()

      of nkCharLit .. nkUInt64Lit:
        add " "
        add $n.intVal + fgBlue
        addFlags()
        addComment()

      of nkFloatLit .. nkFloat128Lit:
        add " "
        add $n.floatVal + fgMagenta
        addFlags()
        addComment()

      of nkIdent:
        add " "
        add n.ident.s + fgGreen
        addFlags()
        addComment()

      of nkSym:
        add " "
        add n.sym.name.s + fgCyan
        if not n.sym.owner.isNil() and n.sym.owner.kind in {
          skModule, skType
        }:
          add (" (" & n.sym.owner.name.s & "." & n.sym.name.s & ")") + termFg(15)

        field "sk"
        add substr($n.sym.kind, 2) + fgBlue

        if n.sym.flags.len > 0:
          field("flags")
          add format(n.sym.flags, 2) + setStyle

        if n.sym.magic != mNone:
          field("magic")
          add substr($n.sym.magic, 1) + setStyle

        if trfSymOptions in flags and 0 < n.sym.options.len:
          field("opts")
          add format(n.sym.options, 3) + setStyle

        addFlags()
        addComment()

      of nkCommentStmt:
        addFlags()
        addComment()

      of nkError:
        let report = conf.getReport(n).semReport
        field("err")
        add substr($report.kind, 4) + termFg(5, 2, 0)
        hfield("errid")
        add $n.reportId.int + fgRed

      else:
        discard

    when defined(useNodeids):
      if trfShowNodeIds in flags:
        field("nid")
        add $n.id

    if n.kind notin nkNone .. nkNilLit:
      addFlags()
      if n.len > 0:
        add "\n"

      addComment(false)

      for newIdx, subn in n:
        if trfSkipAuxError in flags and n.kind == nkError and newIdx in {1, 2}:
          continue

        if trfNamedPositions in flags:
          aux(subn, idx & newIdx, n.len, path & n.kind)

        else:
          aux(subn, idx & newIdx, 0, @[])


        if idx.len + 1 > maxDepth:
          break

        if newIdx > maxLen:
          break

        if newIdx < n.len - 1:
          add "\n"

  aux(pnode, @[], 0, @[])


import hmisc/[base_errors, hdebug_misc]
import std/[options, strutils]
import hpprint


type
  Slice = object
    line: int
    startCol: int
    endCol: int

  CodeSegmentKind = enum
    csPlaintext
    csOccur

  CodeLine = object
    lineLen: int
    lineIdx: int
    segments: seq[CodeSegment]

  CodeSegment = object
    slice*: Slice
    case kind*: CodeSegmentKind
      of csPlaintext:
        discard

      of csOccur:
        occur*: int

  CodeBlock = object
    codeLines: seq[CodeLine]

proc contains(s1, s2: Slice): bool =
  s1.line == s2.line and
  s1.startCol <= s2.startCol and s2.endCol <= s2.endCol

proc initSlice*(line, startCol, endCol: int): Slice =
  Slice(line: line, startCol: startCol, endCol: endCol)

proc splitOn(s1, s2: Slice): tuple[before, after: Option[Slice]] =
  if s1.startCol == s2.startCol and s1.endCol == s2.endCol:
    discard

  else:
    if s1.startCol != s2.startCol:
      result.before = some initSlice(s1.line, s1.startCol, s2.startCol - 1)

    if s1.endCol != s2.endCol:
      result.after = some initSlice(s1.line, s2.endCol + 1, s1.endCol)


proc newPlaintextCode*(slice: Slice): CodeSegment =
  CodeSegment(kind: csPlaintext, slice: slice)

proc newOccurCode*(slice: Slice): CodeSegment =
  CodeSegment(kind: csOccur, slice: slice)

proc newCodeBlock*(text: seq[string]): CodeBlock =
  for idx, line in text:
    result.codeLines.add CodeLine(
      lineLen: line.len,
      lineIdx: idx,
      segments: @[newPlaintextCode(initSlice(idx, 0, line.high))]
    )

proc add(main: var seq[CodeSegment], other: CodeSegment) =
  var idx = 0
  while idx < main.len:
    if other.slice in main[idx].slice:
      let split = main[idx].slice.splitOn(other.slice)

      var offset = 0
      if split.before.isSome():
        main.insert(newPlaintextCode(split.before.get()), max(idx - 1, 0))
        inc offset

      main[idx + offset] = other

      if split.after.isSome():
        main.insert(newPlaintextCode(split.after.get()), idx + 1 + offset)

      break

    inc idx

proc add*(code: var CodeBlock, other: CodeSegment) =
  code.codeLines[other.slice.line].segments.add other

func `[]`(base: seq[string], slice: Slice): string =
  base[slice.line][slice.startCol .. slice.endCol]

proc toXml(code: CodeBlock, base: seq[string]): string =
  for line in code.codeLines:
    for segment in line.segments:
      if segment.kind == csOccur:
        result.add "<occur>"
        result.add base[segment.slice]
        result.add "</occur>"

      else:
        result.add base[segment.slice]

    result.add "\n"

when isMainModule:
  startHax()
  let text = "var a: int\nvar b = a"
  #           0123456789

  var main = newCodeBlock(@[text])

  main.add newOccurCode(initSlice(0, 7, 9))
  main.add newOccurCode(initSlice(0, 4, 4))

  echo main.toXml(@[text])

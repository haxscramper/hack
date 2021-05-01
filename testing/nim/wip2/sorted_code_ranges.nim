import hmisc/types/hmap
import hmisc/[base_errors, hdebug_misc]
import std/[options, strutils]
import hpprint


type
  Pos = object
    line: int
    column: int

  PosSlice = object
    start: Pos
    finish: Pos

  CodeSegmentKind = enum
    csPlaintext
    csOccur
    csBody
    csGroup

  CodeSegment = object
    slice*: PosSlice
    case kind*: CodeSegmentKind
      of csPlaintext:
        discard

      of csOccur:
        occur*: int

      of csBody, csGroup:
        bodyId*: int
        subnodes*: Map[PosSlice, CodeSegment]

proc `<`*(p1, p2: Pos): bool = p1.line < p2.line and p1.column < p2.column
proc `<=`*(p1, p2: Pos): bool = p1.line <= p2.line and p1.column <= p2.column
proc contains(s1, s2: PosSlice): bool =
  s1.start <= s2.start and s2.finish <= s2.finish

proc `<`*(p1, p2: PosSlice): bool =
  p1.finish < p2.start

proc initSlice*(start, finish: (int, int)): PosSlice =
  PosSlice(
    start: Pos(line: start[0], column: start[1]),
    finish: Pos(line: finish[0], column: finish[1]))


proc initSlice*(start, finish: Pos): PosSlice =
  PosSlice(start: start, finish: finish)

proc `-`*(pos: Pos, columnOffset: int): Pos =
  assert pos.column > 0
  return Pos(column: pos.column - columnOffset, line: pos.line)


proc `+`*(pos: Pos, columnOffset: int): Pos =
  Pos(column: pos.column + columnOffset, line: pos.line)

proc splitOn(s1, s2: PosSlice): tuple[before, after: Option[PosSlice]] =
  if s1.start == s2.start and s1.finish == s2.finish:
    discard

  else:
    if s1.start != s2.start:
      result.before = some initSlice(s1.start, s2.start - 1)

    if s1.finish != s2.finish:
      result.after = some initSlice(s2.finish + 1, s1.finish)

  # else:
  #   result.before = some initSlice(s1.start, s2.start - 1)
  #   result.after = some initSlice(s2.finish, s1.finish)


proc newPlaintextCode*(slice: PosSlice): CodeSegment =
  CodeSegment(kind: csPlaintext, slice: slice)

proc newOccurCode*(slice: PosSlice): CodeSegment =
  CodeSegment(kind: csOccur, slice: slice)

proc newGroupCode*(segments: seq[CodeSegment]): CodeSegment =
  result = CodeSegment(kind: csGroup)
  for seg in segments:
    result.subnodes[seg.slice] = seg

proc add(main: var CodeSegment, other: CodeSegment) =
  case main.kind:
    of csPlaintext:
      if other.slice in main.slice:
        let split = main.slice.splitOn(other.slice)
        echov split.before
        echov split.after
        echov main.slice
        echov other.slice

        var segments: seq[CodeSegment]
        if split.before.isSome():
          segments.add newPlaintextCode(split.before.get())

        segments.add other

        if split.after.isSome():
          segments.add newPlaintextCode(split.after.get())

        main = newGroupCode(segments)

      else:
        raiseImplementError("")

    of csGroup:
      for key, segment in mpairs(main.subnodes):
        if other.slice in key:
          segment.add other

    else:
      discard

func `[]`(base: seq[string], slice: PosSlice): seq[string] =
  if slice.start.line == slice.finish.line:
    result.add base[slice.start.line][slice.start.column .. slice.finish.column]

  else:
    result.add base[slice.start.line][slice.start.column .. ^1]
    for line in (slice.start.line + 1) .. (slice.finish.line - 1):
      result.add base[line]

    result.add base[slice.finish.line][0 .. slice.finish.column]

proc toXml(segment: CodeSegment, base: seq[string]): string =
  case segment.kind:
    of csGroup:
      for slice, subsegment in segment.subnodes:
        result.add subsegment.toXml(base)

    of csPlaintext:
      result.add base[segment.slice].join("\n")

    of csOccur:
      result.add "<occur>"
      result.add base[segment.slice].join("\n")
      result.add "</occur>"

    of csBody:
      raiseImplementKindError(segment)

when isMainModule:
  startHax()
  let text = "var a: int"
  #           0123456789

  var main = newPlaintextCode(initSlice((0, 0), (0, text.high)))

  main.add newOccurCode(initSlice((0, 7), (0, 9)))
  pprint main

  main.add newOccurCode(initSlice((0, 4), (0, 4)))
  pprint main

  echo main.toXml(@[text])

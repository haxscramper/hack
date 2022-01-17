import std/[tables, strutils, sequtils]

type
  SeqEditKind* = enum
    ## Diff operation kind
    dekDelete
    dekInsert
    dekKeep

  SeqEdit* = object
    ## Single seq edit operations
    kind*: SeqEditKind
    oldpos*: int
    newPos*: int

  SeqShiftKind* = enum
    ## Shifted seq element kind
    dskDelete
    dskInsert
    dskKeep
    dskEmpty

  ShiftedSeqDiff* = object
    ## Intermediate representation of the shifted diff - later fed into
    ## formatting routines, or can be used for testing.
    oldShifted*: seq[tuple[kind: SeqShiftKind, item: int]]
    newShifted*: seq[tuple[kind: SeqShiftKind, item: int]]


proc myersDiff*[T](
    aSeq, bSeq: openarray[T], itemCmp: proc(x, y: T): bool): seq[SeqEdit] =
  ## Generate series of sequence edit operations necessary to trasnform
  ## `aSeq` into `bSeq`. For item equality comparison use `itemCmp`

  # https://gist.github.com/adamnew123456/37923cf53f51d6b9af32a539cdfa7cc4
  var front: Table[int, tuple[x: int, history: seq[SeqEdit]]]
  front[1] = (0, @[])

  template one(idx: int): int = idx - 1

  let
    aMax = len(aSeq)
    bMax = len(bSeq)

  for d in countup(0, aMax + bMax + 1):
    for k in countup(-d, d + 1, 2):
      let goDown =
        (k == -d or (k != d and front[k - 1].x < front[k + 1].x))


      var (x, history) =
        if goDown:
          (front[k + 1].x, front[k + 1].history)

        else:
          (front[k - 1].x + 1, front[k - 1].history)

      var y = x - k

      if 1 <= y and y <= bMax and goDown:
        history.add SeqEdit(kind: dekInsert, newPos: one(y))

      elif 1 <= x and x <= aMax:
        history.add SeqEdit(kind: dekDelete, oldPos: one(x))

      while x < aMax and
            y < bMax and
            itemCmp(aSeq[x], bSeq[y]):

        x += 1
        y += 1
        history.add SeqEdit(kind: dekKeep, oldPos: one(x), newPos: one(y))

      if x >= aMax and y >= bMax:
        return history

      else:
        front[k] = (x, history)

proc shiftDiffed*[T](
    diff: seq[SeqEdit], oldSeq, newSeq: openarray[T]): ShiftedSeqDiff =

  for line in items(diff):
    case line.kind:
      of dekDelete:
        result.oldShifted.add((dskDelete, line.oldPos))

      of dekInsert:
        result.newShifted.add((dskInsert, line.newPos))

      of dekKeep:
        var
          oldLen = result.oldShifted.len
          newLen = result.newShifted.len

        if oldLen < newLen:
          while oldLen < newLen:
            result.oldShifted.add((dskEmpty, 0))
            inc oldLen

        elif newLen < oldLen:
          while newLen < oldLen:
            result.newShifted.add((dskEmpty, 0))
            inc newLen

        result.oldShifted.add((dskKeep, line.oldPos))
        result.newShifted.add((dskKeep, line.newPos))


proc dollar[T](arg: T): string = $arg

proc formatDiffed*(
    shifted: ShiftedSeqDiff,
    oldSeq, newSeq: seq[string],
    maxUnchanged: int           = 5,
    maxUnchangedWords: int      = high(int),
    showLines: bool             = false,
    stackLongLines: int         = high(int)
  ): string                     =

  ## - `stackLongLines` :: If any of two diffed lines are longer than
  ##   threshold, display then one on top of another instead of side by
  ##   side

  var
    oldText, newText: seq[string]
    lhsMax = 0

  let maxLhsIdx = len($shifted.oldShifted[^1].item)
  let maxRhsIdx = len($shifted.newShifted[^1].item)

  proc editFmt(fmt: SeqShiftKind, idx: int, isLhs: bool): string =
    if showLines:
      let num =
        if fmt == dskEmpty:
          align(" ", maxLhsIdx)

        elif isLhs:
          align($idx, maxLhsIdx)

        else:
          align($idx, maxRhsIdx)

      case fmt:
        of dskDelete: "- " & num
        of dskInsert: "+ " & num
        of dskKeep: "~ " & num
        of dskEmpty: "? " & num

    else:
      case fmt:
        of dskDelete: "- "
        of dskInsert: "+ "
        of dskKeep: "~ "
        of dskEmpty: "? "


  var unchanged = 0
  for (lhs, rhs) in zip(shifted.oldShifted, shifted.newShifted):
    var add = false
    if lhs.kind == dskKeep and rhs.kind == dskKeep:
      if unchanged < maxUnchanged:
        add = true
        inc unchanged

    else:
      add = true
      unchanged = 0

    if add:
      oldText.add editFmt(lhs.kind, lhs.item, true)
      newText.add editFmt(rhs.kind, rhs.item, false)

    if lhs.kind == dskDelete and rhs.kind == dskInsert:
      oldText[^1].add oldSeq[lhs.item]
      newText[^1].add newSeq[rhs.item]


    elif rhs.kind == dskInsert:
      oldText[^1].add oldSeq[lhs.item]
      newText[^1].add newSeq[rhs.item]

    elif lhs.kind == dskDelete:
      oldText[^1].add oldSeq[lhs.item]
      newText[^1].add newSeq[rhs.item]

    else:
      if add:
        oldText[^1].add oldSeq[lhs.item]
        newText[^1].add newSeq[rhs.item]

    if add:
      lhsMax = max(oldText[^1].len, lhsMax)

  for (lhs, rhs) in zip(oldtext, newtext):
    if max(len(lhs), len(rhs)) > stackLongLines:
      result.add "@@"
      result.add lhs
      result.add "\n@@"
      result.add rhs
      result.add "\n"

    else:
      result.add alignLeft(lhs, lhsMax + 3)
      result.add rhs
      result.add "\n"


proc myersDiff*[T](aSeq, bSeq: openarray[T]): seq[SeqEdit] =
  ## Diff overload without explicit comparator proc - use default `==` for
  ## two items.
  myersDiff(aSeq, bSeq, proc(a, b: T): bool = a == b)

proc diffText(text1, text2: seq[string]): string =
  ## Format diff of two text lines
  myersDiff(text1, text2).shiftDiffed(text1, text2).formatDiffed(text1, text2)

echo diffText(@["test"], @["text"])

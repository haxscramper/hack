#import terminal
import sequtils
# import strutils
# import strformat
import wiringPiNim
import algorithm
#import os
import common
import key_codes
import bitops

when defined(profiler):
  import nimprof

if piSetup() >= 0:
  echo "Pi setup ok"
else:
  echo "Pi setup failed"
  quit 1

type
  HIDModifiers = enum
    hmLeftCtrl = 0
    hmLeftShift = 1
    hmLeftAlt = 2
    hmLeftSuper = 3

    hmRightCtrl = 4
    hmRightShift = 5
    hmRightAlt = 6
    hmRightSuper = 7

type
  KeyState = enum
    kstChangedPressed ## Previously unmodified now pressed
    kstChangedReleased ## Previously pressed now released
    kstIdlePressed ## Previously pressed no change now
    kstIdleReleased ## Not pressed no change

  Key = object
    state: KeyState
    case isModifier: bool
    of true: modif: HIDModifiers
    of false: code: KeyCode

  KeyGrid = object
    keyGrid: seq[seq[Key]]
    rowPins: seq[int]
    colPins: seq[int]

proc `[]`(grid: var KeyGrid, row, col: int): var Key =
  result = grid.keyGrid[row][col]

proc updateKey(key: var Key, isPressed: bool): bool =
  result = false

  case key.state:
    of kstIdleReleased:
      if isPressed:
        key.state = kstChangedPressed
        result = true # Key is now pressed

    of kstIdlePressed:
      if not isPressed:
        key.state = kstChangedReleased
        result = true # Key is not released

    of kstChangedPressed:
      if isPressed:
        key.state = kstIdlePressed
        # Key is still pressed, no changes

    of kstChangedReleased:
      if not isPressed:
        # Key is still released, no changes
        key.state = kstIdleReleased

proc makeKeyGrid(rowPins, colPins: seq[int]): KeyGrid =
  KeyGrid(
    keyGrid: newSeqWith(
      rowPins.len, newSeqWith(
        colPins.len, Key(
          state: kstIdleReleased,
          isModifier: false,
          code: ccKeyA))),
    rowPins: rowPins,
    colPins: colPins)

proc hasDifferentValues[T](arr: seq[T]): bool {.compiletime.} =
  var sorted = arr
  sorted.sort()
  for idx, item in sorted[1 ..^ 1]:
    if sorted[idx] == item:
      return false

  return true

proc makeKeyGrid(
  codes: static seq[seq[KeyCode]],
  rowPins, colPins: static seq[int]
     ): KeyGrid =
  static:
    assert codes.len == rowPins.len
    assert codes[0].len == colPins.len
    assert rowPins.hasDifferentValues()
    assert colPins.hasDifferentValues()

  KeyGrid(
    keyGrid: newSeqWith(
      rowPins.len, newSeqWith(
        colPins.len, Key(
          state: kstIdleReleased,
          isModifier: false,
          code: ccKeyA))),
    rowPins: rowPins,
    colPins: colPins)


proc readMatrix(grid: KeyGrid): seq[seq[bool]] =
  ## Scan grid matrix into 2d sequence of button states. `true`
  ## indicates that button is pressed, `false` indicates that button
  ## is released. Returned 2d sequence is of size `[grid.rowPins.len,
  ## grid.colPins.len]`

  result =
    newSeqWith(
      grid.rowPins.len,
      newSeqWith(grid.colPins.len, false))

  for colIdx, colPin in grid.colPins:
    setPinModeOut(colPin)
    digitalWrite(colPin, false)

    for rowIdx, rowPin in grid.rowPins:
      setPinModeIn(rowPin)
      setPinPullUp(rowPin)

      let state = digitalRead(rowPin)
      #echo &"read {state} from {rowIdx}({rowPin}) {colIdx}({colPin})"
      result[rowIdx][colIdx] = not state

      setPinPullOff(rowPin)

    setPinModeIn(colPin)

type
  HIDReport = object
    modifiers: set[HIDModifiers]
    keycodes: array[6, KeyCode]

proc createReport(grid: KeyGrid): HIDReport =
  # TODO comment case-of for key state switching
  var keys: seq[KeyCode]
  for keyRow in grid.keyGrid:
    for key in keyRow:
      case key.state:
        of kstChangedReleased:
          if not key.isModifier:
            keys.add(ccKeyNone)

        of kstChangedPressed:
          if key.isModifier:
            result.modifiers.incl(key.modif)
          else:
            keys.add(key.code)

        else:
          discard

  for idx, keyCode in keys:
    if idx < result.keycodes.len:
      result.keycodes[idx] = keyCode

proc writeHIDReport(report: HIDReport) =
  var modifiers: uint8 = 0
  for it in report.modifiers:
    modifiers.setBit(ord(it))

  var final: array[8, uint8]

  final[0] = modifiers
  final[1] = 0 # ignored

  for idx, code in report.keycodes:
    final[2 + idx] = cast[uint8](code)

  let file = open("/dev/hidg0", fmWrite)
  discard file.writeBytes(final, 0, 8)
  file.close()


proc updateKeyGrid(grid: var KeyGrid, matrixState: seq[seq[bool]]): bool =
  var anyChanges = false

  for rowIdx, rowState in matrixState:
    for keyIdx, keyState in rowState:
      let isChanged = grid.keyGrid[rowIdx][keyIdx].updateKey(keyState)
      anyChanges = anyChanges or isChanged

  return anyChanges

var grid = makeKeyGrid(
  codes = @[
    @[ccKeyA, ccKeyB, ccKey0],
    @[ccKeyJ, ccKeyU, ccKey8],
    @[ccKeyH, ccKeyE, ccKeyN]
  ],
  rowPins = @[0, 1, 2],
  colPins = @[3, 4, 5]
)

for col in grid.colPins:
  setPinModeOut(col)
  setPinPullUp(col)

for row in grid.rowPins:
  setPinModeIn(row)
  #setPinPullDown(row)

var cnt = 0
while true:
  let matrixState = grid.readMatrix()
  let anyChanges = updateKeyGrid(grid, matrixState)

  if anyChanges:
    inc cnt
    let report = grid.createReport()
    report.writeHIDReport()

  if cnt > 100:
    break

import os

import terminal
import sequtils
import strutils
import strformat
import macros
import algorithm
import os
import common
import key_codes
import bitops
import hmisc/helpers
import report

import grid

when defined(profiler):
  import nimprof


proc getChangeFromDefault(state: string, grid: KeyGrid
                         ): tuple[reports: seq[HIDReport], anyChanges: bool] =
  ## Take empty button grid (with all keys released). Transition to
  ## state show by `state`. Report all key changes in form of output
  ## HID report.
  let rows = state.split("|")
  asserteq rows.len, grid.keyGrid.len
  for (stateRow, gridRow) in zip(rows, grid.keyGrid):
    asserteq stateRow.len, gridRow.len

  var gridCopy = grid

  let boolState: seq[seq[bool]] = # Generate mock scan run
    rows.mapIt(it.mapIt(it == '1').concat())

  let anyChanges: bool = # Check for changes
    updateKeyGrid(gridCopy, boolState)

  let gridReport = # Create report on modified grid
    gridCopy.createReports()

  result = (gridReport, anyChanges)


macro transitionAssert(grid, assertionList: untyped): untyped =
  ## Convert each line into assertion: if empty grid (all keys
  ## released and in default position) after transition to state
  ## denoted by left side of the `->` will generate the same hid
  ## report as keq combination denoted by the right side then
  ## assertion succede. For example, if we had a 2x2 grid then
  ## `"01|00" -> "n"` would assert that, given argument grid and upper
  ## right key pressed we will get hid report that sends `"n"`.
  # defer: echo result.toStrLit()

  result = newStmtList()
  result.add quote do:
    var hasErrors {.inject.} = false
    printGrid(`grid`)

  for transition in assertionList:
    if not (transition.kind == nnkInfix and transition[0].strVal == "->"):
      raise newException(ValueError, "each element has to be infix transition")

    let lhs = transition[1]
    let rhs = transition[2]
    result.add quote do:
      block:
        let state {.inject.} = `lhs`
        let report {.inject.} = `rhs`
        let (gridReport {.inject.}, _) = getChangeFromDefault(state, `grid`)
        let targetReport {.inject.} = fromKeybindingStr(report)
        if not (gridReport == targetReport):
          echo &"Transition '{state}' -> '{report}' has failed"
          echo "Grid:"
          printGrid(`grid`)
          echo "Grid hid report:"
          printHIDReport(gridReport)
          echo "Target hid report:"
          printHIDReport(targetReport)

          hasErrors = true

  result.add quote do:
    if hasErrors:
      raise newException(AssertionError, "one of the transitions failed")
    else:
      echo "all transitions are ok"


  result = quote do:
    block:
      `result`


proc `[]`(grid: var KeyGrid, row, col: int): var Key =
  ## Access one key in grid.
  result = grid.keyGrid[row][col]

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


block:
  var testGrid = parseGridConfig("""
rowPins = [0, 1, 2]
colPins = [3, 4, 5]

[[row]]
keys = ["M-", "C-", "s-"]

[[row]]
keys = ["S-", ",", "C-c C-c"]

[[row]]
keys = ["H", "E", "N"]
""")

  testGrid.printGrid()

  # transitionAssert testGrid:
  #   "010|000|001" -> "C-n"
  #   "111|100|001" -> "C-S-s-M-n"
  #   "111|100|010" -> "C-S-s-M-e"
  #   "000|001|000" -> "C-c C-c"


#[
proc main() =
  var grid = makeKeyGrid(
    codes = @[
      @[ccKeyA, ccKeyB, ccKey0],
      @[ccKeyJ, ccKeyU, ccKey8],
      @[ccKeyH, ccKeyE, ccKeyN]
    ],
    rowPins = @[0, 1, 2],
    colPins = @[3, 4, 5]
  )

  if piSetup() >= 0:
    echo "Pi setup ok"
  else:
    echo "Pi setup failed"
    quit 1

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
      let reports = grid.createReports()
      for rep in reports:
        writeHIDReport(rep)

    if cnt > 100:
      break

# main()
echo "done"
]#

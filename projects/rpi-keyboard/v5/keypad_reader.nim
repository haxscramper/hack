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



proc willGenerate(state: string, grid: KeyGrid, report: string): bool =
  let rows = state.split("|")
  asserteq rows.len, grid.keyGrid.len
  for (stateRow, gridRow) in zip(rows, grid.keyGrid):
    asserteq stateRow.len, gridRow.len

  var gridCopy = grid
  let boolState: seq[seq[bool]] = rows.mapIt(
    it.mapIt((it == '0').tern(false, true)).concat()
  )
  let anyChanges: bool = updateKeyGrid(gridCopy, boolState)
  if anyChanges:
    let gridReport = gridCopy.createReport()
    return gridReport == report.fromEmacsNotation()

  true


macro transitionAssert(head, body: untyped): untyped =
  defer: echo result.toStrLit()

  result = newStmtList()
  result.add quote do:
    var hasErrors {.inject.} = false

  for transition in body:
    if not (transition.kind == nnkInfix and transition[0].strVal == "->"):
      raise newException(ValueError, "each element has to be infix transition")

    let lhs = transition[1]
    let rhs = transition[2]
    result.add quote do:
      block:
        let state {.inject.} = `lhs`
        let report {.inject.} = `rhs`
        if not (state.willGenerate(`head`, report)):
          echo &"Transition '{state}' -> '{report}' has failed"
          echo "State hid report:"

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
  var testGrid = makeKeyGrid(
    codes = @[
      @[ccKeyLEFTALT, ccKeyLEFTCTRL, ccKeyLEFTMETA],
      @[ccKeyLEFTSHIFT, ccKeyCOMMA, ccKeyA],
      @[ccKeyH, ccKeyE, ccKeyN]
    ],
    rowPins = @[0, 1, 2],
    colPins = @[3, 4, 5]
  )

  transitionAssert testGrid:
    "010|000|001" -> "C-n"



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
      let report = grid.createReport()
      report.writeHIDReport()

    if cnt > 100:
      break

main()

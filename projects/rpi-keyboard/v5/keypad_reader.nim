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
import hmisc/[helpers, defensive]
import report

import grid

initDefense(
  logPath = true
)

when defined(profiler):
  import nimprof

## .. include:: notes.rst

proc getChangeFromDefault(state: string, grid: KeyGrid
                         ): tuple[reports: seq[HIDReport], anyChanges: bool] =
  ## Take empty button grid (with all keys released). Transition to
  ## state show by `state`. Report all key changes in form of output
  ## HID report.
  let rows = state.split("|")
  assert rows.len == grid.keyGrid.len
  for (stateRow, gridRow) in zip(rows, grid.keyGrid):
    assert stateRow.len == gridRow.len

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

    runTempConfigLock((lcUseFileName, false)): runIndentedLog:
      for rowIdx, rowPin in grid.rowPins:
        setPinModeIn(rowPin)
        # setPinPullUp(rowPin)

        let state = digitalRead(rowPin)
        if not state:
          showInfo &"Reading LOW from [{rowIdx}][{colIdx}]"

        result[rowIdx][colIdx] = not state

        # setPinPullDown(rowPin)

    digitalWrite(colPin, true)
    # setPinModeIn(colPin)

  showInfo "Matrix read completed"


block:
  var testGrid = parseGridConfig("config.json".readFile())

  # testGrid.printGrid()

  # transitionAssert testGrid:
  #   "010|000|001" -> "C-n"
  #   "111|100|001" -> "C-S-s-M-n"
  #   "111|100|010" -> "C-S-s-M-e"
  #   "000|001|000" -> "C-c C-c"

proc `/`(up, down: string): string = joinpath(up, down)

proc main() =
  when useMock:
    showWarn("Using keyboard simulation")

    let dir = "../../ngspice_digital_read"

    ngReadCircuit(dir / "key-grid.net")
    ngAddIncluded(@[
      dir / "io-pin.net",
      dir / "on-off-switch.net"
    ])

    ngSilentSimulation()

  var grid = makeKeyGrid(
    codes = @[
      @[ccKeyA, ccKeyB, ccKey0, ccKeyZ],
      @[ccKeyJ, ccKeyU, ccKey8, ccKeyQ],
      @[ccKeyH, ccKeyE, ccKeyN, ccKey9]
    ],
    colPins = @[1, 2, 3, 4],
    rowPins = @[5, 6, 7],
  )

  if piSetup() >= 0:
    showInfo "Pi setup ok"
  else:
    showError "Pi setup failed"
    die()

  for col in grid.colPins:
    setPinModeOut(col)
    setPinPullUp(col)

  for row in grid.rowPins:
    setPinModeIn(row)
    #setPinPullDown(row)


  when useMock:
    setSwitch(row = 1, col = 1, state = true)

  runIndentedLog:
    while true:
      let matrixState = grid.readMatrix()
      let anyChanges = updateKeyGrid(grid, matrixState)

      if anyChanges:
        showInfo "Keyboard change detected"
        let reports = grid.createReports()
        for rep in reports:
          writeHIDReport(rep)

      break

when isMainModule:
  pprintErr():
    main()

  showInfo "done"

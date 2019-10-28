import macros, tables, strformat, strutils, sequtils

import re
import os
import terminal

proc setChar(x: int, y: int, c: char = '@') =
  setCursorPos(x, y)
  stdout.write(c)


proc printOutline() =
  let w = terminalWidth()
  let h = terminalHeight()
  for row in 0..h:
    setChar(0, row, '|')
    setChar(w, row, '|')

  for col in 0..w:
    setChar(col, 0, '=')
    setChar(col, h, '=')

proc generateGrid(): seq[seq[bool]] =
  newSeqWith(
    terminalWidth() - 2,
    newSeqWith(terminalHeight() - 2, false))

proc printGrid(grid: seq[seq[bool]]) =
  eraseScreen()
  printOutline()
  for x in 0..<grid.len:
    for y in 0..<grid[x].len:
      setChar(x + 1, y + 1, if grid[x][y]: '#' else: ' ')


proc getVal(grid: seq[seq[bool]], x, y: int): bool =
  return
    (0 <= x and x < grid.len) and
    (0 <= y and y < grid[0].len) and
    grid[x][y]

proc `+=`(lhs: var int, rhs: bool) =
  if rhs: inc lhs else: discard

proc doConwayStep(grid: seq[seq[bool]]): seq[seq[bool]] =
  result = grid
  for x in 0..<grid.len:
    for y in 0..<grid[x].len:
      var adj: int = 0
      adj += grid.getVal(x - 1, y - 1)
      adj += grid.getVal(x - 1, y + 1)
      adj += grid.getVal(x + 1, y - 1)
      adj += grid.getVal(x + 1, y + 1)

      adj += grid.getVal(x, y - 1)
      adj += grid.getVal(x, y + 1)
      adj += grid.getVal(x - 1, y)
      adj += grid.getVal(x + 1, y)
      if grid[x][y]: # Cell is alive:
        if adj < 2:
          result[x][y] = false
        elif adj < 4:
          result[x][y] = true
        else:
          result[x][y] = false
      else:
        if adj == 3: result[x][y] = true



var grid = generateGrid()

hideCursor()

proc subReplace[T](
  sourc: var seq[seq[T]],
  start: tuple[x,y: int],
  input: seq[seq[T]]) =
  for x in 0 ..< input.len:
    for y in 0 ..< input[x].len:
      sourc[x + start.x][y + start.y] = input[x][y]


let decoded = "pattern.rle"
  .readFile()
  .split('\n')[2..^1]
  .join("")
  .split('$')
  .mapIt(
    it
    .findAll(re"(\d*b|\d*o)")
    .mapIt(
      if it =~ re"(\d+)[bo]": (matches[0].parseInt(), it[^1])
      else: (1, it[^1]))
    .mapIt(it[1].repeat(it[0]))
    .join("")
    .mapIt(it == 'o'))

grid.subReplace((0,0), decoded)


while true:
  printGrid(grid)
  grid = grid.doConwayStep()
  sleep 100

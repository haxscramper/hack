type
  GridFlags = enum
    singleSeq
    nestedSeq

  Grid[T; Flags: static[set[GridFlags]]] = object
    rows, cols: int
    when singleSeq in Flags:
      elems: seq[T]
    else:
      grid: seq[seq[T]]


func initGrid[T, Flags](rows, cols: int): Grid[T, Flags] =
  when {singleSeq, nestedSeq} <= Flags:
    {.error: "Only single flag is allowed in grid flags".}
  elif len({singleSeq, nestedSeq} - Flags) == 0:
    {.error: "At least one layout configuration flag is needed".}

  when singleSeq in Flags:
    result.elems.setLen(rows * cols)
  else:
    for row in rows:
      result.grid.add @[]
      result.grid[^1].setLen(cols)

func `[]`[T, Flags](grid: Grid[T, Flags], row, col: int): T =
  when singleSeq in Flags:
    return grid.elems[row * grid.rows + col]
  else:
    return grid.grid[row][col]


var test = initGrid[int, {singleSeq}](4, 4)
echo test.elems
# echo test[0, 0]

import std/[sugar, tables, sequtils, strutils, strformat, algorithm]


type
  Edit = enum None, Insert, Delete, Transpose, Change, Keep
  Cell = tuple[cost: int, edit: Edit]
  Matrix = seq[seq[Cell]]

proc format(m: Matrix, a, b: string, offset: int = 2): string =
  result.add "  "
  result.add repeat("    ", offset)
  for ch in b:
    result.add align($ch, 4)

  result.add "\n"

  for lineIdx, line in m:
    if (offset - 1) < lineIdx:
      result.add align($a[lineIdx - offset], 3)

    else:
      result.add "   "

    for colIdx, item in line:
      result.add align($item.cost, 3)
      result.add case item.edit:
        of None: "\e[35m?\e[39m"
        of Insert: "\e[32m+\e[39m"
        of Delete: "\e[31m-\e[39m"
        of Transpose: "\e[36m%\e[39m"
        of Change: "\e[33m~\e[39m"
        of Keep: "."

    result.add "\n"

func cell(cost: int): Cell = (cost, None)
func cell(cost: int, edit: Edit): Cell = (cost, edit)


func min(cells: seq[Cell]): Cell =
  result = cells[0]
  for cell in cells:
    if cell.cost < result.cost:
      result = cell


proc damerauLevenshteinDistance(a, b: string): Matrix =
    # "Infinity" -- greater than maximum possible edit distance
    # Used to prevent transpositions for first characters
    let inf = len(a) + len(b)

    # Matrix: (M + 2) x (N + 2)
    var
      matrix: Matrix = @[newSeqWith(len(b) + 2, cell(inf))]

    matrix &= @[cell(inf)] & mapIt(0 .. len(b), cell(it))
    var tmp = collect(newSeq):
      for m in 1 .. len(a):
        @[cell(inf), cell(m)] & repeat(cell(0), len(b))

    matrix &= tmp

    # Holds last row each element was encountered: `DA` in the Wikipedia pseudocode
    var lastRow: Table[char, int]

    const
      insCost = 1
      delCost = 1
      changeCost = 1
      transposeCost = 1

    # Fill in costs
    for row in 1 .. len(a):
        # Current character in `a`
        var chA = a[row-1]

        # Column of last match on this row: `DB` in pseudocode
        var lastMatchCol = 0

        for col in 1 .. len(b):
            # Current character in `b`
            var chB = b[col - 1]

            # Last row with matching character; `i1` in pseudocode
            var lastMatchingRow = lastRow.getOrDefault(chB, 0)

            # Compute substring distance
            matrix[row + 1][col + 1] = min(@[
              (
                if chA == chB:
                  cell(matrix[row][col].cost, Keep)

                else:
                  # Substitution
                  cell(matrix[row][col].cost + changeCost, Change)
              ),
              cell(matrix[row + 1][col].cost + insCost, Insert),  # Addition
              cell(matrix[row][col + 1].cost + delCost, Delete),  # Deletion

              # Transposition
              cell(
                matrix[lastMatchingRow][lastMatchCol].cost +
                  (row - lastMatchingRow - 1) + transposeCost +
                  (col - lastMatchCol - 1),
                Transpose
              )
              ]
            )

            # If there was a match, update lastMatchCol
            # Doing this here lets me be rid of the `j1` variable from the original pseudocode
            if chA == chB:
              lastMatchCol = col

        # Update last row for current character
        lastRow[chA] = row

    # Return last element
    return matrix


proc levenshteinDistance*(str1, str2: string): Matrix =
  var
    l1 = str1.len
    l2 = str2.len
    m: Matrix = newSeqWith(l1 + 1, newSeqWith(l2 + 1, cell(0)))

  for i in 0 .. l1:
    m[i][0] = cell(i)

  for j in 0 .. l2:
    m[0][j] = cell(j)

  const
    insertCost = 1
    deleteCost = 1
    changeCost = 1

  for i in 1 .. l1:
    for j in 1 .. l2:
      if (str1[i - 1] == str2[j - 1]):
        m[i][j] = m[i - 1][j - 1]

      else:
        m[i][j] = min(@[
          cell(m[i - 1][j].cost + deleteCost, Delete),
          cell(m[i][j - 1].cost + insertCost, Insert),
          cell(m[i - 1][j - 1].cost + changeCost, Change)])

  return m


type
  SeqEdit* = object
    kind*: Edit
    pos*: (int,int)

func `[]`(matrix: Matrix, pos: (int, int)): Cell = matrix[pos[0]][pos[1]]

func step*(matrix: Matrix, pos: (int, int)): (int, int) =
  let (c, r) = pos
  result = pos
  var cost = high(int)


  if 0 < c and 0 < r and matrix[(c - 1, r - 1)].cost < cost:
    result = (c - 1, r - 1)
    cost = matrix[result].cost

  if 0 < r and matrix[(c, r - 1)].cost < cost:
    result = (c, r - 1)
    cost = matrix[result].cost

  if 0 < c and matrix[(c - 1, r)].cost < cost:
    result = (c - 1, r)
    cost = matrix[result].cost


proc traceEdits(matrix: Matrix, offset: int): seq[SeqEdit] =
  var pos = (matrix.high, matrix[^1].high)
  while (offset, offset) < pos:
    pos = matrix.step(pos)
    result.add SeqEdit(
      kind: matrix[pos].edit,
      pos: (pos[0] - offset, pos[1] - offset))

  return result.reversed()

proc applyEdits(a, b: string, edits: seq[SeqEdit]): string =
  result = a
  for edit in edits:
    let (pa, pb) = edit.pos
    case edit.kind:
      of Change:
        result[pa] = b[pb]

      of Keep:
        discard

      of Insert:
        result.insert($b[pb], pa + 1)

      else:
        discard

    echo result

block:
  let (a, b) = ("democrat", "republican")
  let ed = damerauLevenshteinDistance(a, b)
  echo ed.format(a, b)
  let edits = ed.traceEdits(2)
  for ed in edits:
    echo ed

  discard applyEdits(a, b, edits)

echo levenshteinDistance("a cat", "an act").format("a cat", "an act", 1)

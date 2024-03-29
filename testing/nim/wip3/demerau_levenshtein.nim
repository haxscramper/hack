import std/[sugar, tables, sequtils, strutils, strformat]


type
  Edit = enum None, Insert, Delete, Transpose, Change
  Cell = tuple[cost: int, edit: Edit]
  Matrix = seq[seq[Cell]]

proc format(m: seq[seq[Cell]]): string =
  for line in m:
    for item in line:
      result.add align($item.cost, 3)
      result.add case item.edit:
        of None: "\e[35m?\e[39m"
        of Insert: "\e[32m+\e[39m"
        of Delete: "\e[31m-\e[39m"
        of Transpose: "\e[36m%\e[39m"
        of Change: "\e[33m~\e[39m"


    result.add "\n"

func cell(cost: int): Cell = (cost, None)
func cell(cost: int, edit: Edit): Cell = (cost, edit)


func min(cells: seq[Cell]): Cell =
  result = cells[0]
  for cell in cells:
    if cell.cost < result.cost:
      result = cell


proc damerau_levenshtein_distance(a, b: string): Matrix =
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

    echo format(matrix)

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
                cell(
                  matrix[row][col].cost +
                  (if chA == chB: 0 else: changeCost),
                  Change), # Substitution
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

echo damerau_levenshtein_distance("a cat", "an act").format()

import std/[sugar, tables, sequtils, strutils]

proc format[T](m: seq[seq[T]], width: int = 3): string =
  for line in m:
    for item in line:
      result.add align($item, width)

    result.add "\n"

proc damerau_levenshtein_distance(a, b: string): seq[seq[int]] =
    # "Infinity" -- greater than maximum possible edit distance
    # Used to prevent transpositions for first characters
    let inf = len(a) + len(b)

    # Matrix: (M + 2) x (N + 2)
    var
      matrix: seq[seq[int]]  = @[newSeqWith(len(b) + 2, inf)]

    matrix &= @[inf] & toSeq(0 .. len(b))
    var tmp = collect(newSeq):
      for m in 1 .. len(a):
        @[inf, m] & repeat(0, len(b))

    matrix &= tmp

    echo format(matrix)

    # Holds last row each element was encountered: `DA` in the Wikipedia pseudocode
    var lastRow: Table[char, int]

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

            # Cost of substitution
            var cost = if chA == chB: 0 else: 1

            # Compute substring distance
            matrix[row + 1][col + 1] = min(@[
                matrix[row][col] + cost, # Substitution
                matrix[row + 1][col] + 1,  # Addition
                matrix[row][col + 1] + 1,  # Deletion

                # Transposition
                matrix[lastMatchingRow][lastMatchCol] +
                   (row - lastMatchingRow - 1) + 1 +
                   (col - lastMatchCol - 1)
              ]
            )

            # If there was a match, update lastMatchCol
            # Doing this here lets me be rid of the `j1` variable from the original pseudocode
            if cost == 0:
                lastMatchCol = col

        # Update last row for current character
        lastRow[chA] = row

    # Return last element
    return matrix

echo damerau_levenshtein_distance("a cat", "an act").format()

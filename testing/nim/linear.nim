import std/[enumerate, strformat, sugar]

type
  Matrix = seq[seq[float]]
  Column = seq[float]
  Row = seq[float]

iterator rows(m: Matrix): Row =
  for row in items(m):
    yield row

iterator mrows(m: var Matrix): var Row =
  for row in mitems(m):
    yield row

iterator cols(m: Matrix): Column =
  for colidx in 0 .. m[0].high:
    var result: Column
    for idx, row in pairs(m):
      result[idx] = row[colidx]

    yield result

iterator colIdx(m: Matrix): int =
  for i in 0 .. m[0].high:
    yield i

iterator rowIdx(m: Matrix): int =
  for i in 0 .. m.high:
    yield i

var a = @[
  @[1.0, 2.0, 3.0, 1.0],
  @[2.0, 3.0, 4.0, 1.0],
  @[4.0, 5.0, 4.0, 1.0]
]

const n = 3

block:
  for i in rowIdx(a):
    for j in i + 1 ..< n:
      let ratio = a[j][i] / a[i][i]

      for k in 0 .. n:
        a[j][k] = a[j][k] - ratio * a[i][k]

  var x: array[n, float]

  x[n - 1] = a[n - 1][n] / a[n-1][n - 1]

  for i in countdown(n - 2, 0):
    x[i] = a[i][n]

    for j in i + 1 ..< n:
      x[i] = x[i] - a[i][j] * x[j]

    x[i] = x[i]/a[i][i]

  echo x

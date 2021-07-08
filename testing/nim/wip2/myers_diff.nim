import
  std/[sequtils, tables, strutils],
  hmisc/other/hpprint,
  hmisc/types/colorstring,
  hmisc/extra/hdrawing/hdrawing


func sideBySide*(str1, str2: string): string =
  concatBufsLeft(@[
    str1.toTermBuf(),
    makeTermBuf(w = 3, h = 1),
    str2.toTermBuf()
  ]).toString()



type
  EditKind = enum
    eDelete
    eInsert
    eKeep

  Edit = object
    kind: EditKind
    pos: int
    newPos: int


when false:
  proc diff(e, f: seq[string], i, j: int): seq[Edit] =
    #  Documented at http://blog.robertelder.org/diff-algorithm/
    let (N, M, L, Z) = (len(e), len(f), len(e) + len(f), 2 * min(len(e), len(f)) + 2)

    if N > 0 and M > 0:
      let (w, g, p) = (N - M, repeat(0, Z), repeat(0, Z))

      for h in 0 ..< (L div 2 + int(L mod 2 != 0)) + 1:
        for r in 0 ..< 2:
          var (c, d, o, m) = if r == 0: (g, p, 1, 1) else: (p, g, 0, -1)

          for k in countup( - (h - 2*max(0,h - M)), h - 2*max(0,h - N)+1, 2):
            var
              a =
                if c[(k + 1) mod Z].bool():
                  int(k == -h or k != h and c[(k - 1) mod Z] < c[(k + 1) mod Z])

                 else:
                   c[(k - 1) mod Z] + 1

              b = a - k
              s = a
              t = b

            while
              a < N and
              b < M and
              e[(1 - o) * N + m * a + (o - 1)] == f[(1 - o) * M + m * b + (o - 1)]:

              (a, b) = (a + 1, b + 1)

            c[k mod Z] = a
            let z = -(k - w)

            if L mod 2 == o and
               z >= -(h - o) and
               z <= h - o and
               c[k mod Z] + d[z mod Z] >= N:

              let (D, x, y, u, v) =
                if o == 1:
                  (2 * h - 1, s, t, a, b)
                else:
                  (2 * h, N - a, M - b, N - s, M - t)

              if D > 1 or (x != u and y != v):
                return diff(e[0 .. x], f[0 .. y], i, j) & diff(e[u .. N], f[v .. M],i + u,j + v)

              elif M > N:
                return diff(@[], f[N .. M], i + N, j + N)

              elif M < N:
                return diff(e[M .. N],@[], i + M, j + M)

              else:
                return @[]

    elif N > 0: #  Modify the return statements below if you want a different edit script format
      for n in 0 ..< N:
        result.add Edit(kind: eDelete, positionOld: i + n)
      # return [{"operation": "delete", "position_old": i+n} for n in range(0,N)]
    else:
      for n in 0 ..< M:
        result.add Edit(kind: eInsert, positionOld: i, positionNew: j + n)
      # return [{"operation": "insert", "position_old": i,"position_new":j+n} for n in range(0,M)]

proc myersDiff[T](aSeq, bSeq: openarray[T]): seq[Edit] =
  # https://gist.github.com/adamnew123456/37923cf53f51d6b9af32a539cdfa7cc4
  var front: Table[int, tuple[x: int, history: seq[Edit]]]
  front[1] = (0, @[])

  template one(idx: int): int = idx - 1

  let
    aMax = len(aSeq)
    bMax = len(bSeq)

  for d in countup(0, aMax + bMax + 1):
    for k in countup(-d, d + 1, 2):
      let goDown =
        (k == -d or (k != d and front[k - 1].x < front[k + 1].x))


      var (x, history) =
        if goDown:
          (front[k + 1].x, front[k + 1].history)

        else:
          (front[k - 1].x + 1, front[k - 1].history)

      var y = x - k

      if 1 <= y and y <= bMax and goDown:
        history.add Edit(kind: eInsert, pos: one(y))

      elif 1 <= x and x <= aMax:
        history.add Edit(kind: eDelete, pos: one(x))

      while x < aMax and y < bMax and aSeq[one(x + 1)] == bSeq[one(y + 1)]:
        x += 1
        y += 1
        history.add Edit(kind: eKeep, pos: one(x), newPos: one(y))

      if x >= aMax and y >= bMax:
        return history

      else:
        front[k] = (x, history)

when isMainModule:
  let
    oldText = @["apple","orange","pear", "text"]
    newText =  @["apple","orange","blueberry", "potato", "text"]

  let d = myersDiff(oldText, newText)

  var oldDiff, newDiff: seq[string]


  for line in d:
    case line.kind:
      of eDelete:
        oldDiff.add "- " & oldText[line.pos]

      of eInsert:
        newDiff.add "+ " & newText[line.pos]

      of eKeep:
        var (oldp, newp) = (line.pos, line.newPos)
        while oldp < newp:
          oldDiff.add "?"
          inc oldp

        while newp < oldp:
          newDiff.add "?"
          inc newp


        oldDiff.add "~ " & oldText[line.pos]
        newDiff.add "~ " & oldText[line.pos]

  echo sideBySide(oldDiff.join("\n"), newDiff.join("\n"))

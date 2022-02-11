import std/[tables, intsets, sequtils, algorithm, strformat]
import hmisc/core/all

startHax()

proc pop(i: var IntSet): int =
  for item in items(i):
    result = item
    break

  i.excl result

type IdxCostMap* = Table[(int, int), int] 

proc stablematch[T](
    lhs, rhs: seq[T], weight: proc(a, b: int): int,
    order: SortOrder = SortOrder.Ascending
  ): tuple[lhsIgnore, rhsIgnore: seq[int], map: IdxCostMap] =
  ## Do a weighted matching of the items in lhs and rhs sequences using
  ## weight function. Return most cost-effective matching elements.

  var lfree: seq[int]
  var canTry: Table[int, seq[int]]
  var rmap: Table[int, (int,int)]

  for idx in 0 ..< len(lhs):
    lfree.add idx

  for l in lfree:
    canTry[l] = @[]
    for r in 0 ..< len(rhs):
      canTry[l].add r

  proc getCost(l, r: int, res: var IdxCostMap): int =
    if (l, r) notin res:
      res[(l, r)] = weight(l, r)

    res[(l, r)]

  var tmp: IdxCostMap
  while 0 < len(lfree):
    let l = lfree.pop()
    if 0 < canTry[l].len:
      let r = canTry[l].pop()
      if r in rmap:
        let (oldL, _) = rmap[r]
        let tryCost = getCost(l, r, tmp)
        let otherCost = getCost(oldL, r, tmp)
        let better =
          if order == Ascending:
            otherCost < tryCost
          else:
            otherCost > tryCost

        if better:
          lfree.add oldL
          rmap[r] = (l, r)

        else:
          lfree.add l

      else:
        discard getCost(l, r, tmp)
        rmap[r] = (l, r)

  var lset: IntSet
  for idx in 0 ..< len(rhs):
    if idx in rmap:
      lset.incl rmap[idx][0]
      result.map[rmap[idx]] = tmp[rmap[idx]]

    else:
      result.rhsIgnore.add idx

  for idx in 0 ..< len(lhs):
    if idx notin lset:
      result.lhsIgnore.add idx

let
  lhs = @['9', '1', '8']
  rhs = @['1', '3', '4', '9']


for dir in [Ascending, Descending]:
  let (lignore, rignore, map) = stablematch(
    lhs, rhs,
    proc(a, b: int): int = return lhs[a].ord + rhs[b].ord,
    dir
  )

  let s = toSeq(pairs(map)).sortedByIt(-it[1])
  echo s
  for (k, v) in s:
    echo &"{lhs[k[0]]}[{k[0]}] ? {rhs[k[1]]}[{k[1]}] -> {v}"

  echo lignore
  echo rignore

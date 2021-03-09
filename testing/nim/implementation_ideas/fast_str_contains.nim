import benchy
import std/[random, critbits, sequtils, times, sugar, md5, macros, algorithm]

const chars = toSeq('0' .. 'Z')
var rand = initRand(228)

proc randString(): string =
  var buf: string
  for i in 0 .. 100:
    buf.add sample(rand, chars)

  buf


const seq0: seq[string] = collect(newSeq):
  for idx in 0 .. 10_000:
    $($idx).repeat(50)

func matchForStrs(mainStr: NimNode, strs: seq[(string, int)], depth: int): NimNode =
  var strs = sortedByIt(strs, it[0])

  var ch = strs[0][0][0]

  result = nnkCaseStmt.newTree(
    nnkBracketExpr.newTree(mainStr, newLit(depth)))

  var buf: seq[(string, int)]
  for str in strs:
    let start = str[0][0]
    if start == ch:
      buf.add (str[0][1 ..^ 1], str[1])

    else:
      result.add nnkElifBranch.newTree(
        matchForStrs(mainStr, buf, depth + 1))

  result.add nnkElse.newTree(newLit(-1))


macro fastFind(strs: static[seq[string]], str: string): untyped =
  var newStrs: seq[(string, int)]
  for idx, str in pairs(strs):
    newStrs.add (str, idx)

  result = matchForStrs(str, newStrs, 0)
  echo result.repr



timeIt "Item is in seq":
  var res = find(seq0, randString())
  keep(res)

timeIt "Fast find":
  var res = fastFind(seq0, randString())
  keep(res)

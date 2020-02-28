import sequtils
import bitops
import math
import strutils
import sets
import tables

import algorithm

var t: Table[HashSet[string], string]

t[toHashSet(["ctrl"])] = "ctrl-c"
t[toHashSet(["shift"])] = "ctrl-c"
t[toHashSet(["meta"])] = "meta-c"
t[toHashSet(["alt", "ctrl"])] = "alt-c"

echo t

echo t.hasKey(toHashSet(["ctrl", "alt"]))

# template sortedByIt(s: typed, value: untyped): typed =
#   s.mapIt(
#     (it,
#        block:
#          echo it
#          let it {.inject.} = it
#          value)).sorted(cmp = proc(lhs, rhs: type(value)): )

# echo sortedByIt(@["sdf", "22222", "121222"], it.len)

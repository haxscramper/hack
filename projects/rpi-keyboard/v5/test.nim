import sequtils
import bitops
import math
import strutils
import sets
import tables

var t: Table[HashSet[string], string]

t[toHashSet(["ctrl"])] = "ctrl-c"
t[toHashSet(["shift"])] = "ctrl-c"
t[toHashSet(["meta"])] = "meta-c"
t[toHashSet(["alt", "ctrl"])] = "alt-c"

echo t

echo t.hasKey(toHashSet(["ctrl", "alt"]))

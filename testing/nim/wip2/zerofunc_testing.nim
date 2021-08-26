import zero_functional
import std/macros

let data = @[1,2,3,4] --> map((echo it; it * 2))

macro `|>`(a, b: untyped): untyped =
  echo a.treeRepr()
  echo b.treeRepr()


1 |> 2 |> it * 3

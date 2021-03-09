import macros

macro t(a: typed): untyped =
  echo a.treeRepr()


t:
  let a = 12
  let b = 12
  proc c(q,z: int): int = z + q * 2

  echo c(a,b)

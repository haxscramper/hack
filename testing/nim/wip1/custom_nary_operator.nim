import macros
proc arrImpl(a, b, c: int): void = echo a, " ", b, " ", c

macro `~`(lhs: untyped{nkBracketExpr}, c: int): untyped =
  let a = lhs[0]
  let b = lhs[1]
  quote do:
    arrImpl(`b`, `b`, `c`)

2[2] ~ 2
3 ~ 2

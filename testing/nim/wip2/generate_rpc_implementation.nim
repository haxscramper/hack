# https://stackoverflow.com/posts/67150407/edit

import std/[macros]

proc rcall*[A, B, R](fn: string, a: A, b: B, _: type[R]): R =
  echo (fn, a, b)

macro remotefn(fn: typed) =
  let fname = fn.str_val()
  # `quote do` generates hygienic identifiers - i.e. all new
  # variables/functions introduced by it are unique and not visible in
  # global. To fix this you need to explicitly create identifier for your
  # procedure using `ident`

  let
    multId = ident("multiply") # < Function name identifier

    # Same goes for variables - nim does have a name-based overloading, so
    # you need to make sure function arguments use the same identifiers as
    # the original one
    aId = ident("a")
    bId = ident("b")

  result = quote do:
    proc `multId`(`aId`, `bId`: int): int =
      rcall(`fname`, 1, 1, int)

  echo result.toStrLit()

proc multiply(a, b: int): int
remotefn multiply

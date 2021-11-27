const
  h = "lib.hpp"
  l = "liblib.so"

type
  WithConstructor {.importcpp: "WithConstructor", header: h.} = object

proc newWithConstructor(arg: cint): WithConstructor {.
  importcpp: "WithConstructor(@)", constructor, cdecl.}

proc newWithConstructor(arg: cint, arg2: cint): WithConstructor {.
  importcpp: "WithConstructor(@)", constructor, cdecl, dynlib: l.}

proc main() =
  let val1 = newWithConstructor(20)
  let val2 = newWithConstructor(20, 30)

main()

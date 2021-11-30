const
  h = "lib.h"
  l = "liblib.so"

type
  str {.importc: "struct str", header: h.} = object
    field: cint

  Const[T] {.importcpp: "const 0".} = object

# echo str()

proc cc(arg: Const[int]) = discard

cc(Const[int]())


proc get_cstring_dl(str: cstring): cint {.
  dynlib: l, importc: "get_cstring_dl".}

echo get_cstring_dl(nil)

proc get_cstring_link(str: cstring): cint {.
  nodecl, importc: "get_cstring_link".}

echo get_cstring_link(nil)

const cpplib = "cpplibrary.hpp"

proc getInt(arg: cint): cint {.
  header: cpplib,
  importcpp: "getInt(@)".}

type
  ParentObj {.
    header: cpplib,
    importcpp: "Parent"
  .} = object

  DefaultObj {.
    header: cpplib,
    importcpp: "Default"
  .} = object

proc nimLogic*(str: cstring): cstring {.exportc.} =
  let s = $getInt(90) & " -- " & $str
  return allocCstringArray([s])[0]

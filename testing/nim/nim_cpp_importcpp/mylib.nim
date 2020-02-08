proc getInt(arg: cint): cint {.
  header:"cpplibrary.hpp",
  importcpp: "getInt(@)".}

proc nimLogic*(str: cstring): cstring {.exportc.} =
  let s = $getInt(90) & " -- " & $str
  return allocCstringArray([s])[0]

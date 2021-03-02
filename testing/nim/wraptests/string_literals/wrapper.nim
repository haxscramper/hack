type
  S {.importcpp: "S", header: "file.cpp".} = object

proc toS(str: cstring, l: culong): S {.
  importcpp: "operator\"\"_S(@)".}

let str = "hellow world"
let val: S = toS(str.cstring, str.len.culong)

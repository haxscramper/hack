import std/strutils

type
  Obj = object
  PObj = ref Obj

  RObj = ref object


template show(expr: untyped): untyped =
  echo alignLeft(astToStr(expr), 30), expr

show Obj() is ref
show Obj() is ref object
show PObj() is ref
show PObj() is ref object
show RObj() is ref
show RObj() is ref object


show int is distinct

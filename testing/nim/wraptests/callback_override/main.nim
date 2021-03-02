import callbacks






var second = block:
  var derived = newCppBaseDerived[int]()

  derived.setBaseMethod proc(this: var CppBaseDerived[int], arg: cint) {.closure.} =
      echo "Override callback with nim implementation ", arg

  derived.baseMethod(12)

  derived

second.baseMethod(12333)

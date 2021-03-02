import callbacks

type
  FullyNimDerived = object of CppBaseDerived
    fld1: int

proc baseMethod(derived: FullyNimDerived, arg: int):  void
  {.importcpp: "#.baseMethod(@)", header: derivedHeader.}

proc toFullyNimDerived(base: ptr CppBaseDerived):
  ptr FullyNimDerived {.noinit.} =
  {.emit:"return (`FullyNimDerived`*)(`base`);".}

var nimder = FullyNimDerived()

nimder.baseMethod(222)

nimder.baseMethodImpl =
  proc(ni: ptr CppBaseDerived, arg: cint) {.cdecl.} =
    echo toFullyNimDerived(ni).fld1
    echo "Fully derived in nim, ", arg

nimder.baseMethod(333)
nimder.baseMethod(444)

echo "Finished"

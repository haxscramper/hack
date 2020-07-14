import common_types

proc dynlibProc*(): void {.exportc, dynlib, cdecl.} =
  miscProc()
  raise DynlibEx(f1: 289)


#=============================  Attempt V2  ==============================#

proc dynlibEntryPoint*(
  errorCb: proc(ex: DynlibEx)): void {.exportc, dynlib, cdecl.} =
  try:
    dynlibProc()
  except DynlibEx:
    echo "Caught error inside DLL"
    errorCb(cast[DynlibEx](getCurrentException()))

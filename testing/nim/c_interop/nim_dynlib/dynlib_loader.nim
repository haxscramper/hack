import common_types

import dynlib


type
  DynlibProc = proc() {.nimcall.}
  DynlibEntry = proc(errorCb: proc(ex: DynlibEx)) {.nimcall.}

let dll = loadLib("./libdynlib_implementation.so")

#=============================  Attempt v1  ==============================#

if false:
  # It seems like catching exception from DLL is not really supported.
  if dll != nil:
    let symAddr = dll.symAddr("dynlibProc")
    if symAddr != nil:
      let procImpl: DynlibProc = cast[DynlibProc](symAddr)
      try:
        echo "calling proc implementation"
        procImpl()
      except DynlibEx:
        let ex = cast[DynlibEx](getCurrentException())
        echo "Exception from dynlib"
        echo ex.f1
      except:
        echo "Catch-all clause"
    else:
      echo "failed to find symbol"
  else:
    echo "failed to load dynlib"

#=============================  Attempt V2  ==============================#

proc errorCallback(ex: DynlibEx): void =
  echo "processing error callback from dynlib"
  echo ex[]

if true:
  # It seems like catching exception from DLL is not really supported.
  if dll != nil:
    let symAddr = dll.symAddr("dynlibEntryPoint")
    if symAddr != nil:
      let procImpl: DynlibEntry = cast[DynlibEntry](symAddr)
      echo "calling proc implementation"
      procImpl(errorCallback)
    else:
      echo "failed to find symbol"
  else:
    echo "failed to load dynlib"

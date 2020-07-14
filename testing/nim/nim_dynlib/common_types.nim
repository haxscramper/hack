type
  DynlibEx* = ref object of CatchableError
    f1*: int

proc miscProc*(): void =
  echo "called misc proc from common types"

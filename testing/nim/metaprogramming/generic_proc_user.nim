# generic_proc_user.nim
import generic_proc_overload

type U* = object

proc `$`(u: U): string = "U"

proc implementationForT(a: U): void =
  echo "Using implementation for U"

usingDollar(12)
usingDollar(U()) # Compiles as expected

genericProc(12)
# genericProc(U()) # Fails to compile
# genericProc("string") # Fails to compile


# generic_proc_overload.nim
proc implementationForT(a: int): void =
  echo "Using implementation for integer"

proc genericProc*[T](a: T): void =
  implementationForT(a)

proc usingDollar*[T](a: T): void =
  echo $a

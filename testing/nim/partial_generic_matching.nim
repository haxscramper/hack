type
  U[T1, T2] = object
    f1: T1
    f2: T2

proc impl[T1, T2](arg: U[T1, T2]): void =
  echo "Using fully generic proc"

proc impl[T2](arg: U[int, T2]): void =
  echo "Using partially specialized proc"

proc impl(arg: U[int, float]): void =
  echo "Using fully specialized proc"

var u1: U[float, string]; impl(u1)
var u2: U[int, string]; impl(u2)
var u3: U[int, float]; impl(u3)

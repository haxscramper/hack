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

#======================  multiple implementations  =======================#


echo "\t\t----\t"

block:
  type
    Multi[T] = object
      when T is int:
        f1: float
      else:
        f2: string

  echo Multi[int]()
  echo Multi[string]()

  proc impl(arg: Multi[int]): void =
    echo "Specific implementation for integer"

  proc impl[T](arg: Multi[T]): void =
    echo "Generic implementation"

  var m1: Multi[int]; impl(m1)
  var m2: Multi[float]; impl(m2)

#======================  Generict 'T' and 'seq[T]'  ======================#

echo "\t\t----\t"

block:
  proc impl[T](arg: T): void =
    echo "Using most generic implementation"

  proc impl[T](arg: seq[T]): void =
    echo "Using less generic implementation"

  impl(@[12, 2, 3])
  impl("Hello")


#=====================  mutally recursive generics  ======================#

echo "\t\t----\t"

block:
  proc impl[T](arg: T): void =
    echo "Most generic implementation, ", typeof(arg)

  proc impl[T](arg: seq[T]): void =
    echo "Less generic implementation, ", typeof(arg)
    for val in arg:
      impl(val)

  impl(@[@[@[90]]])

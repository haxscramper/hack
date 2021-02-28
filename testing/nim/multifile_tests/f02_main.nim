# Test of generic overload resolution across module boundaries

import f02_generic_overload_mixin

type
  U[T] = object
    f2: seq[T]

proc impl*[T](arg: U[T]): void =
  echo "User-defined overload for type U"
  impl(arg.f2)

proc impl*(arg: string): void =
  echo "Concrete implementation for 'string'"

impl(U[U[string]](f2: @[U[string](f2: @["test"])]))

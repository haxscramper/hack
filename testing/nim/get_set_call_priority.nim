type
  Test = object
    field2: int

proc `field=`*(t: var Test, val: int): void =
  echo "set"

proc field*(t: Test): void = echo "get"
proc field*(t: var Test): void = echo "get mutable"

var ee: Test

ee.field
ee.field = 123

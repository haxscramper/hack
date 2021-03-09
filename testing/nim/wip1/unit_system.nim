## **Attempt (failed)**: To implement type-based dimenstion checking
## for nim. Original idea was to describe type as a
## `static[seq[(string, int)]]` where `string` represents name of the
## unit and `int` represents power. Proof of concept failed due to
## limitations of nim compiler (I either get random crashes or cryptyc
## compilation errors)

## **Results**: you can use procs to do compile-time computations on a
## `static` parameter values. You can also use `macro` to get more
## sophsiticated conversion between types.

#========================  type conversion test  =========================#

import typetraits, macros

block:
  macro toFloatTuple(t: typedesc): typedesc =
    echo "conversion"
    result = quote do: (float, `t`)

  proc testProc[T1](val: T1): toFloatTuple(T1) =
    discard

  echo typeof(testProc('1'))


#==================================  1  ==================================#

type
  Unit[T: static[seq[(string, int)]]] = object
    val: float


proc meter(arg: float): Unit[@[("m", 1)]] =
  discard

proc multiply[N1, N2: static[int], Tstr: static[string]](
  m1: Unit[@[(Tstr, N1)]],
  m2: Unit[@[(Tstr, N2)]]
               ): Unit[@[(Tstr, N1 + N2)]] =

  Unit[@[(Tstr, N1 + N2)]](
    val: m1.val * m2.val
  )

# echo typeof multiply(meter(1), meter(2))

#==================================  2  ==================================#



# proc test1[N1: static[int], N2: static[int]](
#   a1: N1,
#   a2: N2): N1 + N2 =
#     discard

type U[D: static[int]] = object
  val: float

proc addInts(a: int, b: int): int =
  echo "Adding ints"
  return a + b

proc multiply[D1, D2: static[int]](a: U[D1], b: U[D2]): U[addInts(D1, D2)] =
  result.val = a.val * b.val

var u1: U[1]
var u2: U[2]

echo typeof(multiply(u1, u2))

#==================================  3  ==================================#

# proc multiply


when false:
  # FIXME Error: attempt to access a nil address
  proc test2[T: static (string, int)](arg: T): T[1] = discard
  echo test2(("hello", 12))

when false:
  # FIXME Error: attempt to access a nil address
  proc test2(arg: static (string, int)): static[int] = T[1]
  echo test2(("hello", 12))

# static:
#   echo typeof(static ("hello", 12))



type
  U1[T: static[(string, int)]] = object
    val: float

proc square[T: static[string], D: static[int]](
  u1: U1[(T, D)]
    ): U1[(T, D * 2)] =

  U1[(T, D * 2)](val: u1.val * u1.val)

# echo typeof(square(U1[("m", 1)](val: 12)))

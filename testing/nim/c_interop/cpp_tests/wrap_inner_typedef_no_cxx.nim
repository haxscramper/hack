const h = "wrap_inner.hpp"

type
  P[T] {.importcpp: "P<'0>", header: h.} = object
    f: T

  S[T] {.importcpp: "S<'0>", header: h.} = object

  # Have to declare additional object, cannot port procedures/fields of
  # `P`. When compiled to C++ this type /is/ actually a `P[T]`
  S_value_type[T] {.importcpp: "S<'0>::value_type", header: h.} = object
    f: T # In order for `echo` to work correctly I need to port all fields.

proc get[T](s: S[T], arg: T): S_value_type[T] {.importcpp: "#.get(@)", header: h.}

proc initS[T](): S[T] {.importcpp: "S<'*0>()", header: h.}

proc main() =
  let s = initS[cint]().get(1)
  echo s

main()

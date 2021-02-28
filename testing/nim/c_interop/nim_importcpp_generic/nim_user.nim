const cxheader = "generic_type.hpp"

type
  Generic[T] {.
    importcpp: r"Generic<'0>", header: cxheader.} = object

  Explicit = Generic[cint]

proc impl[T](gen: Generic[T]) {.
  importcpp: "#.impl(@)", header: cxheader.}

var expl: Explicit
expl.impl()


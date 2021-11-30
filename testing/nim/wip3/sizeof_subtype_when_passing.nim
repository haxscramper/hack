type
  Base {.inheritable.} = object

  Derived {.bycopy.} = object of Base
    field: int

# Subrange or subtype match: a is a range[T] and T matches f exactly. Or: a is a subtype of f.
proc based(b: Base) =
  echo sizeof(b)

based Derived()
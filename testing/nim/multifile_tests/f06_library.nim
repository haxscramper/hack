import f06_implementation

proc exported*[T](v: T): void =
  echo gen(v)

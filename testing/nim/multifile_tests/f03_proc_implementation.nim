type
  U*[T] = object
    f*: T

proc exportedProc*[T](a: U[T]): void =
  echo "Using exported proc"

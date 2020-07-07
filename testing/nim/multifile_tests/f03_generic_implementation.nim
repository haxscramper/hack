import f03_proc_implementation

proc genericProc*[T](a: T): void =
  # Using proc exported in another module
  exportedProc(U[T](f: a))

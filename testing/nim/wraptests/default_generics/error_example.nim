type
  Functor[T] = object
  Gen[T, D] = object

proc initFunctor[T](): Functor[T] = discard
proc initGen[T, D](arg: D): Gen[T, D] = discard
proc initGen[T](): Gen[T, Functor[T]] = discard

let val1 = initGen[string]()

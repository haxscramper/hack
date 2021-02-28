import sets
proc exported*[T](arg: T): void =
  mixin difference
  mixin items
  var a, b: HashSet[int]
  discard difference(a, b)

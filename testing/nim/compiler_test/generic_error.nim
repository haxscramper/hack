type U[T] = object
proc gen[T](arg: U) = discard
gen[int]()
# 1

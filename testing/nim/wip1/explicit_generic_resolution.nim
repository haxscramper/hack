# UFCS affects generic resolution

proc test[T](a: int) = discard

12.test[float]() # Cannot instantiate

test[float](12) # works as expected

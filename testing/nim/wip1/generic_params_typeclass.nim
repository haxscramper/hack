type
  A = float | int

proc gen[T](a: A, b: T): A = discard
proc cbGen[A, T](a: T, cb: proc(aa: A)) = discard

cbGen("er", proc(a: int) = discard)
cbGen("er", proc(a: string) = discard)
echo gen(1.2, "eee")

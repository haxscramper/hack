proc test(a: int) = echo "called a"
proc test(b: int) = echo "called b"

proc test(a: float): int = echo "called a"
proc test(b: float): string = echo "called b"

proc test[T](a: T): int = echo "called a"
proc test[T](b: var T): string = echo "called b"

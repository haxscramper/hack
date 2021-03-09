proc `&&`(s: varargs[int]): int = echo s
template optConc{ `&&` * a }(a: int): untyped = &&a
discard 2 && 3 && 4

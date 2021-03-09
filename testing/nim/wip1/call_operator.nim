{.experimental: "callOperator".}

proc `()`(a: int) = echo "cakk"
proc `()`(s: string, args: varargs[string, `$`]) =
  echo s, ":: ", args

1()

"Hello" 12, 3, 4

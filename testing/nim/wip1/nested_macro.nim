import macros

macro a(): untyped = echo "123"
macro b(): untyped = quote: a()
macro c(): untyped = quote: b()

macro top(body: untyped): untyped =
  echo body.toStrLit()

top(c())

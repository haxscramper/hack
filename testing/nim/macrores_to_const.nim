import macros

func hello[T](a: T): T = a

macro test(): untyped =
  quote do:
    hello(12)

const a = test()

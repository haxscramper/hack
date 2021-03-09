import macros

type
  Test = object
    f1: int

proc `=destroy`(x: var Test) =
  echo "destuctor " & $x.f1

block:
  let t = Test(f1: 12)

let t =
  block:
    var t: ref Test
    new(t)
    t.f1 = 33

    t

macro letMacro(args: varargs[untyped]): untyped =
  for arg in args:
    echo arg.treeRepr()

  quote do:
    echo "hello"

macro param(docstr: string, body: untyped): untyped =
  quote do:
    echo "hello"

let a {.letMacro.}: int = 12

# dumpTree:
#   \hello{world}{12\sqrt{@toLatex{`var`}}}{888}{999}

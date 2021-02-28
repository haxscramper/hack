{.experimental.}

import macros

type
  Hell[T, Name] = object
    f1: T
    f2: Name

  Obj = object
    f1: Hell[int, "12"]

proc `=destroy`(o: var Obj): void =
  echo "Destroying Obj"

proc `=sink`(dest: var Obj; source: Obj) =
  echo "Running sink for Obj"
  `=destroy`(dest)
  dest = source
  # dest.field = source.field

macro impl(t: typed): untyped =
  echo t.getTypeImpl().toStrLit()

import typetraits

let inst = Obj(f1: Hell[int, "world"](f1: 12))

# echo genericParams(type inst.f1)

impl(Obj())

var x: Obj = Obj()

block:
  var x: Hell[int, "he"]; echo "typeof: ", typeof(x)
block:
  var x: Hell[12, "he"]; echo "typeof: ", typeof(x)

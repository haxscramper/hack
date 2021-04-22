import std/options

template `?`(t: typedesc): untyped = Option[t]
template `or`[T](opt: Option[T], default: T): untyped =
  let val = opt
  if val.isSome(): val.get() else: default

var tmp: ?int

let expr = tmp or 12
echo expr

var tmp2: ?string = some("123")

let expr2 = tmp2 or (echo "no eager evaluation"; "123123")

type
  Obj = object
    field: ?string

echo typeof tmp
echo typeof Obj().field

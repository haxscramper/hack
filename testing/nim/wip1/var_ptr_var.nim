import strformat, macros, sugar
import std/decls

type
  Obj = object
    f1: float
    f3: char

  CbPayload = object
    p: ptr[Obj]

proc `$`[T](p: ptr T): string = $cast[int](p)
proc `$`(p: pointer): string = $cast[int](p)

proc callback(payload: CbPayload): void =
  var obj {.byaddr.}: Obj = payload.p[]
  # payload.p[].f3 = '5'
  echo &"{obj} from {payload.p}"
  obj.f3 = '2'
  echo &"{obj} set @ {addr obj}"

var obj: Obj
echo &"{obj} @ {addr obj}"
callback(CbPayload(p: addr obj))
echo &"{obj} @ {addr obj}"

#=================================  eee  =================================#

proc getCb(data: pointer, cb: proc(clientdata: pointer) {.cdecl.}) {.cdecl.} =
  echo "executing ... "
  cb(data)

var variable: string = "---"
var buf: seq[string]

var actualData = (
  strv: addr variable,
  buf: addr buf
)

type Data = typeof(actualData)

getCb(
  addr actualdata,
  proc(clientData: pointer) {.cdecl.} =
    let data = cast[ptr[Data]](clientData)
    var strv {.byaddr.}: string = data[].strv[]
    var buf {.byaddr.}: seq[string] = data[].buf[]
    strv = "hhhh"
    buf.add "hello"
)

echo "strv: ", variable
echo "bufv: ", buf

var invar: int
var eee1 {.byaddr.} = invar

macro makeVisitorTest(): untyped =
  let byaddrid = ident "byaddr"
  quote do:
    var eee2 {.`byaddrid`.} = invar
#              ^^^^^^^^^^
#              |
#              Need to use interpolation to avoid gensym on indents

makeVisitorTest


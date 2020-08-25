import strformat
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
var intvar: int = 12

var actualData = (intv: addr intvar, strv: addr variable)
type Data = typeof(actualData)

getCb(
  addr actualdata,
  proc(clientData: pointer) {.cdecl.} =
    let data = cast[ptr[Data]](clientData)
    var strv {.byaddr.}: string = data[].strv[]
    var intv {.byaddr.}: int = data[].intv[]
    strv = "hhhh"
    intv = 124314
)

echo "strv: ", variable
echo "intv: ", intvar




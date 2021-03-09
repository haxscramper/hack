type
  RefObj = ref Obj
  Obj = object
    f1 {.requiresinit.}: float

proc `=destroy`(obj: var Obj) = echo "Destroy Obj"

let hellos = Obj(f1: 2.3)
let hellor = RefObj(f1: 1.2)
var hello: RefObj

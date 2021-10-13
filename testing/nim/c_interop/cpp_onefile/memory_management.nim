type
  NimObject {.exportc: "NimObject".} = object
    field: int

proc `=destroy`(obj: var NimObject) {.exportc: "destroy_NimObject".} =
  echo "Called nim destructor for ", obj.field


type
  CxxVector[T] {.bycopy, importcpp: "std::vector", header: "<vector>".} = object

proc add[T](v: var CxxVector, item: T) {.importcpp: "#.push_back(@)".}

proc `=destroy`[T](obj: var CxxVector[T]) =
  echo "Called destroy for cxx vector"

proc main1() =
  block:
    var v: CxxVector[NimObject]
    v.add NimObject(field: 3)

  block:
    var s: seq[NimObject]
    s.add NimObject(field: 1)

# main1()

type
  CxxContainer[T] {.importcpp, header: "memory_management.hpp".} = object
    value: T

proc `=destroy`[T](obj: var CxxContainer[T]) =
  echo "Called destroy for cxx container"
  `=destroy`(obj.value)

# proc `=copy`(dest: var T; source: T)
proc `=copy`[T](dest: var CxxContainer[T], source: CxxContainer[T]) =
  echo "Copying cxx container"

# proc `=sink`(dest: var T; source: T)
proc `=sink`[T](dest: var CxxContainer[T], source: CxxContainer[T]) =
  echo "Moving cxx container"

proc main() =
  var c = CxxContainer[NimObject](value: NimObject(field: 2))
  var c1 = c

  echo c

main()

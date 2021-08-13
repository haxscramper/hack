type
  Struct {.importcpp: "Struct", header: "struct_with_constructor.hpp", bycopy.} = object
    field: cint

proc newImportAux() {.header: "<new>", importcpp: "((void)0)".}
proc destroyStruct(s: var Struct) {.importcpp: "#.~Struct()".}

proc newStruct(): ref Struct =
  newImportAux()
  new(result, proc (self: ref Struct) = destroyStruct(self[]))
  {.emit: "new ((void*)result) Struct();"}

proc main() =
  let str = newStruct()
  echo str[]

main()

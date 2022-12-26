import hmatching
import std/macros

macro typeDef(t: untyped): untyped =
  let t = t[0][0]
  echo t.treeRepr()
  assertMatch(
    t,
    TypeDef[@name, (GenericParams[@params] | Empty()), ObjectTy()]
  )

  echo typeof(name)
  echo typeof(params)
  echo params.isSome()

typeDef:
  type
    Thing[T] = object
      field: int

import typetraits, macros, strformat

#[

Experiments with variadic generics.

Second version of simple sum type implementation. Since Nim already
has good sum types (case) objects I didn't try to implement 'general'
solution that would work in all cases.

It is meant to be used in cases where separate type for object is too
much - simple sequence of heterogenous items for example.

]#

type
  Variant[Types] = object
    values: Types
    idx: int

proc getN*[Types](invar: Variant[Types], idx: static[int]): auto =
  static:
    assert idx < arity(Types),
     "Cannot get value at index " & $idx &
      " - variant has only " & $arity(Types) & " types"

  if invar.idx == idx:
    invar.values[idx]
  else:
    raiseAssert(
      &"Cannot get value at index {idx} - current index is " &
        &"{invar.idx}")

template get*[Types](invar: Variant[Types], val: typed): untyped =
  var idx: int = 0
  var res: val
  for _, fld in invar.values.fieldPairs():
    when fld is val:
      if invar.idx == idx:
        res = fld
      else:
        raiseAssert("Cannot get value for type `" & $typeof(val) &
          "` - current variant index is " & $invar.idx & ", but " &
          "value with type `" & $typeof(val) & "` has index " & $idx)
    inc idx

  res

template set*[Types](invar: var Variant[Types], other: typed) =
  var idx: int = 0

  for _, fld in invar.values.fieldPairs():
    when fld is typeof(other):
      invar.idx = idx
      fld = other

    inc idx


template variant*[Types](val: typed): Variant[Types] =
  block:
    var res: Variant[Types]
    res.set val
    res

template hasType*[Types](invar: Variant[Types], t: typed): bool =
  var res: bool = false
  var idx: int = 0
  for _, fld in invar.values.fieldPairs():
    if (fld is t) and (invar.idx == idx):
      res = true

    inc idx

  res

proc hasIdx*[Types](invar: Variant[Types], idx: int): bool =
  invar.idx == idx

block:
  var test = variant[(int, float)](12)
  echo test.getN(0)
  echo test.get(int)
  assert test.hasType(int)
  assert test.hasIdx(0)
  test.set 1.2
  assert test.hasIdx(1)
  assert test.hasType(float)

  try:
    echo test.getN(0)
  except AssertionError:
    echo "Cannot getN(0)"
    echo getCurrentExceptionMsg()
  finally:
    echo test.getN(1)

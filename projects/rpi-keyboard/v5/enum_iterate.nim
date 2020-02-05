import macros

type
  EnumWithHoles = enum
    val1 = 12
    val2 = 14

macro disjointIterImpl(x: typed): untyped =
  var values: seq[NimNode]
  for value in x.getTypeImpl[1..^1]:
    values.add newIdentNode($value.tostrlit)

  result = nnkStmtList.newTree(
    nnkPrefix.newTree(
      newIdentNode("@"),
      nnkBracket.newTree(values)))

macro disjointIter*(typ: typedesc[enum]): untyped =
  quote do:
    block:
      var a: `typ`
      disjointIterImpl(a)


for val in disjointIter(EnumWithHoles):
  echo val

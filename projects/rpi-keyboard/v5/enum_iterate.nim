import macros

# iterator disjointIter*(E: typedesc[enum]): E =
#   static

type
  EnumWithHoles = enum
    val1 = 12
    val2 = 14

#macro echoImpl(head: untyped) = echo getImpl(head)

macro disjointIter(x: typed): untyped =
  var values: seq[NimNode]
  for value in x.getTypeImpl[1..^1]:
    values.add newIdentNode($value.tostrlit)

  result = nnkStmtList.newTree(
    nnkPrefix.newTree(
      newIdentNode("@"),
      nnkBracket.newTree(
        values
        )))

var a: EnumWithHoles

echo disjointIter(a)

# for val in disjoinIter(EnumWithHoles):
#   echo val

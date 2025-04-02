case 1:
  of 3: 
    echo "asdf"

  of 4:
    echo "dfad"

  else:
    echo ""

echo type(3)


case 1:
  of int8(3): 
    echo "asdf"

  of int8(4):
    echo "dfad"

  else:
    echo ""

type
  Test = enum One, Two, Three

case Test.One:
  of Test.One:
    echo "one"

  of Test.Two:
    echo "two"

  of Test.Three:
    echo "Three"

# This is an example how an abstract syntax tree could be modelled in Nim
type
  NodeKind = enum  # the different node types
    nkInt,          # a leaf with an integer value
    nkFloat,        # a leaf with a float value
    nkString,       # a leaf with a string value
    nkAdd,          # an addition
    nkSub,          # a subtraction
    nkIf            # an if statement
  Node = ref object
    case kind: NodeKind  # the `kind` field is the discriminator
    of nkInt: intVal: int
    of nkFloat: floatVal: float
    of nkString: strVal: string
    of nkAdd, nkSub:
      leftOp, rightOp: Node
    of nkIf:
      condition, thenPart, elsePart: Node

var n = Node(kind: nkFloat, floatVal: 1.0)
# the following statement raises an `FieldDefect` exception, because
# n.kind's value does not fit:
n.strVal = ""

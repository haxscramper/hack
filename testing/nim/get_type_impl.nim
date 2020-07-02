import macros

macro dumpType(a: typed) =
  echo "=========== ", a.toStrLit()
  let kind = a.getTypeImpl().kind
  case kind:
    of nnkBracketExpr:
      let typeSym = a.getTypeImpl()[1]
      echo "\t\tType implementation code"
      echo typeSym.getTypeImpl().toStrLit()
      echo "Type kind: ", typeSym.kind()
      echo "Impl kind: ", typeSym.getTypeImpl().kind()
      echo "Typeof: ", typeof(a)
      echo "\t\tType implementation tree"
      echo typeSym.getTypeImpl().treeRepr()

    of nnkObjectTy, nnkRefTy:
      echo "\t\tType implementation code"
      echo a.getTypeImpl().toStrLit()
      echo "Type kind: ", a.kind()
      echo "Impl kind: ", a.getTypeImpl().kind()
      echo "Typeof: ", typeof(a)
      echo "\t\tType implementation tree"
      echo a.getTypeImpl().treeRepr()
    else:
      raiseAssert("Unknown parameter kind: " & $kind)

template test(typeparam: typed, msg: string) =
  static:
    echo "\t\t ---- \e[32m", msg, "\e[0m"
  var a {.inject.}: typeparam
  dumpType(a)

block:
  type
    U = object

  test U, "Object without fields"

block:
  type
    U = ref object

  test U, "Ref object without fields"

block:
  type
    U = object
      f1: int

  test U, "Object with one field"

block:
  type
    U = object
      case a: bool
        of true:
          f1: int
        of false:
          f2: float

  test U, "Single-case object"



block:
  type
    U[T] = object
      when T is string:
        f1: int
      else:
        f1: seq[int]


  block: test U[int], "'When' object"
  block: test U[string], "'When' object"

type
  Type = object
    case a: bool
      of true:
        f11: int
        f12: int
      of false:
        case b: bool
          of true:
            f21: string
          of false:
            f31: float

test Type, "Multi-case object"

proc getKindFields(node: NimNode): seq[tuple[field, kindType: string]] =
  case node.kind:
    of nnkObjectTy:
      return node[2].getKindFields()
    of nnkRecList:
      for elem in node:
        result &= elem.getKindFields()
    of nnkRecCase:
      let idefs = node[0]
      var top = (field: $idefs[0], kindType: $idefs[1])
      for branch in node[1..^1]:
        for fld in branch[1]:
          if fld.kind == nnkRecCase:
            result &= fld.getKindFields()

      result.add top
    else:
      discard

macro tmp(a: typed): typed =
  let flds = a.getTypeImpl().getKindFields()
  echo flds


tmp(a)

block:
  type
    U = object
      f1: int

  U.dumpType()

  proc generic[T](a: T): void =
    # Get type implementation for generic parameter
    T.dumpType()


  generic(12)
  generic(U())

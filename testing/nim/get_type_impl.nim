import macros, typetraits

macro dumpType(a: typed) =
  echo "=========== ", a.toStrLit()
  echo "=========== ", a.getType().toStrLit()
  let kind = a.getTypeImpl().kind
  echo "=========== kind: ", kind
  # echo "== Type impl:", a.getTypeImpl().lispRepr()
  case kind:
    of nnkBracketExpr:
      let typeSym = a.getTypeImpl()[1]
      # if typeSym.kind == nnkSym:
      #   echo "Sym type"
      #   let unref = typeSym.getTypeImpl().getTypeImpl()
      #   echo "Unref ", typeSym.getTypeImpl().lispRepr()

      if typeSym.kind == nnkSym:
        echo "3", typeSym.getTypeImpl().lispRepr()

      echo "\t\tType implementation code"
      echo typeSym.lispRepr()
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

block:
  type
    G[T] = object
      f1: T

  G.dumpType()
  (G[int]).dumpType
  var p: G[int]
  p.dumpType()

  echo genericParams(G[int])

block:
  (float, char).dumpType()
  ((float, char)).dumpType()
  (tuple[a: float, b: char]).dumpType()

for name, val in fieldPairs((12, 3)):
  echo name

static: echo "\e[41m*======\e[49m  Ref object  \e[41m======*\e[49m"


block:
  macro te(a: typed): untyped =
    let ts = a.getTypeImpl()
    echo a.kind
    echo ts.kind
    if ts.kind == nnkBracketExpr:
      echo a.getTypeImpl()[1].getImpl().treeRepr()
    else:
      echo a.getTypeImpl()[0].getImpl().treeRepr()

  type
    A = ref object
      f: int
    B = object
      f2: int

    G[T] = ref object
      f: T

  static: echo "\e[41m*==========\e[49m  A  \e[41m===========*\e[49m"
  A.te()
  static: echo "\e[41m*==========\e[49m  B  \e[41m===========*\e[49m"
  B.te()
  static: echo "\e[41m*==========\e[49m  G  \e[41m===========*\e[49m"
  var f: G[int]
  f.te()

static: echo "\e[41m*\e[49m  Generic case object with void field  \e[41m*\e[49m"

block:
  macro te(a: typed): untyped =
    let ts = a.getTypeImpl()
    echo a.kind
    echo ts.kind
    echo ts.treeRepr()

  type
    G[T, F] = object
      case ok: bool
        of false:
          t: T
        of true:
          f: F


    G2[T, F] = object
      case ok: bool
        of false:
          t: T
        of true:
          f: F
          e: int


  var z: G[int, void]; te(z)
  # `void` for type parameter results in Nested `RecList`
  # OfBranch
  #   IntLit 1
  #   RecList
  #     RecList

  var e: G[int, float]; te(e)
  # OfBranch
  #   IntLit 1
  #   RecList
  #     IdentDefs
  #       Sym "f"
  #       Sym "float"
  #       Empty

  var e1: G2[int, void]; te(e1)
  # Adding another field changes nothing
  # OfBranch
  #   IntLit 1
  #   RecList
  #     RecList
  #     IdentDefs
  #       Sym "e"
  #       Sym "int"
  #       Empty

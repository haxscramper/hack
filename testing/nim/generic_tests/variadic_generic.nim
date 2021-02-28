## Attempt to implement variadic generics. (emulate `std::variant`)

## **Results**: very hack-ish solution, but good enough. Since nim
## does not support variadic generics I had to define variadic type
## for each number of arguments separately (`Var1_Impl`, `Var2_Impl`
## and so on) and then write all procedures etc. multiple times.
## Assingment operator is kind of awakward one too: regular `=` has to
## have signature `=(this: var T, other: T)` and cannot be overloaded
## for assigning objects of different types, so I had to use different
## operator.
##
## Procedures that accept variants is another hack (not finished yet)
## - due to limitations of nim type inference for generics I cannot
## just write converter from `T1` to `Var[T0, T1]` - it will generate
## `cannot instantiate T1` error. In C++ it is solved via implicit
## constructors - as far as I'm concerned nim does not have anything
## like this.

import macros
import typetraits
import sequtils

type
  Var1_Impl[T0] = object
    case kind: range[0 .. 0]:
      of 0: f0: T0

  Var2_Impl[T0, T1] = object
    case kind: range[0 .. 1]:
      of 0: f0: T0
      of 1: f1: T1

  Var3_Impl[T0, T1, T2] = object
    case kind: range[0 .. 2]:
      of 0: f0: T0
      of 1: f1: T1
      of 2: f2: T2

proc arity(node: NimNode): int =
  return toSeq(node.children).len

proc tupleTypes(node: NimNode): seq[NimNode] =
  for child in node:
    result.add child

macro variant(types: typedesc): typedesc =
  let impl = getTypeInst(types)
  result = nnkStmtList.newTree(
    nnkBracketExpr.newTree(
      @[ident("Var" & $arity(types) & "_Impl")] &
      tupleTypes(types)
    )
  )

proc `:=`[T0](this: var Var1_Impl[T0], val: T0) = this.f0 = val

proc `:=`[T0, T1](this: var Var2_Impl[T0, T1], val: T0) = this.f0 = val
proc `:=`[T0, T1](this: var Var2_Impl[T0, T1], val: T1) = this.f1 = val

macro variantProc(procDef: untyped): untyped =
  for param in procDef[3]:
    if param.kind == nnkIdentDefs and param[1].kind == nnkCall:
      echo param[1].treeRepr()

  # TODO define overload for original proc that accept types from
  # variant descrioption. This would allow to call variant procs as
  # regular ones with implicit type conversion enabled.

  result = quote do:
    `procDef`



var hello: variant((int, float))

hello := 12

proc varArg(a: variant((char, string))): void {.variantProc.} =
  echo "accepting variant"

proc varArg(a: float): void =
  echo "accepting float"

type
  VarField = object
    f1: variant((char, variant((int, char))))

var inst: VarField

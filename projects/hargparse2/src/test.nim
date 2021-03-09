import macros
import strformat
import result
import hmisc/helpers
import sequtils
import strutils
import options
import deques
import typeinfo
import typetraits
import tables

## Partially successful experiment on passing **ANY** value from
## compile time to runtime as long as type can *exist* at runtime.
## Also might work if you need to pass /value/ of nim node to toplevel
## macro. Currently it works only on types that expos all necessary
## fields.

type
  OptDescBase* = ref object of RootObj
    name*: string
    help*: string
    docstr*: string
    pfix*: seq[string]

  OptDesc* = ref object of OptDescBase
    maxValues*: int
    # parseto*: Option[NimNode] # Disabling this field will allow to use
    #                           # this type at both compile-time and
    #                           # runtime. Otheriwse it is only
    #                           # available at compile-time.

  ArgDesc* = object
    name*: string
    variadic*: bool
    help*: string
    docstr*: string

  Test* = object
    name*: string
    help*: string
    dosctr*: string
    args*: seq[ArgDesc]
    subs*: seq[Test]
    opts*: seq[OptDesc]


proc initCodegen[T](arr: seq[T]): NimNode {.discardable.}

proc initCodegen(val: char | int | bool | string): NimNode {.discardable.} =
  when val is char or val is int or val is string:
    when val is string: newStrLitNode(val)
    elif val is int: newIntLitNode(val)
    else: newIdentNode($val)
  else:
    initCodegenObject(val)

proc initCodegenObject[T: object | tuple](obj: T): NimNode {.discardable.} =
  var fieldInit: seq[NimNode]
  if obj is object:
    fieldInit.add newIdentNode($typeof(T))

  for name, value in obj.fieldPairs:
    when isNamedTuple(typeof obj):
      fieldInit.add initCodegen(value)
    else:
      fieldInit.add nnkExprColonExpr.newTree(
        ident(name), initCodegen(value)
      )

  if obj is object:
    nnkObjConstr.newTree(fieldInit)
  else:
    nnkPar.newTree(fieldInit)

proc initCodegen(obj: object | tuple): NimNode {.discardable.} =
  initCodegenObject(obj)

proc initCodegen(nd: NimNode): NimNode {.discardable.} =
  discard

proc initCodegen[T](arr: seq[T]): NimNode {.discardable.} =
  nnkPrefix.newTree(
    newIdentNode("@"),
    nnkBracket.newTree(arr.mapIt(it.initCodegen())))

proc initCodegen[T](opt: Option[T]): NimNode {.discardable.} =
  if opt.isSome():
    initCodegen(opt.get())
  else:
    quote do:
      none(`T`)

proc initCodegen[K, V](tbl: Table[K, V]): NimNode {.discardable.} =
  var fieldInit: seq[NimNode]
  for key, val in tbl:
    fieldInit.add nnkPar.newTree(
      initCodegen[K](key),
      initCodegen[V](val)
    )

  nnkCall.newTree(
    newIdentNode("toTable"),
    nnkBracket.newTree(fieldInit)
  )

macro convtest(): untyped =
  var t = [(1, Test(name: "hello"))].toTable()
  result = initCodegen(t)
  # result = initCodegen(Test(name: "fff"))
  # result = quote do:
  #   let qqq {.inject.} = `result`

  # result = nnkStmtList.newTree(result)


when false:
  # Const cannot be used to store `ref` objects (and this is expected:
  # `ref` assumes usage of heap and possibility of modification for
  # objects is always present for the heap-allocated type: you can
  # just craete new alias and modify it (well, at least this is my
  # understanding))
  var ctime {.compiletime.} = [(1, Test(name: "hello"))].toTable()
  const runtime = ctime

# Writting object literally in code allows to bypass this limitation:
# result is not a zero-ovearhead, but if you absolutely need to have
# `ref` object this is good enough.
let items = convtest()

# echo [(12, 12)].toTable()


# type
#   TImpl = object
#     f1: seq[int]

# proc tablegenImpl(tbl: Table[int, int]): NimNode {.discardable.} =
#   var fieldInit: seq[NimNode]
#   for key, val in tbl:
#     fieldInit.add nnkPar.newTree(
#       newIntLitNode(12),
#       quote do: TImpl(f1: @[])
#     )

#   nnkCall.newTree(
#     newIdentNode("toTable"),
#     nnkBracket.newTree(fieldInit)
#   )

# macro tablegen(): untyped =
#   result = tablegenImpl([(12, 12)].toTable())
#   echo result.treeRepr()


# discard tablegen()

when false:
  type
    T1 = object
      f1: int
      f2: seq[T1]

    T2 = object
      f1: int
      f2: seq[T1]
      f3: seq[T2]
      f4: seq[NimNode]

  let qqq {.inject.} = toTable([(
    1,
    T2(f1: 12, f2: @[], f3: @[]))
  ])

  let qqq12 {.inject.} = toTable([(
    1,
    Test(name: "hello", help: "", dosctr: "", args: @[], subs: @[], opts: @[]))])

  echo qqq

# Question that I almost asked

#[

I have a macro that generates some code along the lines of `let qqq
{.inject.} = toTable([(1, Test(name: "hello", args: @[], subs: @[],
opts: @[]))])`, but after macro instantiation it fails with
compilation error `Error: request to generate code for .compileTime
proc: toTable` which points to this piece of code:

```
  nnkCall.newTree(
    newIdentNode("toTable"),
    nnkBracket.newTree(fieldInit)
  )
```

but I completely fail to understand what might cause this error: I
generate code that calls `toTable` which is a perfectly find procedure
to call at runtime:

]#

# Correct response

# One of the types uses data structure available only at compile-time:
# to be more precise `NimNode` cannot be accessed by runtime
# procedures. This means any generic type overloaded with `NimNode`
# (or some other `type` that uses `NimNode` as one of the fields) will
# automaticallyt become compile-time.

when false:
  type
    RefTest = ref object
      f1: int
      f2: seq[int]

  var data {.compiletime.}: Table[int, RefTest] = [(
    12, RefTest(f1: 12, f2: @[]))].toTable()

  const pass = data

# Refenrence types still cannot be passed to `const`, but this
# limitation can be worked around using `initCodegen` macro.

# Usage of reference types allows to bypass limitation imposed by the
# `NimNode`: you can inherit from base object that is available at
# both runtime and compile time and add compile-time-only fields to
# derived object.

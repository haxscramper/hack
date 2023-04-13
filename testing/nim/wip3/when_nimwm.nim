import std/macros

macro a(t: typed) =
  echo t.treeRepr()

macro thing() =
  echo "run"


type WhichBackend = enum VM, Js, C, Cpp

proc idPass(b: WhichBackend): WhichBackend {.discardable.} = b

proc getContext(): WhichBackend =
  when nimvm:
    return VM
  else:
    return Js

static:
  echo "ctx", getContext()

echo getContext()

proc someGenericThing[T]() =
  echo "Can do generic? ", getContext()

macro imContext() =
  echo "which context?: ", getContext()
  someGenericThing[int]()

imContext()

proc calledIn() =
  when getContext() == VM:
    echo "CALLED IN VM"
  elif getContext() == Js:
    echo "CALLED IN JS"

static:
  calledIn()

calledIn()


a:
  when defined(js):
    let a = 12
    thing()
  else:
    let b = 23
    thing()

a:
  when nimvm:
    let a = 12
    thing()
  else:
    let b = 23
    thing()

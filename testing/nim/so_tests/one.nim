import std/[macros]

proc fnSignature(fn: NimNode): string =
  let fnImpl =
    case fn.kind:
      of nnkIdent:   fn.getImpl
      of nnkProcDef: fn
      else:          return "invalid usage"

  fnImpl.treeRepr()

macro fnSignatureMacro(fn: typed) =
  echo fn.fnSignature()
  if fn.kind() == nnkProcDef:
    result = fn

proc a(): string {.fnSignatureMacro.} =
  discard

fnSignatureMacro a

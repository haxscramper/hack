import os

const
  dir = currentSourcePath().splitFile().dir
  flags = "gcc -fpic -shared -o" & dir / "shared.so " &
    dir / "type_conversions.c"

static:
  echo flags
  echo gorge(flags)

const shared = dir / "shared.so"

type
  Types = object
    cdeclProc: proc(arg: cint) {.cdecl.}
    closureProc: proc(arg: int)

    arrayOfInt: array[12, int]
    seqOfInt: seq[int]

  CallbackT = proc() {.cdecl.}

proc setCallback(cb: CallbackT) {.dynlib: shared, importc: "setCallback".}
proc invokeCallback() {.dynlib: shared, importc: "invokeCallback".}

setCallback(proc() {.cdecl.} = echo "called callback")
invokeCallback()

echo Types()

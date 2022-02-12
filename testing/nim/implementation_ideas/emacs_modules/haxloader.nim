import ./emacs_api, ./emacs_sugar
import std/[
  macros,
  os,
  strutils,
  osproc,
  times,
  dynlib,
  tables
]

var loaded: Table[string, LibHandle]

var subEnv: EmEnv

proc loadFile(env: EmEnv, file: string, recompile: bool = true): bool {.
  emcallp: "load-file".} =
  subEnv = env
  setEnv(env)
  echo "loading file [", file, "]"
  var nimFile = file
  if not isAbsolute(nimFile):
    for path in env.items(env.symVal("load-path")):
      let path = path as string
      if fileExists(path / file):
        nimFile = path / file
        break

  echo "found ", nimFile
  assert fileExists(nimFile)
  let (path, name, ext) = splitFile(nimFile)
  let soname = path / name & ".so"
  if not fileExists(soname) or
     getLastModificationTime(soname) < getLastModificationTime(nimFile):
    echo "recompiling"
    let res = execCmd(
      "nim c --passc=-g --app=lib -o='$#' '$#'" % [soname, nimFile])

    if res != 0:
      echo "compilation failed"
      return false



  let lib = loadLib(soname)
  if isNil(lib):
    echo "failed to load the library"
    return false

  removeFile soname

  loaded[soname] = lib
  let initf = cast[proc(r: ptr EmRuntime): cint {.cdecl.}](
    lib.symAddr("emacs_module_init"))

  if isNil(initf):
    echo "no init function"
    return false

  var r: EmRuntime
  r.size = sizeof(r).uint()
  echo r.size
  r.getEnvironment = proc(r: ptr EmRuntime): EmEnv {.cdecl.} =
    subEnv

  echo "Calling init"
  if initf(addr r) == 0:
    echo "called init from other module function"
    return true

  return true


emInit():
  echo "initialized module loader"

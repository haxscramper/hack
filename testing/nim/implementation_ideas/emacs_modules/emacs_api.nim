import
  std/[
    compilesettings,
    macros
  ]

{.pragma: emtype, header: "emacs-module.h", bycopy.}

var plugin_is_GPL_compatible {.
  exportc: "plugin_is_GPL_compatible",
  dynlib
.}: cint

type
  EmRuntimePrivate* = object
  EmEnvPrivate* = object

  EmValue* {.emtype, importc: "emacs_value"} = object
  EmArity* = enum
    EmVariadicFunction = -2

  EmRuntime* {.emtype, importc: "struct emacs_runtime".} = object
    size*: uint
    privateMembers* {.importc: "private_members".}: ptr EmRuntimePrivate
    getEnvironment* {.importc: "get_environment".}:
      proc (ert: ptr EmRuntime): ptr EmEnv {.cdecl.}

  EmInitFunction* = proc (ert: ptr EmRuntime): cint {.cdecl.}
  EmProc* = proc (
        env: ptr EmEnv;
        nargs: uint;
        args: ptr EmValue;
        a4: pointer): EmValue {.cdecl.}

  EmProcNim* = proc(env: ptr EmEnv, nargs: uint, args: ptr EmValue)

  EmFuncallExit* = enum
    EmFuncallExitReturn = 0
    EmFuncallExitSignal = 1
    EmFuncallExitThrow = 2

  EmMakeFunctionNim = proc(
      env: ptr EmEnv;
      minArity: uint;
      maxArity: uint;
      function: EmProc;
      documentation: cstring): EmValue

  EmEnv* {.emtype, importc: "emacs_env".} = object
    size*: uint
    privateMembers* {.importc: "private_members".}: ptr EmEnvPrivate

    makeGlobalRef* {.importc: "make_global_ref".}:
      proc (env: ptr EmEnv; anyReference: EmValue): EmValue {.cdecl.}

    freeGlobalRef* {.importc: "free_global_ref".}:
      proc (env: ptr EmEnv; globalReference: EmValue) {.cdecl.}

    nonLocalExitCheck* {.importc: "make_local_exit_check".}:
      proc (env: ptr EmEnv): EmFuncallExit {.cdecl.}

    nonLocalExitClear* {.importc: "non_local_exit_check".}:
      proc (env: ptr EmEnv) {.cdecl.}

    nonLocalExitGet* {.importc: "non_local_exit_get".}: proc (
      env: ptr EmEnv;
      nonLocalExitSymbolOut: ptr EmValue;
      nonLocalExitDataOut: ptr EmValue): EmFuncallExit {.cdecl.}

    nonLocalExitSignal* {.importc: "non_local_exit_signal"}: proc (
      env: ptr EmEnv;
      nonLocalExitSymbol: EmValue;
      nonLocalExitData: EmValue) {.cdecl.}

    nonLocalExitThrow* {.importc: "non_local_exit_throw"}: proc (
      env: ptr EmEnv; tag: EmValue; value: EmValue) {.cdecl.}

    makeFunction* {.importc: "make_function"}: proc (
      env: ptr EmEnv,
      minArity: uint,
      maxArity: uint,
      function: EmProc,
      documentation: cstring,
      data: pointer): EmValue {.cdecl.}

    funcallImpl* {.importc: "funcall"}: proc (
      env: ptr EmEnv;
      function: EmValue;
      nargs: uint;
      args: ptr EmValue): EmValue {.cdecl.}

    internImpl* {.importc: "intern"}: proc (
      env: ptr EmEnv; symbolName: cstring): EmValue {.cdecl.}

    typeOf* {.importc: "type_of"}: proc (
      env: ptr EmEnv; value: EmValue): EmValue {.cdecl.}

    isNotNil* {.importc: "is_not_nil"}: proc (
      env: ptr EmEnv; value: EmValue): bool {.cdecl.}

    eq* {.importc: "eq"}: proc (
      env: ptr EmEnv; a: EmValue; b: EmValue): bool {.cdecl.}

    extractInteger* {.importc: "extract_integer"}: proc (
      env: ptr EmEnv; value: EmValue): int64 {.cdecl.}

    makeInteger* {.importc: "make_integer"}: proc (
      env: ptr EmEnv; value: int64): EmValue {.cdecl.}

    extractFloat* {.importc: "extract_float"}: proc (
      env: ptr EmEnv; value: EmValue): cdouble {.cdecl.}

    makeFloat* {.importc: "make_float"}: proc (
      env: ptr EmEnv; value: cdouble): EmValue {.cdecl.}

    copyStringContents* {.importc: "copy_string_contents"}: proc (
      env: ptr EmEnv;
      value: EmValue;
      buffer: cstring;
      sizeInout: ptr uint): bool {.cdecl.}

    makeString* {.importc: "make_string"}: proc (
      env: ptr EmEnv;
      contents: cstring;
      length: uint): EmValue {.cdecl.}

    makeUserPtr* {.importc: "make_user_ptr"}: proc (
        env: ptr EmEnv;
        fin: proc (a1: pointer);
        `ptr`: pointer): EmValue {.cdecl.}

    getUserPtr* {.importc: "get_user_ptr"}: proc (
        env: ptr EmEnv; uptr: EmValue): pointer {.cdecl.}

    setUserPtr* {.importc: "set_user_ptr"}: proc (
      env: ptr EmEnv; uptr: EmValue; `ptr`: pointer)

    # void (*(*getUserFinalizer) (EmEnv *env, EmValue uptr)) (void *);
    setUserFinalizer* {.importc: "set_user_finalizer"}: proc (
      env: ptr EmEnv; uptr: EmValue; fin: proc (a1: pointer)) {.cdecl.}

    vecGet* {.importc: "vec_get"}: proc (
      env: ptr EmEnv; vec: EmValue; i: uint): EmValue {.cdecl.}

    vecSet* {.importc: "vec_set"}: proc (
      env: ptr EmEnv; vec: EmValue; i: uint; val: EmValue) {.cdecl.}

    vecSize* {.importc: "vec_size"}: proc (env: ptr EmEnv; vec: EmValue): uint {.cdecl.}

proc EmModuleInit*(ert: ptr EmRuntime): cint {.importc.}

type
  PemEnv = ptr EmEnv

proc intern(env: PemEnv, name: string): EmValue =
  result = env.internImpl(env, name.cstring)

proc funcall(env: PemEnv, fun: EmValue, args: seq[EmValue]): EmValue =
  if len(args) == 0:
    env.funcallImpl(env, fun, 0, nil)

  else:
    env.funcallImpl(env, fun, args.len.uint, unsafeAddr args[0])

proc funcall(env: PemEnv, fun: string, args: seq[EmValue]): EmValue =
  funcall(env, env.intern(fun), args)

type
  EmProcData = object
    minArity: int
    maxArity: int
    name: string
    docstring: string
    impl: EmProc
    data: pointer

proc defun(env: PemEnv, impl: EmProcData) =
  discard env.funcall("defalias", @[
    env.intern(impl.name),
    env.makeFunction(
      env,
      impl.minArity.uint,
      impl.maxArity.uint,
      impl.impl,
      impl.docstring.cstring,
      impl.data
    )
  ])

template defun(
    nowEnv: PemEnv,
    procName: string,
    argLen: Slice[int],
    doc: string,
    body: untyped
  ): untyped =

  block:
    proc implProc(
        env {.inject.}: PemEnv,
        nargs {.inject.}: uint,
        args {.inject.}: ptr EmValue
      ): EmValue {.closure.} =

      body

    let impl = EmProcData(
      name: procName,
      docstring: doc,
      minArity: argLen.a,
      maxArity: argLen.b,
      impl: cast[EmProc](rawProc(implProc)),
      data: rawEnv(implProc)
    )

    nowEnv.defun(impl)

const
  emcallPrefix {.strdefine.}: string = ""

# proc treeRepr(env: PemEnv, )

proc fromEmacs[I: SomeInteger](env: PemEnv, target: var I, value: EmValue) =
  target = I(env.extractInteger(env, value))

proc toEmacs(env: PemEnv, value: SomeInteger): EmValue =
  env.makeInteger(env, value.int64)

macro emcall(impl: untyped): untyped =
  echo treeRepr(impl)

  let
    wrapName = ident(impl.name().strVal() & "Emcall")
    wrapImpl = ident(impl.name().strVal() & "Emprox")
    nargs = ident("nargs")
    args = ident("args")
    env = ident("env")

  var data: EmProcData
  data.name = emcallPrefix & impl.name().strVal()
  data.maxArity = impl.params().len() - 1

  var
    implRepack = newStmtList()
    recall = newCall(impl.name())
    count = 0


  for arg in impl.params()[1..^1]:
    let typ = arg[^2]
    for name in arg[0..^3]:
      var pass = ident(name.strVal())

      implRepack.add quote do:
        var `pass`: `typ` = default(`typ`)
        if `count` < `nargs`:
          fromEmacs(`env`, `pass`, `args`[`count`])

      recall.add pass
      inc count

  result = quote do:
    `impl`

    let `wrapImpl` = proc(
      `env`: PemEnv,
      `nargs`: uint,
      `args`: ptr UncheckedArray[EmValue]
    ): EmValue {.closure.} =
      `implRepack`
      return toEmacs(`env`, `recall`)

    let `wrapName` =
      block:
        var base = `data`
        base.impl = cast[EmProc](rawProc(`wrapImpl`))
        base.data = rawEnv(`wrapImpl`)

        base


proc returnValue(val: int, other: int = 12): int {.emcall.} =
  result = val + 12

echo returnValueEmcall

template emInit(body: untyped): untyped =
  proc init(runtime {.inject.}: ptr EmRuntime): cint {.
    exportc: "emacs_module_init", cdecl, dynlib.} =

    var env {.inject.}: PemEnv = runtime.getEnvironment(runtime)

    assert not isNil(env)

    body

    discard env.funcall("provide", @[env.intern("emacs_api")])

    return 0

emInit():
  echo "initalized emacs"

  env.defun("test", 0..0, "doc"):
    return env.makeInteger(env, 123)

  env.defun(returnValueEmcall)

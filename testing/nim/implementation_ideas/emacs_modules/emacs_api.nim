type
  ptrdiffT = uint

var plugin_is_GPL_compatible {.exportc.}: cint

type
  EmValueTag* = object
  EmRuntimePrivate* = object
  EmEnvPrivate* = object

  EmEnv* = EmEnv25
  EmValue* = ptr EmValueTag
  EmArity* = enum
    EmVariadicFunction = -2

  EmRuntime* {.bycopy.} = object
    size*: ptrdiffT
    privateMembers* {.importc: "private_members".}: ptr EmRuntimePrivate
    getEnvironment* {.importc: "get_environment".}: proc (ert: ptr EmRuntime): ptr EmEnv

  EmInitFunction* = proc (ert: ptr EmRuntime): cint {.cdecl.}
  EmProc* = proc (env: ptr EmEnv; nargs: ptrdiffT; args: ptr EmValue;
                   data: pointer): EmValue {.cdecl.}

  EmProcNim* = proc(env: ptr EmEnv, nargs: ptrdiffT, args: ptr EmValue)

  EmFuncallExit* = enum
    EmFuncallExitReturn = 0
    EmFuncallExitSignal = 1
    EmFuncallExitThrow = 2

  EmMakeFunctionNim = proc(
      env: ptr EmEnv;
      minArity: ptrdiffT;
      maxArity: ptrdiffT;
      function: proc (
        env: ptr EmEnv;
        nargs: ptrdiffT;
        args: ptr EmValue;
        a4: pointer): EmValue;
      documentation: cstring): EmValue

  EmEnv25* {.bycopy.} = object
    size*: ptrdiffT
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
      env: ptr EmEnv;
      minArity: ptrdiffT;
      maxArity: ptrdiffT;
      function: proc (
        env: ptr EmEnv;
        nargs: ptrdiffT;
        args: ptr EmValue;
        a4: pointer): EmValue;
      documentation: cstring;
      data: pointer): EmValue {.cdecl.}

    funcall* {.importc: "funcall"}: proc (
      env: ptr EmEnv;
      function: EmValue;
      nargs: ptrdiffT;
      args: ptr EmValue): EmValue

    intern* {.importc: "intern"}: proc (env: ptr EmEnv; symbolName: cstring): EmValue {.cdecl.}
    typeOf* {.importc: "type_of"}: proc (env: ptr EmEnv; value: EmValue): EmValue {.cdecl.}
    isNotNil* {.importc: "is_not_nil"}: proc (env: ptr EmEnv; value: EmValue): bool {.cdecl.}
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
      sizeInout: ptr ptrdiffT): bool {.cdecl.}

    makeString* {.importc: "make_string"}: proc (
      env: ptr EmEnv;
      contents: cstring;
      length: ptrdiffT): EmValue {.cdecl.}

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
      env: ptr EmEnv; vec: EmValue; i: ptrdiffT): EmValue {.cdecl.}

    vecSet* {.importc: "vec_set"}: proc (
      env: ptr EmEnv; vec: EmValue; i: ptrdiffT; val: EmValue) {.cdecl.}

    vecSize* {.importc: "vec_size"}: proc (env: ptr EmEnv; vec: EmValue): ptrdiffT {.cdecl.}

proc EmModuleInit*(ert: ptr EmRuntime): cint {.importc.}

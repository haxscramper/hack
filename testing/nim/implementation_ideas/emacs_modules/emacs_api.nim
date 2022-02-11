import
  std/[
    compilesettings,
    macros,
    strutils,
    options
  ]

{.pragma: emtype, header: "emacs-module.h", bycopy.}

var plugin_is_GPL_compatible {.
  exportc: "plugin_is_GPL_compatible",
  dynlib
.}: cint

type
  EmRuntimePrivate* = object
  EmEnvObjPrivate* = object

  EmValue* {.emtype, importc: "emacs_value"} = object
  EmArity* = enum
    EmVariadicFunction = -2

  EmRuntime* {.emtype, importc: "struct emacs_runtime".} = object
    size*: uint
    privateMembers* {.importc: "private_members".}: ptr EmRuntimePrivate
    getEnvironment* {.importc: "get_environment".}:
      proc (ert: ptr EmRuntime): ptr EmEnvObj {.cdecl.}

  EmInitFunction* = proc (ert: ptr EmRuntime): cint {.cdecl.}
  EmProc* = proc (
        env: ptr EmEnvObj;
        nargs: uint;
        args: ptr EmValue;
        a4: pointer): EmValue {.cdecl.}

  EmProcNim* = proc(env: ptr EmEnvObj, nargs: uint, args: ptr EmValue)

  EmFuncallExit* = enum
    emFuncallExitReturn = 0 ## The last API function exited normally.
    emFuncallExitSignal = 1 ## The last API function signaled an error.
    emFuncallExitThrow = 2 ## The last API function exited via throw.

  EmMakeFunctionNim = proc(
      env: ptr EmEnvObj;
      minArity: uint;
      maxArity: uint;
      function: EmProc;
      documentation: cstring): EmValue

  EmEnvObj* {.emtype, importc: "emacs_env".} = object
    size*: uint
    privateMembers* {.importc: "private_members".}: ptr EmEnvObjPrivate

    makeGlobalRef* {.importc: "make_global_ref".}:
      proc (env: ptr EmEnvObj; anyReference: EmValue): EmValue {.cdecl.}

    freeGlobalRef* {.importc: "free_global_ref".}:
      proc (env: ptr EmEnvObj; globalReference: EmValue) {.cdecl.}

    nonLocalExitCheck* {.importc: "non_local_exit_check".}:
      proc (env: ptr EmEnvObj): EmFuncallExit {.cdecl.} ##[

This function returns the kind of nonlocal exit condition stored in env.

]##

    nonLocalExitClear* {.importc: "non_local_exit_clear".}:
      proc (env: ptr EmEnvObj) {.cdecl.} ##[

This function clears the pending nonlocal exit conditions and data from
env. After calling it, the module API functions will work normally. Use
this function if your module function can recover from nonlocal exits of
the Lisp functions it calls and continue, and also before calling any of
the following two functions (or any other API functions, if you want them
to perform their intended processing when a nonlocal exit is pending).

]##

    nonLocalExitGet* {.importc: "non_local_exit_get".}: proc (
      env: ptr EmEnvObj;
      nonLocalExitSymbolOut: ptr EmValue;
      nonLocalExitDataOut: ptr EmValue): EmFuncallExit {.cdecl.} ##[

This function returns the kind of nonlocal exit condition stored in env,
like non_local_exit_check does, but it also returns the full information
about the nonlocal exit, if any. If the return value is
emacs_funcall_exit_signal, the function stores the error symbol in *symbol
and the error data in *data (see Signaling Errors). If the return value is
emacs_funcall_exit_throw, the function stores the catch tag symbol in
*symbol and the throw value in *data. The function doesn’t store anything
in memory pointed by these arguments when the return value is
emacs_funcall_exit_return.

]##

    nonLocalExitSignal* {.importc: "non_local_exit_signal"}: proc (
      env: ptr EmEnvObj;
      nonLocalExitSymbol: EmValue;
      nonLocalExitData: EmValue) {.cdecl.} ##[

This function signals the error represented by error with the specified
error data `data`. The module function should return soon after calling this
function. This function could be useful, e.g., for signaling errors from
module functions to Emacs.

]##

    nonLocalExitThrow* {.importc: "non_local_exit_throw"}: proc (
      env: ptr EmEnvObj; tag: EmValue; value: EmValue) {.cdecl.} ##[

This function throws to the Lisp catch symbol represented by tag, passing
it value as the value to return. Your module function should in general
return soon after calling this function. One use of this function is when
you want to re-throw a non-local exit from one of the called API or Lisp
functions.

]##

    makeFunction* {.importc: "make_function"}: proc (
      env: ptr EmEnvObj,
      minArity: uint,
      maxArity: uint,
      function: EmProc,
      documentation: cstring,
      data: pointer): EmValue {.cdecl.}

    funcallImpl* {.importc: "funcall"}: proc (
      env: ptr EmEnvObj;
      function: EmValue;
      nargs: uint;
      args: ptr EmValue): EmValue {.cdecl.} ##[

This function calls the specified func passing it nargs arguments from the
array pointed to by args. The argument func can be a function symbol (e.g.,
returned by intern described above), a module function returned by
make_function (see Module Functions), a subroutine written in C, etc. If
nargs is zero, args can be a NULL pointer.

The function returns the value that func returned.

]##

    internImpl* {.importc: "intern"}: proc (
      env: ptr EmEnvObj; symbolName: cstring): EmValue {.cdecl.}

    typeOfImpl* {.importc: "type_of"}: proc (
      env: ptr EmEnvObj; value: EmValue): EmValue {.cdecl.}

    isNotNil* {.importc: "is_not_nil"}: proc (
      env: ptr EmEnvObj; value: EmValue): bool {.cdecl.}

    eq* {.importc: "eq"}: proc (
      env: ptr EmEnvObj; a: EmValue; b: EmValue): bool {.cdecl.}

    extractInteger* {.importc: "extract_integer"}: proc (
      env: ptr EmEnvObj; value: EmValue): int64 {.cdecl.}

    makeInteger* {.importc: "make_integer"}: proc (
      env: ptr EmEnvObj; value: int64): EmValue {.cdecl.}

    extractFloat* {.importc: "extract_float"}: proc (
      env: ptr EmEnvObj; value: EmValue): cdouble {.cdecl.}

    makeFloat* {.importc: "make_float"}: proc (
      env: ptr EmEnvObj; value: cdouble): EmValue {.cdecl.}

    copyStringContents* {.importc: "copy_string_contents"}: proc (
      env: ptr EmEnvObj;
      value: EmValue;
      buffer: cstring;
      sizeInout: ptr uint): bool {.cdecl.}  ##[

This function stores the UTF-8 encoded text of a Lisp string specified by
arg in the array of char pointed by buf, which should have enough space to
hold at least *len bytes, including the terminating null byte. The argument
len must not be a NULL pointer, and, when the function is called, it should
point to a value that specifies the size of buf in bytes.

If the buffer size specified by *len is large enough to hold the string’s
text, the function stores in *len the actual number of bytes copied to buf,
including the terminating null byte, and returns true. If the buffer is too
small, the function raises the args-out-of-range error condition, stores
the required number of bytes in *len, and returns false. See Module
Nonlocal, for how to handle pending error conditions.

The argument buf can be a NULL pointer, in which case the function stores
in *len the number of bytes required for storing the contents of arg, and
returns true. This is how you can determine the size of buf needed to store
a particular string: first call copy_string_contents with NULL as buf, then
allocate enough memory to hold the number of bytes stored by the function
in *len, and call the function again with non-NULL buf to actually perform
the text copying.

]##

    makeString* {.importc: "make_string"}: proc (
      env: ptr EmEnvObj;
      contents: cstring;
      length: uint): EmValue {.cdecl.}

    makeUserPtr* {.importc: "make_user_ptr"}: proc (
        env: ptr EmEnvObj;
        fin: proc (a1: pointer);
        `ptr`: pointer): EmValue {.cdecl.}

    getUserPtr* {.importc: "get_user_ptr"}: proc (
        env: ptr EmEnvObj; uptr: EmValue): pointer {.cdecl.}

    setUserPtr* {.importc: "set_user_ptr"}: proc (
      env: ptr EmEnvObj; uptr: EmValue; `ptr`: pointer)

    # void (*(*getUserFinalizer) (EmEnvObj *env, EmValue uptr)) (void *);
    setUserFinalizer* {.importc: "set_user_finalizer"}: proc (
      env: ptr EmEnvObj; uptr: EmValue; fin: proc (a1: pointer)) {.cdecl.}

    vecGetImpl* {.importc: "vec_get"}:
      proc(env: ptr EmEnvObj; vec: EmValue; i: uint): EmValue {.cdecl.}

    vecSetImpl* {.importc: "vec_set"}:
      proc(env: ptr EmEnvObj; vec: EmValue; i: uint; val: EmValue) {.cdecl.}

    vecSizeImpl* {.importc: "vec_size"}:
      proc(env: ptr EmEnvObj; vec: EmValue): uint {.cdecl.}

proc EmModuleInit*(ert: ptr EmRuntime): cint {.importc.}

type
  EmEnv = ptr EmEnvObj

  EmNonlocalSignal* = object of CatchableError

proc intern(env: EmEnv, name: string): EmValue =
  result = env.internImpl(env, name.cstring)

proc intVal*(env: EmEnv, value: EmValue): int64 =
  env.extractInteger(env, value)

proc floatVal*(env: EmEnv, value: EmValue): cdouble =
  env.extractFloat(env, value)

proc strVal*(env: EmEnv, value: EmValue): string =
  var size: uint = 0
  discard env.copyStringContents(env, value, nil, addr size)
  result = newString(size - 1)
  discard env.copyStringContents(env, value, addr result[0], addr size)

proc symStrVal*(env: EmEnv, value: EmValue): string

proc vecGet*(env: EmEnv, vec: EmValue, idx: int): EmValue =
  env.vecGetImpl(env, vec, idx.uint)

proc vecSet*(env: EmEnv, vec: EmValue, idx: int, val: EmValue) =
  env.vecSetImpl(env, vec, idx.uint, val)

proc vecLen*(env: EmEnv, vec: EmValue): int =
  env.vecSizeImpl(env, vec).int

iterator vecItems*(env: EmEnv, vec: EmValue): EmValue =
  for idx in 0 ..< env.vecLen(vec):
    yield env.vecGet(vec, idx)

proc boolVal*(env: EmEnv, value: EmValue): bool =
  if env.isNotNil(env, value):
    return true

  else:
    return false

proc emTypeof*(env: EmEnv, value: EmValue): string =
  env.symStrVal(env.typeOfImpl(env, value))


proc emVal*(env: EmEnv, value: string): EmValue =
  env.makeString(env, value.cstring, value.len.uint)

proc funcallRaw*(env: EmEnv, fun: EmValue, args: seq[EmValue]): EmValue =
  if len(args) == 0:
    result = env.funcallImpl(env, fun, 0, nil)

  else:
    result = env.funcallImpl(env, fun, args.len.uint, unsafeAddr args[0])

proc treeRepr*(env: EmEnv, value: EmValue): string =
  var res = addr result
  proc add(args: varargs[string, `$`]) =
    for arg in args:
      res[].add arg

  proc addi(level: int, args: varargs[string, `$`]) =
    add repeat("  ", level)
    add args


  proc aux(value: EmValue, level: int) =
    let typ = env.emTypeof(value)
    addi level, typ
    case typ:
      of "cons":
        add "\n"
        addi level + 1, "car\n"
        aux(env.funcallRaw(env.intern("car"), @[value]), level + 2)
        add "\n"
        addi level + 1, "cdr\n"
        aux(env.funcallRaw(env.intern("cdr"), @[value]), level + 2)

      else:
        add "[[!" & $typ & "!]]"


  aux(value, 0)


proc funcall*(
    env: EmEnv,
    fun: EmValue,
    name: string,
    args: seq[EmValue],
    checkErr: bool
  ): EmValue {.discardable.} =

  result = funcallRaw(env, fun, args)

  if not checkErr:
    return

  var
    symOut: EmValue
    dataOut: EmValue

  case env.nonLocalExitGet(env, addr symOut, addr dataOut):
    of emFuncallExitSignal:
      env.nonLocalExitClear(env)
      var err: ref EmNonlocalSignal
      new(err)

      err.msg = "Call to '$#' exited via signal '$#. Err data $#" % [
        name,
        env.strVal(env.funcallRaw(env.intern("symbol-name"), @[symOut])),
        env.treeRepr(dataOut)
      ]

      raise err

    else:
      discard

proc funcall*(
    env: EmEnv, fun: string, args: seq[EmValue] = @[],
    checkErr: bool = true
  ): EmValue {.discardable.} =

  return funcall(env, env.intern(fun), fun, args, checkErr = checkErr)

proc symStrVal*(env: EmEnv, value: EmValue): string =
  env.strVal(env.funcall("symbol-name", @[value]))

type
  EmAtom = distinct EmValue
  EmArray = distinct EmValue
  EmBignum = distinct EmValue
  EmBoolVector = distinct EmValue
  EmBool = distinct EmValue
  EmBuffer = distinct EmValue
  EmByteCodeFunction = distinct EmValue
  EmCaseTable = distinct EmValue
  EmCharOrString = distinct EmValue
  EmCharTable = distinct EmValue
  EmCommand = distinct EmValue
  EmConditionVariable = distinct EmValue
  EmCons = distinct EmValue
  EmCustomVariable = distinct EmValue
  EmFixnum = distinct EmValue
  EmFloat = distinct EmValue
  EmFont = distinct EmValue
  EmFrameConfiguration = distinct EmValue
  EmFrameLive = distinct EmValue
  EmFrame = distinct EmValue
  EmFunction = distinct EmValue
  EmHashTable = distinct EmValue
  EmIntegerOrMarker = distinct EmValue
  EmInteger = distinct EmValue
  EmKeymap = distinct EmValue
  EmKeyword = distinct EmValue
  EmList = distinct EmValue
  EmMarker = distinct EmValue
  EmMutex = distinct EmValue
  EmNlist = distinct EmValue
  EmNumberOrMarker = distinct EmValue
  EmNumber = distinct EmValue
  EmOverlay = distinct EmValue
  EmProcess = distinct EmValue
  EmRecord = distinct EmValue
  EmSequence = distinct EmValue
  EmStringOrNull = distinct EmValue
  EmString = distinct EmValue
  EmSubr = distinct EmValue
  EmSymbol = distinct EmValue
  EmSyntaxTable = distinct EmValue
  EmThread = distinct EmValue
  EmVector = distinct EmValue
  EmWholenum = distinct EmValue
  EmWindowConfiguration = distinct EmValue
  EmWindowLive = distinct EmValue
  EmWindow = distinct EmValue

func emTypePredicate[T](expect: typedesc[T]): string =
  when T is EmAtom: "atom"
  elif T is EmArray: "arrayp"
  elif T is EmBignum: "bignump"
  elif T is EmBoolVector: "bool-vector-p"
  elif T is EmBool: "booleanp"
  elif T is EmBuffer: "bufferp"
  elif T is EmByteCodeFunction: "byte-code-function"
  elif T is EmCaseTable: "case-table-p"
  elif T is EmCharOrString: "char-or-string-p"
  elif T is EmCharTable: "char-table-p"
  elif T is EmCommand: "commandp"
  elif T is EmConditionVariable: "condition-variable-p"
  elif T is EmCons: "consp"
  elif T is EmCustomVariable: "custom-variable-p"
  elif T is EmFixnum: "fixnump"
  elif T is EmFloat: "floatp"
  elif T is EmFont: "fontp"
  elif T is EmFrameConfiguration: "frame-configuration-p"
  elif T is EmFrameLive: "frame-live-p"
  elif T is EmFrame: "framep"
  elif T is EmFunction: "functionp"
  elif T is EmHashTable: "hash-table-p"
  elif T is EmIntegerOrMarker: "integer-or-marker-p"
  elif T is EmInteger: "integerp"
  elif T is EmKeymap: "keymapp"
  elif T is EmKeyword: "keywordp"
  elif T is EmList: "listp"
  elif T is EmMarker: "markerp"
  elif T is EmMutex: "mutexp"
  elif T is EmNlist: "nlistp"
  elif T is EmNumberOrMarker: "number-or-marker-p"
  elif T is EmNumber: "numberp"
  elif T is EmOverlay: "overlayp"
  elif T is EmProcess: "processp"
  elif T is EmRecord: "recordp"
  elif T is EmSequence: "sequencep"
  elif T is EmStringOrNull: "string-or-null-p"
  elif T is EmString: "stringp"
  elif T is EmSubr: "subrp"
  elif T is EmSymbol: "symbolp"
  elif T is EmSyntaxTable: "syntax-table-p"
  elif T is EmThread: "threadp"
  elif T is EmVector: "vectorp"
  elif T is EmWholenum: "wholenump"
  elif T is EmWindowConfiguration: "window-configuration-p"
  elif T is EmWindowLive: "window-live-p"
  elif T is EmWindow: "windowp"
  else:
    {.error: "Unexpected type for mismatch checking - " & $T.}

func getValidationCall[I: SomeInteger](expect: typedesc[I]): string =
  emTypePredicate(EmNumber)

type
  EmTypeError = object of CatchableError


proc mismatch[T](env: EmEnv, value: EmValue, expect: T):
    Option[tuple[want, got: string]] =

  let callname = getValidationCall(T)
  if not env.boolVal(env.funcall(callname, @[value])):
    result = some (callname, env.emTypeof(value))

proc expectValid[T](
    env: EmEnv, value: EmValue, expect: T,
    desc: string = ""
  ): bool =
  ## Return `true` if value matches expected type, or raise `EmTypeError`
  let err = mismatch(env, value, expect)
  if err.isSome():
    let (want, got) = err.get()

    raise newException(
      EmTypeError,
      "Nim type mismatch - expected '$#', but got '$#'$#" % [
        want, got,
        if 0 < len(desc): ". " & desc else: ""
    ])

  else:
    return true


proc fromEmacs[I: SomeInteger](
    env: EmEnv, target: var I, value: EmValue, check: bool = true) =
  if not check or expectValid(env, value, target):
    target = I(env.extractInteger(env, value))

proc toEmacs(env: EmEnv, value: SomeInteger): EmValue =
  env.makeInteger(env, value.int64)

proc toEmacs(env: EmEnv, value: string): EmValue =
  env.makeString(env, value.cstring, value.len.uint)

proc funcall*[Args](
    env: EmEnv, name: string, args: Args,
    checkErr: bool = true
  ): EmValue {.discardable.} =

  var emArgs: seq[EmValue]
  for name, value in fieldPairs(args):
    emArgs.add env.toEmacs(value)

  return env.funcall(name, emArgs, checkErr = checkErr)




type
  EmProcData = object
    minArity: int
    maxArity: int
    name: string
    docstring: string
    impl: EmProc
    data: pointer

proc defun(env: EmEnv, impl: EmProcData) =
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
    nowEnv: EmEnv,
    procName: string,
    argLen: Slice[int],
    doc: string,
    body: untyped
  ): untyped =

  block:
    proc implProc(
        env {.inject.}: EmEnv,
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
  emcallNamespace {.strdefine.}: string = ""

template emError(env: EmEnv): untyped =
  {.line: instantiationInfo(fullPaths = true).}:
    let ex = getCurrentException()
    let trace = ex.getStackTrace()
    let msg = getCurrentExceptionMsg()
    echo "Raising error"
    discard env.funcallRaw(
      env.intern("error"), @[env.emVal(
        "Nim exception: $# - $#\n$#" % [$ex.name, $ex.msg, trace]
      )])

    # quit QuitFailure


macro emcall(impl: untyped): untyped =
  echo treeRepr(impl)

  let
    wrapName = ident(impl.name().strVal() & "Emcall")
    wrapImpl = ident(impl.name().strVal() & "Emprox")
    nargs = ident("nargs")
    args = ident("args")
    env = ident("env")

  var data: EmProcData
  if 0 < len(emcallNamespace):
    data.name = emcallNamespace & ":"

  data.name.add impl.name().strVal()
  data.maxArity = impl.params().len() - 1
  let implName = impl.name().toStrLit()

  var
    implRepack = newStmtList()
    recall = newCall(impl.name())
    count = 0

  let info = newLit(impl.lineInfoObj())

  for arg in impl.params()[1..^1]:
    let typ = arg[^2]
    for name in arg[0..^3]:
      var pass = ident(name.strVal())
      let argname = name.toStrLit()
      implRepack.add quote do:
        var `pass`: `typ` = default(`typ`)
        if `count` < `nargs`:
          if expectValid(
            `env`,
            `args`[`count`],
            `pass`,
            desc = (
              "Argument '" & `argname` & "' for proc '" & `implName` & "' " &
                "defined in " & $`info`
            )
          ):
            fromEmacs(`env`, `pass`, `args`[`count`], check = false)

      recall.add pass
      inc count

  result = quote do:
    `impl`

    let `wrapImpl` = proc(
      `env`: EmEnv,
      `nargs`: uint,
      `args`: ptr UncheckedArray[EmValue]
    ): EmValue {.closure.} =
      `implRepack`
      try:
        result = toEmacs(`env`, `recall`)

      except:
        emError(`env`)

    let `wrapName` =
      block:
        var base = `data`
        base.impl = cast[EmProc](rawProc(`wrapImpl`))
        base.data = rawEnv(`wrapImpl`)

        base

type
  EmDefun[Args, Ret] = object
    name: string

func getfun*[Args, Ret](
    name: string, args: typedesc[Args], ret: typedesc[Ret]
  ): EmDefun[Args, Ret] =

  result.name = name


proc funcall*[Args, Ret](
    env: EmEnv, fun: EmDefun[Args, Ret],
    args: Args,
    checkErr: bool = true): Ret =
  env.fromEmacs(result, env.funcall(fun.name, args, checkErr = checkErr))


proc funcall*[Args, Ret](
    env: EmEnv, fun: EmDefun[Args, Ret],
    checkErr: bool = true): Ret =
  env.fromEmacs(result, env.funcall(fun.name, @[], checkErr = checkErr))

proc returnValue(val: int, other: int = 12): int {.emcall.} =
  result = val + 12

echo returnValueEmcall


template emInit(body: untyped): untyped =
  proc init(runtime {.inject.}: ptr EmRuntime): cint {.
    exportc: "emacs_module_init", cdecl, dynlib.} =

    var env {.inject.}: EmEnv = runtime.getEnvironment(runtime)

    assert not isNil(env)

    try:
      body
      discard env.funcall("provide", @[env.intern("emacs_api")])

    except:
      env.emError()

    return 0

let pointMin = getfun("point-min", void, int)
let pointMax = getfun("point-max", void, int)

emInit():
  echo "initalized emacs"

  env.defun("test", 0..0, "doc"):
    return env.toEmacs(123)

  env.funcall("point-min", (1, 2, 3))
  env.defun(returnValueEmcall)

  echo "from emacs: ", env.funcall(pointMin)
  echo "from emacs: ", env.funcall(pointMax)

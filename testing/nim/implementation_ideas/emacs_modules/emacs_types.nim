{.pragma: emtype, header: "emacs-module.h", bycopy.}

import std/options

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

    eqImpl* {.importc: "eq"}: proc (
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

type
  EmEnv* = ptr EmEnvObj

  EmNonlocalSignal* = object of CatchableError

  EmWrongNumberOfArguments* = object of EmNonLocalSignal

type
  EmAtom* = distinct EmValue
  EmArray* = distinct EmValue
  EmBignum* = distinct EmValue
  EmBoolVector* = distinct EmValue
  EmBool* = distinct EmValue
  EmBuffer* = distinct EmValue
  EmByteCodeFunction* = distinct EmValue
  EmCaseTable* = distinct EmValue
  EmCharOrString* = distinct EmValue
  EmCharTable* = distinct EmValue
  EmCommand* = distinct EmValue
  EmConditionVariable* = distinct EmValue
  EmCons* = distinct EmValue
  EmCustomVariable* = distinct EmValue
  EmFixnum* = distinct EmValue
  EmFloat* = distinct EmValue
  EmFont* = distinct EmValue
  EmFrameConfiguration* = distinct EmValue
  EmFrameLive* = distinct EmValue
  EmFrame* = distinct EmValue
  EmFunction* = distinct EmValue
  EmHashTable* = distinct EmValue
  EmIntegerOrMarker* = distinct EmValue
  EmInteger* = distinct EmValue
  EmKeymap* = distinct EmValue
  EmKeyword* = distinct EmValue
  EmList* = distinct EmValue
  EmMarker* = distinct EmValue
  EmMutex* = distinct EmValue
  EmNlist* = distinct EmValue
  EmNumberOrMarker* = distinct EmValue
  EmNumber* = distinct EmValue
  EmOverlay* = distinct EmValue
  EmProcess* = distinct EmValue
  EmRecord* = distinct EmValue
  EmSequence* = distinct EmValue
  EmStringOrNull* = distinct EmValue
  EmString* = distinct EmValue
  EmSubr* = distinct EmValue
  EmSymbol* = distinct EmValue
  EmSyntaxTable* = distinct EmValue
  EmThread* = distinct EmValue
  EmVector* = distinct EmValue
  EmWholenum* = distinct EmValue
  EmWindowConfiguration* = distinct EmValue
  EmWindowLive* = distinct EmValue
  EmWindow* = distinct EmValue

  EmBuiltinType* =
    EmAtom | EmArray | EmBignum | EmBoolVector | EmBool | EmBuffer |
    EmByteCodeFunction | EmCaseTable | EmCharOrString | EmCharTable |
    EmCommand | EmConditionVariable | EmCons | EmCustomVariable |
    EmFixnum | EmFloat | EmFont | EmFrameConfiguration | EmFrameLive | EmFrame |
    EmFunction | EmHashTable | EmIntegerOrMarker | EmInteger | EmKeymap |
    EmKeyword | EmList | EmMarker | EmMutex | EmNlist | EmNumberOrMarker |
    EmNumber | EmOverlay | EmProcess | EmRecord | EmSequence |
    EmStringOrNull | EmString | EmSubr | EmSymbol | EmSyntaxTable |
    EmThread | EmVector | EmWholenum | EmWindowConfiguration |
    EmWindowLive | EmWindow

  OrNil*[T] = object
    value*: Option[T]

  EmTypeError* = object of CatchableError

import
  std/[
    compilesettings,
    macros,
    strutils,
    options,
    macrocache
  ]

import ./emacs_types
export emacs_types

var plugin_is_GPL_compatible {.
  exportc: "plugin_is_GPL_compatible",
  dynlib
.}: cint

proc intern*(env: EmEnv, name: string): EmValue =
  result = env.internImpl(env, name.cstring)

proc eq*(env: EmEnv, a, b: EmValue): bool =
  env.eqImpl(env, a, b)

proc intVal*(env: EmEnv, value: EmValue): int64 =
  env.extractInteger(env, value)

proc floatVal*(env: EmEnv, value: EmValue): cdouble =
  env.extractFloat(env, value)

proc strVal*(env: EmEnv, value: EmValue): string =
  var size: uint = 0
  discard env.copyStringContents(env, value, nil, addr size)
  result = newString(size - 1)
  discard env.copyStringContents(env, value, addr result[0], addr size)

proc symName*(env: EmEnv, value: EmValue): string

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


proc getType*(env: EmEnv, value: EmValue): EmValue =
  env.typeOfImpl(env, value)

proc getTypeName*(env: EmEnv, value: EmValue): string =
  env.symName(env.getType(value))

proc emVal*(env: EmEnv, value: string): EmValue =
  env.makeString(env, value.cstring, value.len.uint)

proc funcallRaw*(env: EmEnv, fun: EmValue, args: seq[EmValue]): EmValue =
  if len(args) == 0:
    result = env.funcallImpl(env, fun, 0, nil)

  else:
    result = env.funcallImpl(env, fun, args.len.uint, unsafeAddr args[0])

proc car*(env: EmEnv, value: EmValue): EmValue =
  assert env.eq(env.getType(value), env.intern("cons"))
  env.funcallRaw(env.intern("car"), @[value])

proc cdr*(env: EmEnv, value: EmValue): EmValue =
  assert env.eq(env.getType(value), env.intern("cons"))
  env.funcallRaw(env.intern("cdr"), @[value])

iterator consItems*(env: EmEnv, cons: EmValue): EmValue =
  var cons = cons
  while env.eq(env.getType(cons), env.intern("cons")):
    yield env.car(cons)
    cons = env.cdr(cons)

  # yield cons

proc formatRaw*(
    env: EmEnv, value: EmValue, fmtCall: string = "prin1-to-string"
  ): string =
  env.strVal(env.funcallRaw(env.intern(fmtCall), @[value]))

proc treeRepr*(env: EmEnv, value: EmValue): string =
  var res = addr result
  proc add(args: varargs[string, `$`]) =
    for arg in args:
      res[].add arg

  proc addi(level: int, args: varargs[string, `$`]) =
    add repeat("  ", level)
    add args


  proc aux(value: EmValue, level: int) =
    let typ = env.getTypeName(value)
    addi level, typ
    case typ:
      of "cons":
        # add "\n"
        # addi level + 1, "car\n"
        # aux(env.funcallRaw(env.intern("car"), @[value]), level + 2)
        # add "\n"
        # addi level + 1, "cdr\n"
        # aux(env.funcallRaw(env.intern("cdr"), @[value]), level + 2)

        for item in env.consItems(value):
          add "\n"
          aux(item, level + 1)


      of "integer":
        add " ", env.intVal(value)

      of "symbol":
        add " '", env.symName(value)

      of "subr":
        add " '", env.formatRaw(value)

      of "string":
        add " \"", env.strVal(value), "\""

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
      let errname = env.strVal(
        env.funcallRaw(env.intern("symbol-name"), @[symOut]))

      var err: ref EmNonlocalSignal


      case errname:
        of "wrong-number-of-arguments":
          err = (ref EmWrongNumberOfArguments)()
          let
            dr = env.cdr(dataOut)
            exp = env.cdr(dr)
            got = env.car(dr)

          err.msg = (
            "Wrong number of arguments for '$#' " &
              "- expected $# but got $#") % [
                name,
                env.formatRaw(exp),
                env.formatRaw(got),
          ]

        else:
          err = (ref EmNonLocalSignal)()
          err.msg = "Call to '$#' exited via signal '$#. Err data $#" % [
            name, errname, env.treeRepr(dataOut)]

      raise err

    else:
      discard

proc funcall*(
    env: EmEnv, fun: string, args: seq[EmValue] = @[],
    checkErr: bool = true
  ): EmValue {.discardable.} =

  return funcall(env, env.intern(fun), fun, args, checkErr = checkErr)

proc symVal*(env: EmEnv, value: string): EmValue =
  env.funcall("symbol-value", @[env.intern(value)])


proc symName*(env: EmEnv, value: EmValue): string =
  env.strVal(env.funcall("symbol-name", @[value]))

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

func getValidationCall(expect: typedesc[string]): string =
  emTypePredicate(EmString)

func getValidationCall(expect: typedesc[bool]): string =
  emTypePredicate(EmBool)

func getValidationCall[I: EmBuiltinType](expect: typedesc[I]): string =
  emTypePredicate(I)

func getValidationCall[T](expect: typedesc[OrNil[T]]): string =
  getValidationCall(T)

proc mismatch[T](env: EmEnv, value: EmValue, expect: T):
    Option[tuple[want, got: string]] =

  let callname = getValidationCall(T)
  if not env.boolVal(env.funcall(callname, @[value])):
    result = some (callname, env.getTypeName(value))

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


proc fromEmacs*[B: EmBuiltinType](
  env: EmEnv, target: var B, value: EmValue, check: bool = true) =
  if not check or expectValid(env, value, target):
    target = B(value)

proc toEmacs*[B: EmBuiltinType](env: EmEnv, value: B): EmValue =
  EmValue(value)

proc fromEmacs*[I: SomeInteger](
    env: EmEnv, target: var I, value: EmValue, check: bool = true) =
  if not check or expectValid(env, value, target):
    target = I(env.intVal(value))

proc fromEmacs*[T](
    env: EmEnv, target: var OrNil[T], value: EmValue, check: bool = true) =
  if env.isNotNil(env, value):
    var tmp: T
    fromEmacs(env, target, value, check)
    target.value = some tmp

proc fromEmacs*(
    env: EmEnv, target: var string, value: EmValue, check: bool = true) =
  if not check or expectValid(env, value, target):
    target = env.strVal(value)

proc fromEmacs*(
    env: EmEnv, target: var bool, value: EmValue, check: bool = true) =
  if not check or expectValid(env, value, target):
    target = env.boolVal(value)

proc toEmacs*(env: EmEnv, value: SomeInteger): EmValue =
  env.makeInteger(env, value.int64)

proc toEmacs*(env: EmEnv, value: string): EmValue =
  env.makeString(env, value.cstring, value.len.uint)

proc toEmacs*(env: EmEnv, value: bool): EmValue =
  if value: env.intern("t") else: env.intern("nil")

proc fromEmacs*[T](env: EmEnv, value: EmValue, check: bool = true): T =
  fromEmacs(env, result, value, check)

proc funcall*[Args](
    env: EmEnv, name: string, args: Args,
    checkErr: bool = true
  ): EmValue {.discardable.} =

  var emArgs: seq[EmValue]
  for name, value in fieldPairs(args):
    emArgs.add env.toEmacs(value)

  return env.funcall(name, emArgs, checkErr = checkErr)

type
  EmProcData* = object
    minArity*: int
    maxArity*: int
    name*: string
    docstring*: string
    impl*: EmProc
    data*: pointer

proc defun*(env: EmEnv, impl: EmProcData) =
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

template defun*(
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

template emError*(env: EmEnv): untyped =
  {.line: instantiationInfo(fullPaths = true).}:
    let ex = getCurrentException()
    let trace = ex.getStackTrace()
    let msg = getCurrentExceptionMsg()
    discard env.funcallRaw(
      env.intern("error"), @[env.emVal(
        "Nim exception: $# - $#\n$#" % [$ex.name, $ex.msg, trace]
      )])

const emcallNames = CacheSeq"emcallNames"

proc emcallImpl*(bindpatt: string, impl: NimNode): NimNode =

  let
    wrapName = genSym(ident = impl.name().strVal() & "Emcall")
    wrapImpl = ident(impl.name().strVal() & "Emprox")
    nargs = ident("nargs")
    args = ident("args")
    env = ident("env")

  emcallNames.add wrapName

  var data: EmProcData
  if 0 < len(emcallNamespace):
    data.name = emcallNamespace & ":"

  data.name.add bindpatt % [impl.name().strVal()]

  data.maxArity = impl.params().len() - 1
  let implName = impl.name().toStrLit()

  var
    implRepack = newStmtList()
    recall = newCall(impl.name())
    count = 0

  let info = newLit(impl.lineInfoObj())

  if impl.params()[1][1].eqIdent("EmEnv"):
    count = -1

  for arg in impl.params()[1..^1]:
    let typ = arg[^2]
    for name in arg[0..^3]:
      var pass = ident(name.strVal())
      let argname = name.toStrLit()
      var init = newCall("default", typ)
      if arg[^1].kind != nnkEmpty:
        init = arg[^1]

      if typ.eqIdent("EmEnv"):
        discard

      else:
        implRepack.add quote do:
          var `pass`: `typ` = `init`
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

macro emcallp*(bindname: static[string], impl: untyped): untyped =
  result = emcallImpl(bindname, impl)

macro emcall*(body: untyped): untyped =
  result = emcallImpl("$1", body)

macro embind*(name: static[string], body: untyped): untyped =
  let env = body.params()[1][0]
  if body.params()[1][1].strVal() != "EmEnv":
    error(
      "Expected `EmEnv` as a first argument for embind target proc",
      body)

  var pass = nnkTupleConstr.newTree()
  for arg in body.params()[2..^1]:
    for name in arg[0..^3]:
      pass.add name

  result = body
  let fcall =
    if len(pass) == 0:
      newCall("funcall", env, newLit(name))

    else:
      newCall("funcall", env, newLit(name), pass)

  result.body = newCall(nnkBracketExpr.newTree(
    ident"fromEmacs",
    body.params()[0],
  ), env, fcall)

type
  EmDefun*[Args, Ret] = object
    name*: string

  EmVar*[T] = object
    name*: string

func getvar*[T](name: string): EmVar[T] = EmVar[T](name: name)

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

macro bindAllEmcall(env: EmEnv): untyped =
  result = newStmtList()
  for name in emcallNames:
    result.add newCall("defun", env, name)

template emInit*(body: untyped): untyped =
  proc init(runtime {.inject.}: ptr EmRuntime): cint {.
    exportc: "emacs_module_init", cdecl, dynlib.} =
    var env {.inject.}: EmEnv = runtime.getEnvironment(runtime)

    assert not isNil(env)
    bindAllEmcall(env)

    try:
      body
      discard env.funcall("provide", @[
        env.intern(querySetting(projectName))])

    except:
      env.emError()

    return 0


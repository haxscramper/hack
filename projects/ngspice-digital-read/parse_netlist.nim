import tables, options, sugar
import os
import parseutils
import re
import strutils
import hmisc/helpers
import sequtils, strformat
import shell
import posix_utils

type
  ExternalSourceError = ref object of CatchableError
  ShellExecError = ref object of ExternalSourceError
    retcode: int ## Return code of failed command
    errmsg: string ## Shell command error message
    shellcmd: string ## Shell command


template tryShellRun(body: untyped): string =
  let shellConf {.gensym.}: set[DebugOutputKind] = {}
  let (res, err, code) = shellVerboseErr shellConf:
    body

  if code != 0:
    raise ShellExecError(
      msg: "Shell execution failed",
      retcode: code,
      errmsg: err,
      shellcmd: astToStr(body)
    )

  res

template withCwd(cwd: string, body: untyped): untyped =
  ## Store current directory, switch to `cwd`, execute body and switch
  ## back
  let nowDir = getCurrentDir()
  setCurrentDir(cwd)

  body

  setCurrentDir(nowDir)


func joinLiteral(msg: string): string =
  msg.split("\n")
    .filterIt(it.find(re"[^\s]") != -1)
    .mapIt(it[it.find(re"""[^\s]""") .. ^1])
    .join(" ")

# DOC use formatting only on literal nodes, pas non-literal as-is
template optFmt(arg: string{lit}): untyped = &arg
proc optFmt(arg: string): string = arg

template longAssertionCheck*(expression: untyped, body: untyped): untyped =
  ## Raise `AssertionError` if `expression` evaluates as false. Body
  ## is a string literal which will be passed as a message. It will be
  ## passed to `&` macro - i.e. variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      let variable = 2
      longAssertionCheck(variable == 3):
        """
        Failed to break math while comparing {variable} to `3`
        """
    except AssertionError:
      test = true

    assert test

  static: assert body is string
  assert expression, joinLiteral(optFmt body)


template longAssertionFail*(body: untyped): untyped =
  ## Raise `AssertionError`. Body is a string literal which will be
  ## passed as a message. It will be passed to `&` macro - i.e.
  ## variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      longAssertionFail:
        "Assertion failed"
    except AssertionError:
      test = true

    assert test

  static: assert body is string
  raise newException(AssertionError, joinLiteral(&body))


template longValueCheck*(expression: untyped, body: untyped): untyped =
  ## Raise `ValueError` if `expression` evaluates as false. Body is a
  ## string literal which will be passed as a message. It will be
  ## passed to `&` macro - i.e. variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      let variable = 2
      longValueCheck(variable == 3):
        """
        Failed to break math while comparing {variable} to `3`
        """
    except ValueError:
      test = true

    assert test

  if not (expression):
    raise newException(ValueError, joinLiteral(&body))


template longValueFail*(body: untyped): untyped =
  ## Raise `ValueError`. Body is a string literal which will be
  ## passed as a message. It will be passed to `&` macro - i.e.
  ## variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      longValueFail:
        "Assertion failed"
    except ValueError:
      test = true

    assert test

  static: assert body is string
  raise newException(ValueError, joinLiteral(&body))


type
  OptStr = Option[string]
  OptInt = Option[int]

type
  NGInstanceKind = enum
    ngdResistor
    ngdDiode

    ngdVoltageSource

    ngdCustom

    ngdVoltageSwitch

  NGInstance* = object
    name*: string ## Alphanumeric name of the instance
    terms*: seq[int] ## Connected terminals
    modelName*: OptStr ## Name of the model
    params*: Table[string, string] ## Additional parameters, not
                                   ## specified in the type.

    case kind*: NGInstanceKind:
      of ngdResistor:
        resistance*: int
      of ngdVoltageSwitch:
        initStage: bool
      of ngdVoltageSource:
        dcValue: float
      of ngdCustom, ngdDiode:
        nil

  NGModel = object
    kind: NGInstanceKind
    name: string
    params: Table[string, string]

  NGNodeKind = enum
    ngnInstance
    ngnControl
    ngnModel

  NGNode = object
    case kind: NGNodeKind:
      of ngnControl:
        beginEnd: (string, string)
      of ngnInstance:
        dev: NGInstance
      of ngnModel:
        model: NGModel

  NGDocument = object
    included: seq[string]
    nodes: seq[NGNode]

func getTerminals(dev: NGInstance): (int, int) =
  ## Returng `N+` and `N-` pins for the node.
  (dev.terms[0], dev.terms[1])

# func `$`(n: NGNNode) = ...

proc parseIntSeq(s: seq[string],
                 inclusive: (int, int) | int,
                 onMissing: string): seq[int] =
  ## Parse sequence of strings into integers and throw exception if
  ## number of items is less than necessary.
  when inclusive is int:
    longAssertionCheck(s.len >= inclusive):
      """Error while parsing integer sequence. Cannot access {inclusive} index
      as sequence has len {s.len}. {onMissing}"""

    try:
      result = @[s[inclusive].parseInt()]
    except:
      longAssertionFail:
        """Failed to parse s[{inclusive}] as integer:
        {getCurrentExceptionMsg()}. {onMissing}"""

  else:
    longAssertionCheck(s.len >= inclusive[1]):
      """Error while parsing integer sequence: sequnce length is too small:
      expected {inclusive[1]}, but found {s.len} """

  longValueCheck(inclusive[1] < s.len):
      "Error while parsing intergers: expected {inclusive[1]} elements but found {s.len}. {onMissing}"

  for i in inclusive[0] .. inclusive[1]:
    result.add s[i].parseInt()

func filterEmpty(s: seq[string]): seq[string] =
  ## Filter out stirng that contains only whitespace characters ([\s])
  s.filterIt(it =~ re"[^\s]")

func joinkv[K, V](t: Table[K, V], eqTok: string = "="): seq[string] =
  ## Join table values as key-value pairs
  collect(newSeq):
    for k, v in t: &"{k}{eqTok}{v}"

func toString(kind: NGInstanceKind): char =
  case kind:
    of ngdDiode: 'D'
    of ngdResistor: 'R'
    of ngdCustom: 'X'
    of ngdVoltageSwitch: 'S'
    of ngdVoltageSource: 'V'

func toInstanceKind(kind: string): NGInstanceKind =
  case kind[0].toLowerAscii():
    of 'd': ngdDiode
    of 'r': ngdResistor
    of 'v': ngdVoltageSource
    of 'x': ngdCustom
    of 's': ngdVoltageSwitch
    else:
      longValueFail: "Unknown instance kind: '{kind[0]}'"

iterator asKVTokens(s: seq[string], eqTok: string = "="): tuple[k, v: string] =
  ## Iterate sequence of key-value tokens
  # TODO handle cases like `k=v`, `k= v` and `k =v` (pair passed as
  # one or two tokens)
  for kIdx in 0 ..< (s.len div 3):
    let idx = kIdx * 3
    longAssertionCheck(s[idx + 1] == eqTok):
      "Invalid equality token: expected {eqTok}, found {s[idx + 1]} at [{idx + 1}]"

    yield (s[idx], s[idx + 2])


proc parseNGModel(config: seq[string]): NGModel =
  let tokens = config.joinw().split(" ").filterEmpty()
  result.name = tokens[1]
  result.kind = tokens[2].toInstanceKind()
  for k, v in tokens[3..^1].asKVTokens():
    result.params[k] = v

template takeItUntil(s: untyped, pr: untyped): untyped =
  var res: seq[type(s[0])]
  for it {.inject.} in s:
    if not pr:
      res.add it
    else:
      break

  res

proc convertUntilEx[T, R](
  s: openarray[T], conv: proc(arg: T): R): tuple[res: seq[R], idx: int] =
  ## Run conversion until exeption is thrown. `counter` will be
  ## assigned to an **index of first failed** item in sequence.
  for idx, it in s:
    try:
      result.res.add conv(it)
    except:
      result.idx = idx
      break

assert @["12", "99", "%%%", "90"].convertUntilEx(
  parseInt) == (@[12, 99], 2)

func splitTupleFirst*[T, R](tupl: (T, R), second: var R): T =
  result = tupl[0]
  second = tupl[1]

func splitTupleSecond*[T, R](tupl: (T, R), second: var T): R =
  result = tupl[1]
  second = tupl[0]


template convertItUntilEx(
  s: untyped, conv: untyped, counter: var int = 0): untyped =
  ## Run conversion until exception is thrown `counter` will be
  ## assigned to an **index of first failed** item in sequence.
  runnableExamples:
    import strutils

    var idx = 0
    assert @["12", "99", "%%%", "90"].convertItUntilEx(
      parseInt(it), idx) == @[12, 99]

    assert idx == 2

  var res: seq[type(
    block:
      var it {.inject.}: type(s[0])
      conv
  )]

  for idx, it {.inject.} in s:
    try:
      res.add conv
    except:
      counter = idx
      break

  res


func getNth[T](s: openarray[T], n: int, onErr: string): T =
  longAssertionCheck(s.len > n):
    onErr

  return s[n]

proc parseNgnNode(lns: seq[(int, string)]): NGNode =
  let lineIdx = lns[0][0]
  let config: seq[string] = lns
    .mapIt(it[1])
    .mapIt(it.startsWith("+").tern(it[1..^1], it))
    .join(" ")
    .split(" ")

  let first = config[0]
  if first.startsWith("."): # Control node
    case first.toLowerAscii():
      of ".model":
        result = NGNode(kind: ngnModel, model: parseNGModel(config))
  else:
    let resDev: NGInstance =
      case first[0].toLowerAscii():
        of 'r':
          NGInstance(
            kind: ngdResistor,
            name: first[1..^1],
            terms: config.parseIntSeq(
              (1, 2), &"Resistor missing connection pin on line {lineIdx}"),
            resistance:
              block:
                let strval = config.getNth(
                  3, &"Resistor missing value on line {lineIdx}")
                90
          )

        of 'x':
          var modelIdx: int = 0
          NGInstance(
            kind: ngdCustom,
            terms: config[1..^1].convertUntilEx(parseInt).splitTupleFirst(modelIdx),
            name: first[1..^1],
            modelName: some(config[modelIdx + 1])
          )
        of 'd':
          NGInstance(
            kind: ngdDiode,
            terms: config.parseIntSeq(
              (1, 2), &"Diode missing conneciton pin on line {lineIdx}"),
            modelName: some(config[3])
          )

        of 'v':
          NGInstance(
            kind: ngdVoltageSource,
            terms: config.parseIntSeq(
              (1, 2), &"Voltage source missing connection pin on line {lineIdx}"))
        else:
          longValueFail:
            "Unknow device type: {first[0]}"

    result = NGNode(kind: ngnInstance, dev: resDev)

proc toString(node: NGNode): string =
  case node.kind:
    of ngnModel:
      let modl = node.model
      result = &".MODEL {modl.name} {modl.kind.toString()}(" &
        &"{modl.params.joinkv().joinw()})"

    of ngnInstance:
      let inst = node.dev
      let dtype =
        case inst.kind:
          of ngdResistor: "R"
          of ngdDiode: "D"
          of ngdCustom: "X"
          of ngdVoltageSource: "V"
          else: ")))"

      result = dtype & inst.name & " " &
        inst.terms.mapIt($it).join(" ") & " " & inst.modelName.get("") & " " &
        (block: collect(newSeq):
           for k, v in inst.params: &"{k}={v}").joinw()


    of ngnControl:
      result = "control"

proc parseNGDoc(path: string): NGDocument =
  let netlist = toSeq(path.lines)
     .enumerate()
     .filterIt(not it[1].startsWith("*"))
     .mapIt((it[0], it[1]))

  result.nodes =
    block:
      var res: seq[NGNode]
      var buf: seq[(int, string)]
      for line in netlist:
        if line[1].startsWith("+"): # Multiline card continuation
          buf.add line
        else:
          if buf.len == 0: # Might be a start of a new mulitline
            buf.add line
          else: # New card
            res.add parseNgnNode(buf)
            buf = @[line]

      res

proc simulate(doc: NGDocument): Table[string, seq[float]] =
  let tmpd = "/tmp/ngpsice-simulation"
  createDir(tmpd)
  let (path, file) = mkstemp(tmpd & "/zzz.net")

  file.writeline("Simulating document")

  for incld in doc.included:
    file.writeline("* included " & incld)
    file.writeline(incld.readFile().string())

  file.writeline(doc.nodes.map(toString).joinl)

  file.close()

  withCwd(tmpd):
    discard tryShellRun:
      ngspice -b "saa.net"



proc main(): void =
  var doc = parseNGDoc("key-grid.net")
  doc.included = @["on-off-switch.net", "io-pin.net"]

  for node in doc.nodes:
    var tmp = node

    if tmp.kind == ngnInstance and
       tmp.dev.kind == ngdCustom:
      case tmp.dev.modelName.get():
        of "on-off-switch": tmp.dev.params["state"] = "0"
        of "io-pin":
          if node.dev.name.parseInt() <= 3:
            tmp.dev.params["state"] = "1"
          else:
            tmp.dev.params["state"] = "0"

  let vectors = doc.simulate()

template getCEx(t: untyped): untyped =
  cast[t](getCurrentException())

template prettyStackTrace(body: untyped): untyped =
  template pprintStackTrace(): untyped =
    let e = getCurrentException()
    let choosenim = getHomeDir() & ".choosenim"

    for tr in e.getStackTraceEntries():
      let filename: string = $tr.filename

      if not filename.startsWith(choosenim):
        let (_, name) = filename.splitPath()
        echo name, " ", tr.line, " \e[31m", tr.procname, "\e[0m"

    let idx = e.msg.find('(')
    echo e.msg[(if idx > 0: idx else: 0)..^1]

  try:
    body
  except ShellExecError:
    pprintStackTrace()

    let e = getCEx(ShellExecError)
    echo "---"
    echo "code: ", e.retcode
    echo "msg: ", e.errmsg

prettyStackTrace:
  main()

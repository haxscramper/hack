import tables, options, sugar
import os
import parseutils
import re
import strutils
import hmisc/helpers
import sequtils, strformat

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

    ngdDcVoltage

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
      of ngdDcVoltage:
        dcValue: float
      of ngdCustom, ngdDiode:
        nil

  NGModel = object
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


  # if s.len < minNumber:
  #   raise newException(
  #     &"Error while parsing intergers: expected {minNumber} but found {s.len}. {onMissing}")

func parseNGModel(config: seq[string]): NGModel =
  discard

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
              (1, 2), &"Resistor missing conneciton pin on line {lineIdx}")
          )

        else:
          longValueFail:
            "Unknow device type: {first[0]}"
    result = NGNode(kind: ngnInstance, dev: resDev)

proc toString(inst: NGInstance): string =
  let dtype =
    case inst.kind:
      of ngdResistor: "R"
      of ngdDiode: "D"
      of ngdCustom: "X"
      of ngdDcVoltage: "V"
      else: ")))"

  result = dtype & inst.name & " " &
    inst.terms.mapIt($it).join(" ") & " " & inst.modelName.get("") & " " &
    (block: collect(newSeq):
       for k, v in inst.params: &"{k}={v}").joinw()

proc toString(node: NGNode): string =
  case node.kind:
    of ngnModel:
      ".MODEL"

    of ngnInstance:
      toString(node.dev)

    of ngnControl:
      "control"


proc main(): void =
  let netlist = toSeq("key-grid.net".lines)
    .enumerate()
    .filterIt(not it[1].startsWith("*"))
    .mapIt((it[0], it[1]))

  let instances =
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
            buf = @[]

      res

  for node in instances:
    var tmp = node

    if tmp.kind == ngnInstance and
       tmp.dev.kind == ngdCustom and
       tmp.dev.modelName == "on-off-switch":

      tmp.dev.params["state"] = "0"

    echo tmp.toString()

template prettyStackTrace(body: untyped): untyped =
  try:
    body
  except:
    let e = getCurrentException()
    let choosenim = getHomeDir() & ".choosenim"

    for tr in e.getStackTraceEntries():
      let filename: string = $tr.filename

      if not filename.startsWith(choosenim):
        let (_, name) = filename.splitPath()
        echo name, " ", tr.line, " \e[31m", tr.procname, "\e[0m"

    let idx = e.msg.find('(')
    echo e.msg[(if idx > 0: idx else: 0)..^1]

prettyStackTrace:
  main()

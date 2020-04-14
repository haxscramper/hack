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
  assert expression, joinLiteral(&body)


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
  NGDeviceKind = enum
    ngdResistor
    ngdDcVoltage

    ngdCustom

    ngdVoltageSwitch

  NGDevice* = object
    name*: string ## Alphanumeric name of the instance
    terms*: seq[int] ## Connected terminals
    modelName*: OptStr ## Name of the model
    params*: Table[string, string] ## Additional parameters, not
                                   ## specified in the type.

    case kind*: NGDeviceKind:
      of ngdResistor:
        resistance*: int
      of ngdVoltageSwitch:
        initStage: bool
      of ngdDcVoltage:
        dcValue: float
      of ngdCustom:
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
        dev: NGDevice
      of ngnModel:
        model: NGModel

func getTerminals(dev: NGDevice): (int, int) =
  ## Returng `N+` and `N-` pins for the node.
  (dev.terms[0], dev.terms[1])

# func `$`(n: NGNNode) = ...

proc parseIntSeq(s: seq[string],
                 inclusive: (int, int) | int,
                 onMissing: string): seq[int] =
  ## Parse sequence of strings into integers and throw exception if
  ## number of items is less than necessary.
  when inclusive is int:
    longAssertionCheck(s.len < inclusive):
      """Error while parsing integer sequence. Cannot access {inclusive} index
      as sequence has len {s.len}. {onMissing}"""

    try:
      result = @[s[inclusive].parseInt()]
    except:
      longAssertionFail:
        """Failed to parse s[{inclusive}] as integer:
        {getCurrentExceptionMsg()}. {onMissing}"""

  else:
    longAssertionCheck(s.len < inclusive[1]):
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

proc convertUntilEx[T, R](s: openarray[T], conv: proc(arg: T): R): tuple[res: seq[R], idx: int] =
  ## Run conversion until exeption is thrown. `counter` will be
  ## assigned to an **index of first failed** item in sequence.
  runnableExamples:
    import strutils

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


template convertItUntilEx(s: untyped, conv: untyped, counter: var int = 0): untyped =
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

proc parseNgnNode(lns: seq[string]): NGNode =
  let config: seq[string] = lns
    .mapIt(it.startsWith("+").tern(it[1..^1], it))
    .join(" ")
    .split(" ")

  let first = config[0]
  if first.startsWith("."): # Control node
    case first.toLowerAscii():
      of ".model":
        result = NGNode(kind: ngnModel, model: parseNGModel(config))
  else:
    let resDev: NGDevice =
      case first[0].toLowerAscii():
        of 'r': NGDevice(
          kind: ngdResistor,
          name: first[1..^1],
          terms: config.parseIntSeq((1, 2), "Resistor missing connection pin"),
          resistance: config.parseIntSeq(3, "Resistor missing value")[0]
        )

        of 'x':
          var modelIdx: int = 0
          NGDevice(
            kind: ngdCustom,
            terms: config[1..^1].convertUntilEx(parseInt).splitTupleFirst(modelIdx),
            name: first[1..^1],
            modelName: some(config[modelIdx + 1])
          )

        else:
          longValueFail:
            "Unknow device type: {first[0]}"




proc main(): void =
  let netlist = collect(newSeq):
    for line in "key-grid.net".lines():
      if not (line.startsWith("*")):
        line


  let devices =
    block:
      var res: seq[NGNode]
      var buf: seq[string]
      for idx, line in netlist:
        if line.startsWith("+"): # Multiline card continuation
          buf.add line
        else:
          if buf.len == 0: # Might be a start of a new mulitline
            buf.add line
          else: # New card
            res.add parseNgnNode(buf)
            buf = @[]

      res

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

prettyStackTrace:
  main()

import tables, options, sugar
import parseutils
import re
import strutils
import hmisc/helpers
import sequtils, strformat

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
    eName*: string ## Alphanumeric name of the instance
    terms*: seq[int] ## Connected terminals
    nodelName*: OptStr ## Name of the model
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

func joinLiteral(msg: string): string =
  msg.split("\n")
    .filterIt(it.len > 0)
    .mapIt(it[it.find(re"""[^\s]""") .. ^1])
    .join(" ")

proc parseIntSeq(s: seq[string],
                 inclusive: (int, int) | int,
                 onMissing: string): seq[int] =
  ## Parse sequence of strings into integers and throw exception if
  ## number of items is less than necessary.
  when inclusive is int:
    if s.len < inclusive:
      raise newException(ValueError, joinLiteral &"""Error while parsing integer sequence. Cannot access {inclusive}
      index as sequence has len {s.len}. {onMissing}""")
    else:
      try:
        result = @[s[inclusive].parseInt()]
      except:
         raise newException(
         ValueError, &"""Failed to parse s[{inclusive}] as integer: {getCurrentExceptionMsg()}.
         {onMissing}""")

  else:
    if s.len < inclusive[1]:
      raise newException(
        ValueError, joinLiteral &"""
        Error while parsing integer sequence: sequnce length
        is too small: expected {inclusive[1]}, but found {s.len}
        """)


  # if s.len < minNumber:
  #   raise newException(
  #     &"Error while parsing intergers: expected {minNumber} but found {s.len}. {onMissing}")

func parseNGModel(config: seq[string]): NGModel =
  discard

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
      case first[0]:
        of 'R': NGDevice(
          kind: ngdResistor,
          terms: config.parseIntSeq((1, 2), "Resistor missing connection pin"),
          resistance: config.parseIntSeq(3, "Resistor missing value")[0]
        )
        else:
          raise newException(
            ValueError, &"""Invald name: {first[0]}""")





when isMainModule:
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

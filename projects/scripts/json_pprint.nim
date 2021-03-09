#!/usr/bin/env nimcr
#nimcr-args c --verbosity:0 --hints:off

import json, shell, terminal
import tables, sequtils
import math

type
  PJsonKind = enum
    pjkArray
    pjkObject
    pjkString

  PrintableJson = object
    case kind*: PJsonKind
    of pjkArray:
      elems: seq[PrintableJson]
    of pjkString:
      str: string
      original: JsonNodeKind
    of pjkObject:
      members: OrderedTable[string, PrintableJson]

func len(node: PrintableJson): int =
  ## Print len of the json node as if it was printed into single line
  case node.kind:
    of pjkString:
      case node.original:
        of JNull: 4
        of JString: node.str.len + 2
        of JFloat, JBool, JInt: node.str.len
        else: node.str.len
    of pjkArray:
      const
        wrapLen = "[  ]".len()
        spacLen = ", ".len()

      node.elems.mapIt(it.len).sum() +
      (node.elems.len() - 1) * spacLen +
      wrapLen
    of pjkObject:
      const
        wrapLen = "{  }".len()
        delmLen = ": ".len()
        spacLen = ", ".len()

      var res = wrapLen
      for key, val in node.members:
        res += key.len() + 2
        res += val.len() + delmLen + spacLen

      res -= spacLen
      res


func toPrintableJson(node: JsonNode): PrintableJson =
  case node.kind:
    of JArray:
      PrintableJson(
        kind: pjkArray,
        elems: node.getElems().map(toPrintableJson)
      )
    of JInt, JFloat, JString, JBool, JNull:
      PrintableJson(
        kind: pjkString,
        str: $node,
        original: node.kind
      )

    of JObject:
      PrintableJson(
        kind: pjkObject,
        members: toSeq(pairs(node.getFields())).mapIt(
          (it[0], it[1].toPrintableJson())
        ).toOrderedTable()
      )

type
  ColoredString = object
    str: string
    col: ForegroundColor

let termW = terminalWidth()

proc print(str: ColoredString): void =
  stdout.styledWrite(str.str, str.col)

proc print(node: PrintableJson, ind: int = 0): void =
  # case node.kind:
  #   of pjkArray:
  #     case node.elems[0].kind:
  #       of pjkString:
  #         let perLine = toInt(node.len() / termW)

  echo "test"

proc die(msg: string, msg2 = "") {.noreturn.} =
  echo msg
  echo msg2
  quit 1

let (res, err, code) = shellVerboseErr {dokCommand}:
  pandoc -t json test.md

# IDEA write macro to automate defensive programming checks: error
# codes, assertions (with adequate error messages) and `die` calls.
if code != 0:
  die "Error while converting to json", err

# proc pprint(node: JsonNode)

parseJson(res).toPrintableJson().print()

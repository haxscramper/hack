import strutils, sequtils, os, parsecsv, streams, future

type
  TestVar* {.exportcpp.} = object
    case kind: bool
    of true:
      a*: int
    of false:
      b*: int

proc testVar*(v: TestVar): void {.exportcpp.} =
  if v.kind:
    echo v.a
  else:
    echo v.b

proc parseCSVListLine*(str: cstring): cstringArray {.exportcpp.} =
  echo "Parsing csv"
  var ss = newStringStream($str)
  var p: CsvParser
  p.open(ss, "parseCSVListLine()")
  discard p.readRow()
  return allocCstringArray(lc[ col.strip() | (col <- items(p.row)), string])


proc parseCSVListLine*(str: cstring, n: int): cstringArray {.exportcpp.} =
  echo "Parsing csv with integer"
  return parseCSVListLine(str)

proc acceptsString*(arg: string): void {.exportcpp.} =
  echo arg, " - -"

proc acceptsStrSeq*(arg: seq[seq[string]]): void {.exportcpp.} =
  echo arg.len

type
  Exported* {. header: "clib.hpp", importcpp: "Exported", .} = object
    val1: cfloat # Impotted type's fields have to be specified
                 # explicitly

proc importedMethod(this: Exported): cfloat {.importcpp: "getVal".}

proc acceptsExported(arg: Exported): void {.exportcpp.} =
  echo arg.importedMethod()



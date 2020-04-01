import strutils, sequtils, os, parsecsv, streams, future

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

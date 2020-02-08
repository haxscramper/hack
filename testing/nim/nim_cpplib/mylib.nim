import strutils, sequtils, os, parsecsv, streams, future

proc parseCSVListLine*(str: cstring): cstringArray {.exportc.} =
  var ss = newStringStream($str)
  var p: CsvParser
  p.open(ss, "parseCSVListLine()")
  discard p.readRow()
  return allocCstringArray(lc[ col.strip() | (col <- items(p.row)), string])

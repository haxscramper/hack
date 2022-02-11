import std/parsecfg
import std/[strutils, streams]

var f = newStringStream("""
val=int
val=$config/test
nimout.gcc = 12
""")
var p: CfgParser
open(p, f, "?")
while true:
  var e = next(p)
  case e.kind
  of cfgEof: break
  of cfgSectionStart:   ## a `[section]` has been parsed
    echo "new section: " & e.section
  of cfgKeyValuePair:
    echo "key-value-pair: " & e.key & ": " & e.value
  of cfgOption:
    echo "command: " & e.key & ": " & e.value
  of cfgError:
    echo e.msg
close(p)

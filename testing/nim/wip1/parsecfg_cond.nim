import os, parsecfg, strutils, streams

let str = """
@if openmp or mkl:
  stackTrace:off
  @if macosx: # Default compiler on Mac is clang without OpenMP and gcc is an alias to clang.
              # Use Homebrew GCC instead for OpenMP support. GCC (v7), must be properly linked via `brew link gcc`
    cc:"gcc"
    gcc.exe:"/usr/local/bin/gcc-7"
    gcc.linkerexe:"/usr/local/bin/gcc-7"
  @end
@end
"""

var f = newStringStream(str)
var p: CfgParser
open(p, f, "str")

while true:
  var e = next(p)
  case e.kind
    of cfgEof:
      break

    of cfgSectionStart:   ## a ``[section]`` has been parsed
      echo("new section: " & e.section)

    of cfgKeyValuePair:
      echo("key-value-pair: " & e.key & ": " & e.value)

    of cfgOption:
      echo("command: " & e.key & ": " & e.value)

    of cfgError:
      echo(e.msg)
close(p)

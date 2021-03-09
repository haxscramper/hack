import parseopt
import os
import colechopkg/lib

if paramCount() == 1 and paramStr(1) in @[
  "help", "-h", "--help", "-?", "-help"]:
  ceUserLog0("""
Usage: <command> <lhs> <rhs>
Wrap each line from stdin into [lhs] [rhs]""")
  quit(0)

if paramCount() != 2:
  ceUserError0("""
Two command line arguments are required: left and right
wrapping strings""")
else:
  let left = paramStr(1)
  let right = paramStr(2)
  for line in stdin.lines():
    echo left, line, right

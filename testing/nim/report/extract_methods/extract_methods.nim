import pegs
import macros, ../../lib/argparse
import options
import os
import re
import strutils
import strformat, sequtils


var ignoredMethods: seq[string]
var linelimit = 10000

func getbalance(str: string): int = str.count('{') - str.count('}')

proc splitClass(str: string): seq[tuple[
  name, body: string]] =
  var parenBalance = 0

  var currentName: string
  var currentBody: seq[string]
  var methStarted: bool
  var className: string
  var inComment: bool

  for line in str.split('\n'):
    if line =~ re".*?class (\w+) .*?":
      className = matches[0]
    elif line =~ re"import.*" or line =~ re"class.*":
      discard

    elif not methstarted and line =~ re".*?(\w+)\s*\(.*\).*\{":
      currentName = matches[0]
      methstarted = true

    if methstarted:
      parenBalance += line.getbalance()

      let notrail =
        if line =~ re"(.*?)\/\/.*": matches[0]
        else: line

      let noComm =
        if inComment:
          if notrail =~ re".*?\*\/(.*)":
            inComment = false
            matches[0]
          else:
            ""
        else:
          if notrail =~ re"(.*?)\/\*.*?\*\/(.*)":
            matches[0] & " " & matches[1]
          elif notrail =~ re"(.*?)\/\*.*":
            inComment = true
            matches[0]
          else:
            notrail


      if nocomm.len < linelimit:
        currentBody.add(noComm)

      if (parenBalance == 0):
        methstarted = false
        if currentName notin ignoredMethods and
           (className & "." & currentName) notin ignoredMethods and
           currentbody.len > 0:
          result.add((
            className & "." & currentName,
            currentBody[1..^1].join("\n")))
        currentBody = @[]


parseArgs:
  opt:
    name: "input-file"
    opt: ["--input-file", "+takes_value"]
    help: "Input file name"
  opt:
    name: "output-dir"
    opt: ["--output-dir", "+takes_value"]
    help: "Output directory"
  opt:
    name: "verbose"
    opt: ["--verbose", "-v"]
    help: "verbose"
  opt:
    name: "ignored-methods"
    opt: ["--ignored-methods", "+takes_value"]
    help: "Comma-separated list of method names to ignore"
  opt:
    name: "maxlen"
    opt: ["--maxlen", "+takes_value"]
    help: "Max threshol for line length. Lines longer will be ignored"
    parseto: int
  opt:
    name: "debug"
    opt: ["--debug"]
    help: "Dump debug information"

if "debug".kp:
  if "maxlen".kp: echo "maxlen: ", "maxlen".k.toint
  if "ignored-methods".kp: echo "ignored: ",
     "ignored-methods".k.tostr.split(",")

if "ignored-methods".kp:
  ignoredMethods = "ignored-methods".k.tostr.split(",")

if "maxlen".kp:
  lineLimit = "maxlen".k.toint

proc allof[T](arr: openarray[T], item: T): bool =
  result = true
  for it in arr:
    result = result and it == item

proc allof[T](arr: openarray[T], st: set[T]): bool =
  result = true
  for it in arr:
    result = result and (it in st)

if "input-file".kp and "output-dir".kp:
  let infile = "input-file".k.tostr
  let outdir = "output-dir".k.tostr
  createDir(outdir)
  for meth in infile.readFile().string.splitClass():
    let outPath = joinpath(outdir, meth.name & ".tmp.c")

    if "verbose".kp:
      echo outPath

    if meth.body.len > 0 and not meth.body.allof({' ', '\n'}):
      outpath.writefile("{\n" & meth.body & "\n}")

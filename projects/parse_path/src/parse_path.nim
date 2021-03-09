# version 1.3
import colechopkg/lib
import hargparse
import hmisc/helpers

import macros
import os
import strutils

# TODO Add suport for reading from stdin
# TODO wrap all actions in proc so they can be used as a functions

parseArgs:
  opt:
    name: "last-suffix"
    opt: ["--last-suffix"]
    help: "Print last suffix for filename"
  opt:
    name: "all-suffixes"
    opt: ["--all-suffixes", "--all-suffices"]
    help: "Print all suffixes for file"
  opt:
    name: "dirname"
    opt: ["--dirname"]
    help: "Print directory name"
  opt:
    name: "name"
    opt: ["--name"]
    help: "Print file name without extension"
  opt:
    name: "del-suffix"
    opt: ["--del-suffix", "+takes_arg"]
    help: """Remove last N suffices from argument (use when you need to get
basename for file that contains dot in the name)"""
    parseto: int
  opt:
    name: "basename"
    opt: ["--basename"]
    help: "Print file name with extension"
  opt:
    name: "no-local"
    opt: ["--no-local"]
    help: "Remove ./ prefix if present before doing anything else"
  opt:
    name: "all"
    opt: ["--all"]
    help: "Pring everything back"

macro compareAssertion(body: untyped): untyped =
  # TODO provide line number for exception
  # defer:
  #   echo "macro result"
  #   echo result.toStrLit()

  result = newStmtList()
  result.add quote do:
    # TODO generate randomname for variable
    var assertionHasErrors {.inject.} = false

  for child in body:
    if not (child.kind == nnkInfix and child[0].strVal == "=="):
      raise newException(ValueError, "each element has to be comparison")
    else:
      let lhs = child[1]
      let rhs = child[2]
      let compareExpr = $child.toStrlit()
      result.add quote do:
        block:
          let lhsRes = `lhs`
          let rhsRes = `rhs`
          if lhsRes != rhsRes:
            echo `compareExpr`, " evaluated as false"
            echo "left result: ", lhsRes
            echo "right result: ", rhsRes
            assertionHasErrors = true

  result.add quote do:
    if assertionHasErrors:
      raise newException(AssertionError, "one of comparisons failed")
    else:
      echo "all comparisons ok"



type
  Split = object
    dirs, prefs: seq[string]

proc pathSplit(inPath: string): Split =
  var path = inPath
  let localPrefix =
    if "no-local".kp():
      if path.startsWith("./"): path = path[2..^1]
      ""
    else:
      ""

  let (dir, name, ext) = path.splitfile()
  let dotsplit = (name & ext).split(".")
  let slashsplit = dir.split("/")

  return Split(
    dirs: slashsplit,
    prefs: dotsplit
  )

proc getLastExt(path: string): string =
  pathSplit(path).prefs[^1]

proc delSuffix(path: string, toDel: int): string =
  pathSplit(path).prefs[0..^(toDel + 1)].join(".")

proc getAllSuffices(path: string): string =
  pathSplit(path).prefs[1..^1].join(".")

proc getDirname(path: string): string =
  pathSplit(path).dirs.join("/")

proc getName(path: string): string =
  pathSplit(path).prefs[0]

proc getAll(path: string): string = path

const testing = true
when testing:
  compareAssertion:
    getlastext("a.a.a.a.b") == "b"
    getlastext("a") == "a"
    getall("/s/b/c") == "/s/b/c"
    getname("/a/b.c") == "b"
    getdirname("/b/b/c") == "/b/b"
    delsuffix("a.b.c.d", 1) == "a.b.c"
    delsuffix("Avenza - Could Be Worse-eaCdfcpla4c.mp3", 1) ==
      "Avenza - Could Be Worse-eaCdfcpla4c"

  quit(1)

if hasErrors:
  quit(1)
elif argParsed.len != 1:
  ceUserError0("need exactly one file name")
  quit(1)
else:
  var path = argParsed[0]

  # var (dir, name, ext) = path.splitFile()
  # let split = (name & ext).split('.')

  # name = suffices[0]
  # let suffices = suffices[1..^1]
  # let basename = ( @[name] & suffices ).join(".")

  if "last-suffix".kp:
    echo getLastExt(path)
  if "del-suffix".kp:
    echo delsuffix(path, "del-suffix".k.toInt())
  if "all-suffixes".kp:
    echo getAllSuffices(path)
  if "dirname".kp:
    echo getDirname(path)
  if "name".kp:
    echo getName(path)
  if "basename".kp:
    echo delSuffix(path, 0)
  if "all".kp:
    echo getall(path)

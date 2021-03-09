import hcrModule
import os
import strformat
import osproc
import strutils

import hotcodereloading

import terminal

proc eColor(arg: string, color: ForegroundColor) =
  setForegroundColor(color, true)
  echo arg
  setForegroundColor(fgDefault)

proc eRed(args: varargs[string, `$`]) = eColor(args.join(" "), fgRed)
proc eGreen(args: varargs[string, `$`]) = eColor(args.join(" "), fgGreen)
proc eBlue(args: varargs[string, `$`]) = eColor(args.join(" "), fgBlue)

let moduleFile = "hcrModule.nim"
let mainFile = "hcrMain.nim"

proc changeModule(idx: string) =
  egreen &"writing new module file for version {idx}"
  moduleFile.writeFile(&"""
static:
  echo "compiling module for {idx} iteration"

proc getIteration*(): string = "{idx}"
""")

proc recompile() =
  egreen "recompiling code"
  let command = &"nim c -o:{mainFile}.tmp {mainFile}"
  eblue &"command: {command}"
  let (output, code) = execCmdEx(command)
  if code != 0:
    ered "compilation failed"
    echo output
  else:
    egreen "compilation ok"

changeModule("starting")

for i in 0..3:
  changeModule($i)
  recompile()

  eblue &"before core reload: {getIteration()}"
  ered "running code reload"
  performCodeReload()
  eblue &"after core reload: {getIteration()}"

  sleep 100
  echo ""

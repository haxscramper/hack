import moustachu
import parsetoml
import sequtils
import strformat
import strutils
import math
import algorithm
import os
import asyncdispatch
import osproc
import posix
import selectors
import asyncfile
import options
import times


import hmisc/termformat
import hmisc/helpers
import hmisc/uprompt
import hmisc/strparser
import hmisc/fsmonitor

import create_script
import hargparse

import colechopkg/lib
import colechopkg/types

import macros
import logging

var utilityPid: Pid

import cli_parser
import file_watcher
import build_parser


proc startKeyListener() {.async.} =
  # TODO wip. I need to somehow read input from stdin even without RET
  # being pressed. This solution still blocks rebuilding and I don't
  # know how to solve this.
  ceinfo "Started key listener", 2
  var inSelector = newSelector[int]()
  inSelector.registerHandle(0, {Event.Read}, -1)

  var stdinAsync = newAsyncFile(AsyncFd(0))

  while true:
    ceLog "Waiting for events on stdin"
    let inChar = await stdinAsync.read(1)

proc select(
  buildOpts: seq[BuildOption],
  selected: Option[tuple[uname, ext: string]] =
    none((string,string))):
    BuildOption =

  let selected =
    if buildOpts.len > 1:
      buildOpts
      .mapIt(it.name & ". Build command: '" & it.buildCommand & "'")
      .toSeq()
      .enumerate()
      .mapIt(($(it[0]) , it[1]))
      .printTwoColumns()

      getInt(
        "Enter selection",
        valRange = (0, buildOpts.len - 1))
    else:
      0

  result = buildOpts[selected]



#~#==== File change loops


proc runDev(opts: CmdParsed) =
  let inputFile = opts.targetFile
  let fileExt = inputFile.getLastExt()
  if not fileExists(inputFile):
    if promptYN("File is missing. Create files?"):
      let templ = templateFromExt(fileExt)
      inputFile.writeFile(templ.body)

      inputFile.addUExec()

      ceUserInfo2(fmt"Created file {inputFile} and added execution permission")

    else:
      ceUserWarn("No file created, exiting development")
      return


  let buildOpts = getBuildOpts(inputFile)
  if buildOpts.len == 0:
    ceUserError0(fmt"""
No build opts found for '{inputFile}'. Make sure
that *at least one* build command with extension '{fileExt}'
or with no extension at all
exists in configuraion file and can be parsed without errors
(see previous messages for error clarifications).
""")
    quit (1)


  asyncCheck startBuilder(
    buildOpts.select(),
    opts.waitUtil,
    opts.maxRepeat)

  runForever()

proc listenChanges(files: seq[string]) {.async.} =
  ## Listen for changes in files (and associated with them) and print
  ## to stdout
  # TODO monitor directory and add new files for watching
  var monitor = newMonitor()
  for file in files:
    monitor.add(file, {MonitorModify})

  while true:
    let events: seq[MonitorEvent] = await monitor.read()
    for ev in events:
      case ev.kind:
        of MonitorModify: echo ev.name
        else: discard

proc startWatch(files: seq[string]) =
  asyncCheck listenChanges(files)
  runForever()



#~#==== Test script generations
proc toFuncName(str: string): string =
  str.multiReplace([
      (".", "_"),
      ("-", "_")
  ])

proc getBuilderFunction(selected: BuildOption, inputFile: string): string =
  result = fmt"""
function build_{inputFile.toFuncName()} {{
  uname="{selected.uname}"
  fsm-build build --uname:"$uname"

  if [[ "$?" != "0" ]]; then
    colecho -e "Build failed"
    fail_state="1"
    return 1
  else
    fail_state="0"
  fi
}}
"""

proc getRunnerFunction(selected: BuildOption, inputFile: string): string =
  result = fmt"""
function run_{inputFile.toFuncName()} {{
  build_{inputFile.toFuncName()}
  if [[ "$fail_state" != "0" ]]; then
    return $fail_state
  fi

  uname="{selected.uname}"
  $msg -i:3 "Running $uname"

  {selected.runCommand}
  echo
}}
"""

proc generateChangeReader(buildFiles: seq[string]): string =
  let funcCalls = buildFiles.mapIt(
    fmt"""
    if [[ "$file" == "{it}" ]]; then
      run_{it.toFuncName()}
    fi""").join("\n")

  let preCall = buildFiles.mapIt("run_" & it.toFuncName()).join("\n")

  fmt"""
{preCall}

fsm-build watch "[$watched]" |
  while read -r file; do
{funcCalls}
  done
"""

proc generateTestScript(inputFile:string): string =
  let buildOpts = getBuildOpts(inputFile)
  let selected = buildOpts.select()
  # TODO use script templates for file header
  let templ: string = fmt"""
#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
IFS=","
watched="'$*'"
IFS=" "
fail_state="0"
msg="colecho -b"

$msg "Starting test script"

{getBuilderFunction(selected, inputFile)}
{getRunnerFunction(selected, inputFile)}
{generateChangeReader(@[inputFile])}

"""
  return templ


when isMainModule:
  let parseResults = parseCMDLine()
  case parseResults.kind:
    of doRunDev:
      runDev(parseResults)
    of watchChanges:
      startWatch(parseResults.targetFiles)
    of runSingleBuild:
      discard
    of createFiles:
      parseResults.testShName.writeFile(
        generateTestScript(
          parseResults.fileToCreate))
    of defaultMode:
      discard
    of debugDump:
      dumpDebugInfo()

# IDEA hide cursor in terminal on start and show it when program is
# terminated. Requires to catch shortcuts.

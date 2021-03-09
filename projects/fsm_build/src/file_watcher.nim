import build_parser

import options
import os
import sequtils
import strutils
import strformat
import posix

import asyncfile
import asyncdispatch

import algorithm

import hmisc/fsmonitor
import hmisc/helpers
import colechopkg/lib
import times

import execution

var utility_pid: Pid

proc startBuilder*(
  selected: BuildOption,
  waitUtil: bool = false,
  maxRepeat: Opt[int]) {.async.} =
    var monitor = newMonitor()
    ceUserInfo0("List of watched files:")

    for file in selected.files.sorted.deduplicate(true):
      ceUserLog0(file)
      monitor.add(file, {MonitorModify})

    proc doBuild(cmd: string): bool =
      printSeparator("upper")
      #echo  "Building using:", cmd
      result = execShellCmd("/bin/bash -c \"$#\"" % cmd) == 0
      printSeparator("lower")

    proc doRunBuild(): bool =
      if doBuild(selected.buildCommand):
        ceUserInfo0("Build succeded, running")
        if selected.runCommand != "":
          let child_pid = startUtility(selected.runCommand)
        true
      else:
        ceUserWarn("Build failed")
        false





    let cooldownSec: float = 1
    var lastBuild: float = epochTime()

    ceUserInfo0("Initial build")
    discard doRunBuild()



    var lastBuildState: bool = true

    # TODO Print message about waiting for changes in according places
    while true:
      let events: seq[MonitorEvent] = await monitor.read()
      # TODO analyze list of changes and reduce list of events to
      # avoid repetitive rebuilds or wait at least certain amount of
      # time between rebuilds ignoring all changes to ignore events
      # that happened in sequence but in different event batches
      if events.mapIt(it.kind == MonitorModify).foldl(a or b) and
         lastBuild + cooldownSec < epochTime():
        block: # Kill utility if needed
          if utilityPid != 0 and waitUtil:
            var statVal: cint
            let res = waitpid(utilityPid, statVal, 0)
            ceUserInfo0("Run finished")
          elif utilityPid != 0 and not waitUtil:
            var null: cint = 0
            let testRes = waitpid(utilityPid, null, WNOHANG)
            if testRes == 0:
              ceUserInfo0("New change, killing utility ...")
              let res = kill(utilityPid, SIGTERM)
              ceUserLog0("Done")
            elif lastBuildState:
              ceUserInfo0("Run finished")

        block: # Rebuild and restart
          lastBuild = epochTime()
          ceUserInfo0 "Rebuilding ..."

          lastBuildState = doRunBuild()
          if not lastBuildState:
            ceUserInfo0 "Waiting for new changes"

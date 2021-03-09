import posix
import colechopkg/lib
import os
import strutils
import strformat
import sequtils

proc startUtility*(command: string): Pid =
  let pid: Pid = fork()
  if pid < 0:
    ceUserError0 "Cannot fork"
    return pid
  elif pid > 0:
    return pid
  else:
    # TODO Add support for runing without use of bash, but by using
    # exec. Add different build command options for build command
    # templates (bash_build and exec_build).
    printSeparator("upper")
    let res = execShellCmd("/bin/bash -c \"$#\"" % command)
    # TODO catch return value and print all errors
    printSeparator("lower")
    quit(0)

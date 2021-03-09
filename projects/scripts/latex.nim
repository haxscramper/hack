#!/usr/bin/env nimcr
#nimcr-args c --verbosity:0 --hints:off

import hmisc/hshell
import shell

let command = 
  "m4 -I /home/test/.config/hax-software/m4circuit pgf.m4 input.tmp.m4 | dpic -g"


echo runShell(command).outstr.len

var tmp = 0
for line in iterstdout(command):
  tmp += line.len + 1

echo tmp

setShellDebugConfig({})

let (res, _) = execShell(command)
echo res.len

let (res2, _) = shellVerbose:
  pipe:
    m4 -I "/home/test/.config/hax-software/m4circuit" pgf.m4 input.tmp.m4 
    dpic -g

echo res2.len

#!/usr/bin/env nimcr
#nimcr-args c --verbosity:0 --hints:off

import osproc, streams, strutils, sequtils
import re, strformat
import parseutils

proc runShell(command: string): tuple[
  outstr, outerr: string,
  outcode: int]
  =
  let pid = startProcess(command, options = {poEvalCommand})

  let outStream = pid.outputStream
  var line = ""

  while pid.running:
    try:
      let streamRes = outStream.readLine(line)
      if streamRes:
        result.outstr &= line & "\n"
    except IOError, OSError:
      assert outStream.isNil
      echo "process died"

  result.outcode = pid.peekExitCode()
  if result.outcode != 0:
    result.outerr = pid.errorStream.readAll()

echo runShell("cp /sssssssssssss /s1111111111111")


doAssert runShell("ls / | head -n5 ").
         outstr.
         split("\n").
         filterIt(it.len != 0).
         len == 5


iterator iterstdout(command: string): string =
  let pid = startProcess(command, options = {poEvalCommand})

  let outStream = pid.outputStream
  var line = ""

  while pid.running:
    try:
      let streamRes = outStream.readLine(line)
      if streamRes:
        yield line
    except IOError, OSError:
      assert outStream.isNil

  let rem = outStream.readAll().split("\n")
  for line in (if rem.len > 0: rem[0..^2] else: rem):
    yield line


for line in iterstdout("cat /etc/passwd | tail"):
  let vals = line.split(":")
  let username = vals[0]
  let password = vals[1]
  let userid = vals[2].parseInt()
  let groupid = vals[3].parseInt()
  let userIdInfo = vals[4]
  let homedir = vals[5]
  let shell = vals[6]

  echo &"{username:<30} ({userIdInfo}) \n{\"\":>10}has shell {shell} \n{\"\":>10}and homedir {homedir}"

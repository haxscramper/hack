import parseopt
import os
import colechopkg/lib
import hargparse

parseArgs:
  opt:
    name: "join"
    opt: ["--join", "-j"]
    help: "Join lines instead of appending value to each one"
  opt:
    name: "newline"
    opt: ["--newline", "-n"]
    help: "Add newline to the end of join string"
  opt:
    name: "no-end"
    opt: ["--no-end"]
    help: "Do not print newline at the end of the output"

if argParsed.len() < 1:
  ceUserError0("Missing join string")
else:
  let joinstr = argParsed[0] & (if "newline".kp(): "\n" else: "")
  if "join".kp():
    var first: bool = true
    for line in stdin.lines():
      if not first:
        stdout.write(joinstr)
      else:
        first = false

      stdout.write(line)
  else:
    for line in stdin.lines():
      stdout.write(line)
      stdout.write(joinstr)

  if not "no-end".kp():
    echo ""

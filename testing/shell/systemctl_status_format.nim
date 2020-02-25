import json
import strtabs
import tables
import strscans
import strformat

let statusfile = "status.tmp.txt"
let showfile = "show.tmp.txt"

let input = readFile(statusfile).string()


var status: seq[JsonNode]
var properties = newStringTable()

proc anything(input: string, argument: var string, start: int): int =
  let diff = input.len - start
  argument = input[start..^1]
  return diff

for line in showfile.lines():
  var key: string
  var val: string
  if scanf(line, "$w=${anything}", key, val):
    properties[key] = val
  else:
    echo &"cannot parse line {line}"

import std/[json, sequtils, tables, strutils]

import jsony, benchy

type
  Record = object
    name, time, server: string

proc renameHook*(v: var Record, fieldName: var string) =
  case fieldName:
    of "Name": fieldName = "name"
    of "Server": fieldName = "server"
    of "Time", "Ti me": fieldName = "time"


let inStr = """
[
 {"Name":"Foo", "Time":"1 minute", "Server":"abcd"},
 {"Name":"Bar", "Time":"10 minute", "Server":"abcd"},
 {"Name":"Lorem", "Time":"45 seconds", "Server":"xyz"},
 {"Name":"Ipsum", "Time":"2 minutes", "Server":"xyz"},
 {"Name":"Ipsum", "Ti me":"2 minutes", "Server":"xyz"}
]
"""

timeIt "using stdlib":
  let input = parseJson(inStr)
  var table: Table[string, seq[JsonNode]]
  for node in input:
    table.mgetOrPut(node["Server"].getStr(), @[]).add node

timeIt "using jsony":
  let input = fromJson(inStr, seq[Record])
  var table: Table[string, seq[Record]]
  for node in input:
    table.mgetOrPut(node.server, @[]).add node


let input = fromJson(inStr, seq[Record])
var table: Table[string, seq[Record]]
for node in input:
  table.mgetOrPut(node.server, @[]).add node

import hmisc/other/hpprint

pprint table

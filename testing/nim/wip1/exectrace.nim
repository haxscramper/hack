import std/[strformat, strutils, xmltree]

var indent = 0
var indentStr = ""
var scopeStack: seq[string]

proc tr(args: varargs[string]) =
  stdout.write indentStr
  echo args.join("")

proc incRecurse() =
  inc indent
  indentStr = "  ".repeat(indent)


proc incRecurse(kind: string) =
  incRecurse()
  scopeStack.add kind


proc decRecurse(withScope: bool = true) =
  dec indent
  indentStr = "  ".repeat(indent)
  if withScope:
    discard scopeStack.pop


proc trPushCall(name: string, args: string) =
  tr("<call>")
  incRecurse("call")

proc trPopCall() =
  decRecurse()
  tr("</call>")

proc trPushScope(kind: string) =
  tr &"<scope kind=\"{kind}\">"
  incRecurse(kind)

proc trPopScope() =
  decRecurse()
  tr "</scope>"

proc trCloseScopes(until: string) =
  while scopeStack.len > 0 and scopeStack[^1] != until:
    trPopScope()


proc trEmitAsgn(name, val: string) =
  tr "<asgn \"" & name & "\"=\"" & xmltree.escape(val) & "\">"

proc trPushProc(name: string, args: varargs[(string, string)]) =
  tr &"<proc-call name=\"{name}\">"
  incRecurse()
  tr "<args>"
  for (name, val) in args:
    tr &"  <arg {name}=\"{xmltree.escape(val)}\">"
  tr "  </args>"

proc trPopProc() =
  trCloseScopes("call")
  decRecurse(false)
  tr "</proc-call>"




proc testProc(arg: int) =
  trPushProc("testProc", ("arg", $arg))
  trPushScope("proc")

  if arg == 0:
    trPushScope("if")

    trPopProc()
    return

  else:
    trPushScope("if")
    trPopScope()

  trPopProc()


block:
  trPushScope("for")
  for i in 0 .. 3:
    trEmitAsgn("i", $i)

    trPushCall("testProc", "12")
    testProc(12)
    trPopCall()

  trPopScope()

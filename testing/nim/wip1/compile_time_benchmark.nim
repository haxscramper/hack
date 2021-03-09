import macros, sequtils, times, strutils

var buf {.compiletime.}: seq[string]

macro timedExecution(body: untyped): untyped =
  let parts = toSeq(body.children)[0 ..< 6]
  let bodyDef = body[6]
  let funcname = newLit($body[0])

  result = nnkProcDef.newTree(parts)
  result.add quote do:
    let start = cpuTime()
    `bodyDef`
    let final = cpuTime()
    when nimvm:
      buf.add(`funcname` & "," & $(final - start))
    else:
      discard

  echo result.toStrLit()

# {.push timedExecution.}

proc a(b: int) {.timedExecution.} =
  discard

# {.pop timedExecution.}

static:
  a(12)
  a(12)
  a(12)
  a(12)
  "/tmp/time.csv".writeFile(buf.join("\n"))

block:
  template m() {.pragma.}

  {.push m.}
  proc p() = discard
  {.pop.}

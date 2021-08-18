proc main() = assert false

{.line: (filename: "test", line: 1).}:
  proc main2() = main()

main2()

template reloc(inFile, inLine, body: untyped): untyped =
  {.line: (filename: inFile, line: inLine).}:
    body

reloc("test.nim", 1):
  let a: int = "123"

import std/macros

macro reloc2(inFile, inLine, body: untyped): untyped =
  result = quote do:
    {.line: (filename: `inFile`, line: `inLine`).}:
      `body`

reloc2("test.nim", 1):
  let a: int = "123"

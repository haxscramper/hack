import std/macros

macro makeIncludeStrLit(
  arg: static[string]): untyped =
  # ^ To pass value to macro use `static[<your-type>]`

  newTree(nnkIncludeStmt, newLit(arg))
  # Generates `include "your string"`

static:
  writeFile("/tmp/something.nim", "echo 123")

const path = "/tmp/something.nim"
# ^ path MUST be known at compile-time in order for macro to work.

makeIncludeStrLit(path)

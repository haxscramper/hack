import
  compiler/[nimeval, llstream, ast]

import
  std/[sequtils, os]

let stdlib* = getHomeDir() / ".choosenim/toolchains/nim-1.4.0/lib"

let intr = createInterpreter(
  "scriptname.nims",
  @[stdlib, stdlib / "pure", stdlib / "core"])

intr.evalScript(llStreamOpen("echo \"hello world!\""))
intr.destroyInterpreter()

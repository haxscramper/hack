# FIXME runtime error; cannot fine `system.nim`
import compiler/[nimeval, vmdef]
import std/os

proc initInterpreter(script: string): Interpreter =
  let std = findNimStdLib()
  echo std
  result = createInterpreter(script , [std, parentDir(currentSourcePath),
    std / "pure", std / "core"])

let i = initInterpreter(currentSourcePath())
i.implementRoutine("*", "exposed", "addFloats") do(vm: VmArgs):
  echo "hello world"

i.evalScript()

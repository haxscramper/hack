import hmisc/other/oswrap
import compiler/[nimeval]
import common

let intr = createInterpreter(
  "test1_script.nims",
  toSeqString(@[stdlib, stdlib / "pure", stdlib / "core"]))


intr.evalScript()
intr.destroyInterpreter()

import common
import compiler/[nimeval, ast]

let intr = createInterpreter(
  "test2_script.nims",
  toSeqString(@[stdlib, stdlib / "pure", stdlib / "core"]))

intr.evalScript()

let sym = intr.selectUniqueSymbol("testSeq")
var output: seq[string]
let val = intr.getGlobalValue(sym)
for data in val.sons:
  output.add data.strVal

echo output

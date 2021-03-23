import common
import compiler/[nimeval, llstream, ast]
import hnimast

let intr = createInterpreter(
  "sciptname.nims",
  toSeqString(@[
    stdlib,
    stdlib / "pure",
    stdlib / "core",
    stdlib / "pure" / "collections"
]))

intr.evalScript(llStreamOpen("""
import tables
let testTable* = {"hello": 42, "world": 100}.toTable
"""))

let val = intr.getGlobalValue(intr.selectUniqueSymbol("testTable"))
echo $val

echo treeRepr(val)

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
let hello* = @[1,2,3,4,4]
"""))

let val = intr.getGlobalValue(intr.selectUniqueSymbol("hello"))
echo $val

echo treeRepr(val)

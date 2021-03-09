import common
import compiler/[vmdef, vm, nimeval, llstream, ast, renderer]


let intr = createInterpreter(
  "sciptname.nims",
  toSeqString(@[
    stdlib,
    stdlib / "pure",
    stdlib / "core",
    stdlib / "pure" / "collections"
]))

let script = """
echo "script proc: ", scriptProc(12, 2)
echo "comiler proc: ", compilerProc(44, 40)
"""

intr.implementRoutine("*", "script", "compilerProc", proc (a: VmArgs) =
  echo "This is happening in compile-time code"
  a.setResult(a.getInt(0) + a.getInt(1))
)

intr.evalScript(llStreamOpen("""
proc scriptProc*(one, two: int): int = one * two
proc compilerProc*(one, two: int): int = discard
""" & script))

let foreignProc = intr.selectRoutine("scriptProc")


let ret = intr.callRoutine(
  foreignProc,
  [newIntNode(nkIntLit, 10), newIntNode(nkIntLit, 32)])

echo ret.intVal

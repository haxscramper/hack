import genny

proc printText() =
  echo "> print text called"

type
  Obj = object
    field*: int

proc getData(o: Obj): int = o.field + 12


# exportProcs:
#   printText

exportObject Obj:
  procs:
    getData(Obj)

writeFiles("generated", "genny_main")

include generated/internal

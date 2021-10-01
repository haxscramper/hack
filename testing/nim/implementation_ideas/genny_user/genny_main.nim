import genny

proc printText() =
  echo "> print text called"

type
  Obj = object
    field*: int

proc getData(o: Obj): int =
  echo o
  result = o.field + 12

iterator items(o: Obj): int =
  for i in 0 .. o.field:
    yield i

# exportProcs:
#   printText

exportObject Obj:
  procs:
    getData(Obj)

  iterators:
    items(Obj)

writeFiles("generated", "genny_main")

include generated/internal

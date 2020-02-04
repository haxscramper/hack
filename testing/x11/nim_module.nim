import hotcodereloading

static:
  echo "Recompiling nim module"

proc getCompileDate*(): string =
  const date = staticExec("date -Is")
  return "This module was compiled on " & date

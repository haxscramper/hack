import os, strutils
import compiler/[parser, llstream, idents, options, pathutils, astalgo, ast]


var filecount = 0
var unparsable: seq[string]
var sloc = 0
proc parsefile(file: string): PNode =
  var pars: TParser
  let cache: IdentCache = newIdentCache()
  let config: ConfigRef = newConfigRef()


  let strs = file.readFile()
  openParser(
    p = pars,
    filename = AbsoluteFile(file),
    inputStream = llStreamOpen(strs),
    cache = cache,
    config = config
  )

  try:
    result = parseAll(pars)
    closeParser(pars)
  except:
    unparsable.add file
    echo "file ", file, " could not be parsed"


for file in walkDirRec("/tmp/nimrepos"):
  if file.endsWith(".nim"):
    sleep 10
    let node = file.parsefile()
    if node != nil:
      inc filecount
    else:
      echo "[", file, "]"

# let node = "/tmp/nimrepos/spinner/tests/demo.nim".parseFile()

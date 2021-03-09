import times, stats

import compiler/[parser, llstream, idents, options, pathutils, astalgo]

var pars: TParser

let cache: IdentCache = newIdentCache()
let config: ConfigRef = newConfigRef()

var parseTime: RunningStat
var totalTime: RunningStat

for i in 0 .. 1:
  let startTime = cpuTime()

  openParser(
    p = pars,
    filename = AbsoluteFile(currentSourcePath()),
    inputStream = llStreamOpen("""
proc llStreamOpen*(data: string): PLLStream =
  new(result)
  result.s = data
  result.kind = llsString

proc llStreamOpen*(f: File): PLLStream =
  new(result)
  result.f = f
  result.kind = llsFile

proc llStreamOpen*(filename: AbsoluteFile, mode: FileMode): PLLStream =
  new(result)
  result.kind = llsFile
  if not open(result.f, filename.string, mode): result = nil

proc llStreamOpen*(): PLLStream =
  new(result)
  result.kind = llsNone

sfasdf asdfa
s dfas df
as fasdf(((((()))))))

proc llReadFromStdin(s: PLLStream, buf: pointer, bufLen: int): int
proc llStreamOpenStdIn*(r: TLLRepl = llReadFromStdin): PLLStream =
  new(result)
  result.kind = llsStdIn
  result.s = ""
  result.lineOffset = -1
  result.repl = r
"""),
    cache = cache,
    config = config
  )

  let parseStart = cpuTime()
  discard parseAll(pars)
  let parseEnd = cpuTime()

  closeParser(pars)
  let endTime = cpuTime()


  parseTime.push(parseEnd - parseStart)
  totalTime.push(endTime - startTime)

echo "total [ns]: ", int(totalTime.mean() * 1000 * 1000)
echo "parse [ns]: ", int(parseTime.mean() * 1000 * 1000)
echo "parse/total: ", int(parseTime.mean() / totalTime.mean() * 100), "%"

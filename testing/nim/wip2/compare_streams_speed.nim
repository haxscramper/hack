import std/[streams, strutils, os]
import pkg/[benchy, faststreams, faststreams/textio]

const
  thisFile = currentSourcePath()
  bigFile = "/tmp/bif_file"
  inText = readFile(thisFIle).repeat(10)

var f = open(bigFile, fmWrite)

for i in 0 ..< 10_000:
  f.write inText

f.close()

proc readLine*(s: InputStream, line: var string): bool =
  if s.readable:
    line = s.readLine()
    result = true


block add_to_strings:
  var str = newStringOfCap(10_000_000)
  var line: string

  template time(name: string, inStream: untyped): untyped =
    str.setLen(0)
    line = ""
    timeIt name:
      let s = inStream
      assert not isNil(s)
      while s.readLine(line):
        str.add line

      close(s)

  time "std/streams for string stream", newStringStream(inText)
  time "faststreams for string stream", unsafeMemoryInput(inText).s
  time "faststreams input file stream", fileInput(thisFile).s
  time "std/streams input file stream", newFileStream(thisFile, fmRead)
  time "faststreams big file", fileInput(bigFile).s
  time "std/streams big file", newFileStream(bigFile)

# I'm pretty sure these numbers don't mean a lot, especially for file
# bench.

# name ............................... min time      avg time    std dv   runs
# std/streams for string stream ...... 0.832 ms      0.961 ms    ±0.068  x1000
# faststreams for string stream ...... 0.376 ms      0.444 ms    ±0.031  x1000
# faststreams input file stream ...... 0.038 ms      0.052 ms    ±0.013  x1000
# std/streams input file stream ...... 0.047 ms      0.050 ms    ±0.002  x1000
# faststreams big file ............ 4601.517 ms   4624.733 ms   ±32.833     x2
# std/streams big file ............ 4405.507 ms   4428.728 ms   ±32.840     x2

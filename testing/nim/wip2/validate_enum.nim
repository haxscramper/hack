
import std/[macros, sequtils, strutils]

macro enumFullRange(a: typed): untyped =
  newNimNode(nnkCurly).add(a.getType[1][1..^1])

type
  LowMediumHigh* {.pure.} = enum
    Low = 1
    Medium = 2
    High = 3

  Antialiasing* {.pure.} = enum
    Off = "Off"
    FourSamples = "4Samples"
    EightSamples = "8Samples"

let data = @["1", "0", "Off", "4Samples"]

for d in data:
  try:
    let intVal = LowMediumHigh(parseInt(d))
    if intVal in enumFullRange(LowMediumHigh):
      echo "correct value for medium-high"

    else:
      echo "try antialiasing"

  except ValueError, RangeDefect:
    try:
      let enVal = parseEnum[Antialiasing](d)
      echo "parse enum: ", enVal

    except ValueError:
      echo "Completely invalid value - ", d

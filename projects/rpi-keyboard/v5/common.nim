import os
import sequtils
import bitops
import math

import hmisc/defensive

initDefense(
  logPath = true
)

const useMock* = fileExists("../desktop.lock")

when useMock:
  # static: echo "Importing mock library"
  import wiringPiMock
  export wiringPiMock
  const mockRun* = true
else:
  # static: echo "Importing real library"
  import wiringPiNim
  export wiringPiNim
  const mockRun* = false

# import os
import strutils, strformat

func debug*(msgs: varargs[string, `$`]): void =
  debugEcho msgs.join(" ")

proc setPinModeOut*(pin: int): void =
  piPinModeOutput(cast[cint](pin))

proc setPinModeIn*(pin: int): void =
  piPinModeInput(cast[cint](pin))

proc digitalWrite*(pin: int, mode: bool): void =
  let value: cint = if mode: 1 else: 0
  showLog &"Writing {value} to pin {pin}"
  piDigitalWrite(cast[cint](pin), value)

proc digitalRead*(pin: int): bool =
  defer:
    showLog &"Read {result} from pin {pin}"

  if piDigitalRead(cast[cint](pin)) == 1:
    true
  else:
    false

proc setPinPullDown*(pin: int): void =
  piPullDown(cast[cint](pin))

proc setPinPullUp*(pin: int): void =
  piPullUp(cast[cint](pin))


proc setPinPullOff*(pin: int): void =
  piPullOff(cast[cint](pin))


###########################  assertion macros  ############################

template assertEq*(lhs, rhs): untyped =
  block:
    let lhsRes = lhs
    let rhsRes = rhs

    if lhsRes != rhsRes:
      raise newException(
        AssertionError,
        "Equality assertion failed, left argument evaluated as" &
          $lhsRes & "and right one as" & $rhsRes)

###########################  helper algorithms  ###########################

func allSubsets*[T](elements: seq[T], emptyset: bool = false): seq[seq[T]] =
  ## Generate all subsets of the sequence. `emptyset` - whether or not
  ## to add empty subset
  var bitmask: seq[bool] = newSeqWith(elements.len, false)
  var counter = 1
  var prev = counter
  if emptyset: result.add @[]
  while counter < 2 ^ elements.len:
    let pos = firstSetBit(counter) - 1
    bitmask[pos] = not bitmask[pos]
    # echo bitmask.mapIt(if it: "1" else: "0").join("")

    var buff: seq[string]
    for (ok, item) in zip(bitmask, elements):
      if ok:
        buff.add item

    result.add buff

    inc counter

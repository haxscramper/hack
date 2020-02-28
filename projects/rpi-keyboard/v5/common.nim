import os
import sequtils
import bitops
import math

when fileExists("../desktop.lock"):
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

proc debug*(msg: string) =
  discard
  # echo msg

proc setPinModeOut*(pin: int): void =
  piPinModeOutput(cast[cint](pin))

proc setPinModeIn*(pin: int): void =
  piPinModeInput(cast[cint](pin))

proc digitalWrite*(pin: int, mode: bool): void =
  let value: cint = if mode: 1 else: 0
  debug &"Writing {value} to pin {pin}"
  piDigitalWrite(cast[cint](pin), value)

proc digitalRead*(pin: int): bool =
  defer:
    debug &"Read {result} from pin {pin}"

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

func allSubsets*[T](elements: seq[T]): seq[seq[T]] =
  ## Generate all subsets of the sequence.
  var bitmask: seq[bool] = newSeqWith(elements.len, false)
  var counter = 1
  var prev = counter
  result.add @[]
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

template anyofIt*(sequence: typed, predicate: untyped): bool =
  ## Return `true` if for any of the items in sequence `predicate`
  ## evaluates as `true`. Otherwise return false.
  var result = false
  for it {.inject.} in sequence:
    if predicate:
      result = true

  result

proc max*[T](x: openArray[T], default: T): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  ## use `default` as starting value for comparison.
  result = default
  for i in x:
    if result < i: result = i

import os

when fileExists("../desktop.lock"):
  static: echo "Importing mock library"
  import wiringPiMock
  export wiringPiMock
  const mockRun* = true
else:
  static: echo "Importing real library"
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

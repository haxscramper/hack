import common
import key_codes
import bitops
import sequtils
import strformat
import strutils

type
  HIDModifiers* = enum
    hmLeftCtrl = 0
    hmLeftShift = 1
    hmLeftAlt = 2
    hmLeftSuper = 3

    hmRightCtrl = 4
    hmRightShift = 5
    hmRightAlt = 6
    hmRightSuper = 7

  HIDReport* = object
    modifiers*: set[HIDModifiers]
    keycodes*: array[6, KeyCode]


proc writeHIDReport*(report: HIDReport) =
  ## Generate HID bits and write to `/dev/hidg0`
  var modifiers: uint8 = 0
  for it in report.modifiers:
    modifiers.setBit(ord(it))

  var final: array[8, uint8]

  final[0] = modifiers
  final[1] = 0 # ignored

  for idx, code in report.keycodes:
    # if idx == 0 or code != ccKeyNone:
    #   echo code
    final[2 + idx] = cast[uint8](code)


  when mockRun:
    echo (0..<final.len).mapIt(&"{it:^3}").join("|")
    echo final.mapIt(&"{it:^3}").join(" ")
  else:
    let file = open("/dev/hidg0", fmWrite)
    discard file.writeBytes(final, 0, 8)
    file.close()

proc fromEmacsNotation*(binding: string): HIDReport =
  let keys = binding.split("-")
  let key = keys[^1]
  let modifiers = keys[0..^2]
  if "C" in modifiers:
    result.modifiers.incl hmLeftCtrl
    result.modifiers.incl hmRightCtrl

  if "M" in modifiers:
    result.modifiers.incl hmLeftAlt
    result.modifiers.incl hmRightAlt

  if "s" in modifiers:
    result.modifiers.incl hmLeftSuper
    result.modifiers.incl hmRightSuper

  if "S" in modifiers:
    result.modifiers.incl hmLeftShift
    result.modifiers.incl hmRightShift

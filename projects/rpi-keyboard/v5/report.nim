import common
import algorithm
import hmisc/helpers
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
    hmLeftMeta = 3

    hmRightCtrl = 4
    hmRightShift = 5
    hmRightAlt = 6
    hmRightMeta = 7

  HIDReport* = object
    modifiers*: set[HIDModifiers]
    keycodes*: array[6, KeyCode]

func toHIDModifer*(code: KeyCode): HIDModifiers =
  ## Convert key code to HID modifier
  case code:
    of ccKeyLeftCtrl: hmLeftCtrl
    of ccKeyLeftShift: hmLeftShift
    of ccKeyLeftAlt: hmLeftAlt
    of ccKeyLeftMeta: hmLeftMeta
    of ccKeyRightCtrl: hmRightCtrl
    of ccKeyRightShift: hmRightShift
    of ccKeyRightAlt: hmRightAlt
    of ccKeyRightMeta: hmRightMeta
    else:
      raise newException(
        ValueError,
        """Only modifier key codes can be converter to modifiers""")

func toArray(report: HIDReport): array[8, uint8] =
  var modifiers: uint8 = 0
  for it in report.modifiers:
    modifiers.setBit(ord(it))

  result[0] = modifiers
  result[1] = 0 # ignored

  for idx, code in report.keycodes:
    # if idx == 0 or code != ccKeyNone:
    #   echo code
    result[2 + idx] = cast[uint8](code)

iterator `>..`(left, right: int): int =
  for num in countdown(left - 1, right):
    yield num


proc printHIDReport*(report: HIDReport) =
  let final = report.toArray()
  var modifierBits: array[8, bool]
  for bit in 8>..0:
    modifierBits[bit] = final[0].testBit(bit)

  let modifiers = modifierBits.reversed().mapIt(it.tern("1", "0")).join("")
  let bitNumbers = @["₇", "₆", "₅", "₄", "₃", "₂", "₁", "₀"].join("")

  echo bitNumbers, " |", (1..<final.len).mapIt(&"{it:^3}").join("|")
  echo modifiers, "   #  ", final[2..^1].mapIt((&"{it:^3}")).join(" ")

proc printHIDReport*(report: seq[HIDReport]) =
  for idx, rep in report:
    echo &"idx: {idx}"
    printHIDReport(rep)

proc writeHIDReport*(report: HIDReport) =
  ## Generate HID bits and write to `/dev/hidg0`
  when mockRun:
    printHIDReport(report)
  else:
    let final = report.toArray()
    let file = open("/dev/hidg0", fmWrite)
    discard file.writeBytes(final, 0, 8)
    file.close()

proc fromEmacsNotation*(binding: string): seq[HIDReport] =
  ## Convert string in emacs notation to keyboard hid report. `M` is
  ## Alt key, `S` is shift, `C` is ctrl, `s` is meta (super/win key)
  for chord in binding.split(" "):
    let keys = chord.split("-")
    let key = keys[^1]
    let modifiers = keys[0..^2]
    var res: HIDReport
    if "C" in modifiers:
      res.modifiers.incl hmLeftCtrl
      res.modifiers.incl hmRightCtrl

    if "M" in modifiers:
      res.modifiers.incl hmLeftAlt
      res.modifiers.incl hmRightAlt

    if "s" in modifiers:
      res.modifiers.incl hmLeftMeta
      res.modifiers.incl hmRightMeta

    if "S" in modifiers:
      res.modifiers.incl hmLeftShift
      res.modifiers.incl hmRightShift

    res.keycodes[0] = fromEmacsKeyName(key)
    result.add res

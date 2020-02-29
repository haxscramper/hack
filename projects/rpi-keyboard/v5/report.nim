import common
import json
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
    # TODO write proc for getting/setting value of nth key and add
    # counter for number of used keys
    keycodes*: array[6, KeyCode]

  KeyConfig* = object
    isFinal*: bool
    makeDefault*: bool
    modifierMap*: seq[(seq[string], string)]


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

proc fromKeybindingStr*(binding: string): seq[HIDReport] =
  ## Convert string in notation to keyboard hid report.
  for chord in binding.split(" "):
    let keys = chord.split("-")
    let key = keys[^1]
    let modifiers = keys[0..^2]
    var res: HIDReport
    if "ctrl" in modifiers: res.modifiers.incl hmLeftCtrl
    if "alt" in modifiers: res.modifiers.incl hmLeftAlt
    if "meta" in modifiers: res.modifiers.incl hmLeftMeta
    if "shift" in modifiers: res.modifiers.incl hmLeftShift

    if key == "":
      res.keycodes[0] = ccKeyNone
    else:
      res.keycodes[0] = fromKeyName(key)

    result.add res

func decodeKeybindingConf*(conf: JsonNode): (KeyConfig, KeyConfig) =
  ## Convert keybinding configuration from json node into `KeyConfig`
  discard


proc toKeybindingStr*(rep: HIDReport): string =
  var buf: seq[string]
  for modif in rep.modifiers:
    case modif:
      of hmLeftAlt, hmRightAlt: buf.add "alt"
      of hmLeftCtrl, hmRightCtrl: buf.add "ctrl"
      of hmLeftMeta, hmRightMeta: buf.add "meta"
      of hmLeftShift, hmRightShift: buf.add "shift"


  let cc = rep.keycodes[0]
  return (buf.len > 0).tern(
    buf.join("-") & "-", ""
  ) & (cc != ccKeyNone).tern(
    cc.getKeyName(), ""
  )

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
    echo &"idx: {idx}, keys: {rep.toKeybindingStr()}"
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

import common
import algorithm
import hmisc/[helpers, hjson]
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
    isActive*: bool ## Key configuration is activated
    isFinal*: bool ## Key should be send immediately
    makeDefault*: bool ## Make default configuration for key
    modifierMap*: seq[(seq[string], string)] ## {modifiers} -> Key
                                             ## sequence mapping


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

func keyConfFromCode*(code: KeyCode): KeyConfig =
  KeyConfig(
    isActive: true,
    isFinal: false,
    makeDefault: true,
    modifierMap: @[(@["default"], code.getKeyName())]
  )

func decodeKeybindingConf*(conf: JsonNode): (KeyConfig, KeyConfig) =
  ##[ Convert keybinding configuration from json node into `KeyConfig`.
  First generated key is `onPress` trigger, second one is `onRelease`.
  Configuration (json node) will be parsed according to the rules:

  - single item (string) - will be converted to press key
  - Array of elements - will generate final key without default
    values. Each element has following fields:
    - `event`: run this on release or on press
    - `mod`: modifiers necessary to activate this. Default
      is the name for the press without any modifiers
    - `key`: Key sequence to run when the key is pressed

  :return: On press and on release triggers for a key configuration.
    First field in tuple is press configuration, second one is
    release.

  ]##
  if conf.kind == JString: # Single string with non-final key
    result[0].isFinal = false
    result[0].makeDefault = true
    result[0].modifierMap = @[(@["default"], conf.asStr())]
    result[0].isActive = true
  elif conf.kind == JArray:
    let elems: seq[JsonNode] = conf.getElems()
    if elems.len() < 1:
      # TODO error?
      discard
    else: # JObject with final key
      result[0].isFinal = true; result[0].makeDefault = false
      result[1].isFinal = true; result[1].makeDefault = false
      for it in elems:
        if it["mod"].kind != JArray:
          raise newException(
            AssertionError,
            "list of modifier must be an array" &
            "Error while attempting to parse configuration " &
            $conf
          )

        let conf = (
          it["mod"].asStrSeq(),
          it["key"].asStr()
        )

        if it.hasKey("event") and it["event"].getStr() == "onRelease":
          result[1].modifierMap.add conf
          result[1].isActive = true
        else:
          result[0].modifierMap.add conf
          result[0].isActive = true


      #   let (press, _) = elems[0].decodeKeybindingConf()
      #   let (release, _) = elems[1].decodeKeybindingConf()
      #   result = (press, release)
      # else:
      #   for it in elems:
      #     let modifier = it[0]
      #     let keybind = it[1]



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

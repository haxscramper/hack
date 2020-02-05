import tables
import enum_iterate

type KeyCode* = enum
  ccKeyNone = 0x00'u8 ## No key

  ccKeyA = 0x04'u8 ## Keyboard a and A
  ccKeyB = 0x05'u8 ## Keyboard b and B
  ccKeyC = 0x06'u8 ## Keyboard c and C
  ccKeyD = 0x07'u8 ## Keyboard d and D
  ccKeyE = 0x08'u8 ## Keyboard e and E
  ccKeyF = 0x09'u8 ## Keyboard f and F
  ccKeyG = 0x0a'u8 ## Keyboard g and G
  ccKeyH = 0x0b'u8 ## Keyboard h and H
  ccKeyI = 0x0c'u8 ## Keyboard i and I
  ccKeyJ = 0x0d'u8 ## Keyboard j and J
  ccKeyK = 0x0e'u8 ## Keyboard k and K
  ccKeyL = 0x0f'u8 ## Keyboard l and L
  ccKeyM = 0x10'u8 ## Keyboard m and M
  ccKeyN = 0x11'u8 ## Keyboard n and N
  ccKeyO = 0x12'u8 ## Keyboard o and O
  ccKeyP = 0x13'u8 ## Keyboard p and P
  ccKeyQ = 0x14'u8 ## Keyboard q and Q
  ccKeyR = 0x15'u8 ## Keyboard r and R
  ccKeyS = 0x16'u8 ## Keyboard s and S
  ccKeyT = 0x17'u8 ## Keyboard t and T
  ccKeyU = 0x18'u8 ## Keyboard u and U
  ccKeyV = 0x19'u8 ## Keyboard v and V
  ccKeyW = 0x1a'u8 ## Keyboard w and W
  ccKeyX = 0x1b'u8 ## Keyboard x and X
  ccKeyY = 0x1c'u8 ## Keyboard y and Y
  ccKeyZ = 0x1d'u8 ## Keyboard z and Z

  ccKey1 = 0x1e'u8 ## Keyboard 1 and !
  ccKey2 = 0x1f'u8 ## Keyboard 2 and @
  ccKey3 = 0x20'u8 ## Keyboard 3 and #
  ccKey4 = 0x21'u8 ## Keyboard 4 and $
  ccKey5 = 0x22'u8 ## Keyboard 5 and %
  ccKey6 = 0x23'u8 ## Keyboard 6 and ^
  ccKey7 = 0x24'u8 ## Keyboard 7 and &
  ccKey8 = 0x25'u8 ## Keyboard 8 and *
  ccKey9 = 0x26'u8 ## Keyboard 9 and (
  ccKey0 = 0x27'u8 ## Keyboard 0 and )

  ccKeyENTER = 0x28'u8 ## Keyboard Return (ENTER)
  ccKeyESC = 0x29'u8 ## Keyboard ESCAPE
  ccKeyBACKSPACE = 0x2a'u8 ## Keyboard DELETE (Backspace)
  ccKeyTAB = 0x2b'u8 ## Keyboard Tab
  ccKeySPACE = 0x2c'u8 ## Keyboard Spacebar
  ccKeyMINUS = 0x2d'u8 ## Keyboard - and _
  ccKeyEQUAL = 0x2e'u8 ## Keyboard = and +
  ccKeyLEFTBRACE = 0x2f'u8 ## Keyboard [ and {
  ccKeyRIGHTBRACE = 0x30'u8 ## Keyboard ] and }
  ccKeyBACKSLASH = 0x31'u8 ## Keyboard \ and |
  ccKeyHASHTILDE = 0x32'u8 ## Keyboard Non-US # and ~
  ccKeySEMICOLON = 0x33'u8 ## Keyboard ; and :
  ccKeyAPOSTROPHE = 0x34'u8 ## Keyboard ' and "
  ccKeyGRAVE = 0x35'u8 ## Keyboard ` and ~
  ccKeyCOMMA = 0x36'u8 ## Keyboard , and <
  ccKeyDOT = 0x37'u8 ## Keyboard . and >
  ccKeySLASH = 0x38'u8 ## Keyboard / and ?
  ccKeyCAPSLOCK = 0x39'u8 ## Keyboard Caps Lock

  ccKeyF1 = 0x3a'u8 ## Keyboard F1
  ccKeyF2 = 0x3b'u8 ## Keyboard F2
  ccKeyF3 = 0x3c'u8 ## Keyboard F3
  ccKeyF4 = 0x3d'u8 ## Keyboard F4
  ccKeyF5 = 0x3e'u8 ## Keyboard F5
  ccKeyF6 = 0x3f'u8 ## Keyboard F6
  ccKeyF7 = 0x40'u8 ## Keyboard F7
  ccKeyF8 = 0x41'u8 ## Keyboard F8
  ccKeyF9 = 0x42'u8 ## Keyboard F9
  ccKeyF10 = 0x43'u8 ## Keyboard F10
  ccKeyF11 = 0x44'u8 ## Keyboard F11
  ccKeyF12 = 0x45'u8 ## Keyboard F12

  ccKeySYSRQ = 0x46'u8 ## Keyboard Print Screen
  ccKeySCROLLLOCK = 0x47'u8 ## Keyboard Scroll Lock
  ccKeyPAUSE = 0x48'u8 ## Keyboard Pause
  ccKeyINSERT = 0x49'u8 ## Keyboard Insert
  ccKeyHOME = 0x4a'u8 ## Keyboard Home
  ccKeyPAGEUP = 0x4b'u8 ## Keyboard Page Up
  ccKeyDELETE = 0x4c'u8 ## Keyboard Delete Forward
  ccKeyEND = 0x4d'u8 ## Keyboard End
  ccKeyPAGEDOWN = 0x4e'u8 ## Keyboard Page Down
  ccKeyRIGHT = 0x4f'u8 ## Keyboard Right Arrow
  ccKeyLEFT = 0x50'u8 ## Keyboard Left Arrow
  ccKeyDOWN = 0x51'u8 ## Keyboard Down Arrow
  ccKeyUP = 0x52'u8 ## Keyboard Up Arrow

  ccKeyNUMLOCK = 0x53'u8 ## Keyboard Num Lock and Clear
  ccKeyKPSLASH = 0x54'u8 ## Keypad /
  ccKeyKPASTERISK = 0x55'u8 ## Keypad *
  ccKeyKPMINUS = 0x56'u8 ## Keypad -
  ccKeyKPPLUS = 0x57'u8 ## Keypad +
  ccKeyKPENTER = 0x58'u8 ## Keypad ENTER
  ccKeyKP1 = 0x59'u8 ## Keypad 1 and End
  ccKeyKP2 = 0x5a'u8 ## Keypad 2 and Down Arrow
  ccKeyKP3 = 0x5b'u8 ## Keypad 3 and PageDn
  ccKeyKP4 = 0x5c'u8 ## Keypad 4 and Left Arrow
  ccKeyKP5 = 0x5d'u8 ## Keypad 5
  ccKeyKP6 = 0x5e'u8 ## Keypad 6 and Right Arrow
  ccKeyKP7 = 0x5f'u8 ## Keypad 7 and Home
  ccKeyKP8 = 0x60'u8 ## Keypad 8 and Up Arrow
  ccKeyKP9 = 0x61'u8 ## Keypad 9 and Page Up
  ccKeyKP0 = 0x62'u8 ## Keypad 0 and Insert
  ccKeyKPDOT = 0x63'u8 ## Keypad . and Delete

  ccKey102ND = 0x64'u8 ## Keyboard Non-US \ and |
  ccKeyCOMPOSE = 0x65'u8 ## Keyboard Application
  ccKeyPOWER = 0x66'u8 ## Keyboard Power
  ccKeyKPEQUAL = 0x67'u8 ## Keypad =

  ccKeyF13 = 0x68'u8 ## Keyboard F13
  ccKeyF14 = 0x69'u8 ## Keyboard F14
  ccKeyF15 = 0x6a'u8 ## Keyboard F15
  ccKeyF16 = 0x6b'u8 ## Keyboard F16
  ccKeyF17 = 0x6c'u8 ## Keyboard F17
  ccKeyF18 = 0x6d'u8 ## Keyboard F18
  ccKeyF19 = 0x6e'u8 ## Keyboard F19
  ccKeyF20 = 0x6f'u8 ## Keyboard F20
  ccKeyF21 = 0x70'u8 ## Keyboard F21
  ccKeyF22 = 0x71'u8 ## Keyboard F22
  ccKeyF23 = 0x72'u8 ## Keyboard F23
  ccKeyF24 = 0x73'u8 ## Keyboard F24

  ccKeyOPEN = 0x74'u8 ## Keyboard Execute
  ccKeyHELP = 0x75'u8 ## Keyboard Help
  ccKeyPROPS = 0x76'u8 ## Keyboard Menu
  ccKeyFRONT = 0x77'u8 ## Keyboard Select
  ccKeySTOP = 0x78'u8 ## Keyboard Stop
  ccKeyAGAIN = 0x79'u8 ## Keyboard Again
  ccKeyUNDO = 0x7a'u8 ## Keyboard Undo
  ccKeyCUT = 0x7b'u8 ## Keyboard Cut
  ccKeyCOPY = 0x7c'u8 ## Keyboard Copy
  ccKeyPASTE = 0x7d'u8 ## Keyboard Paste
  ccKeyFIND = 0x7e'u8 ## Keyboard Find
  ccKeyMUTE = 0x7f'u8 ## Keyboard Mute
  ccKeyVOLUMEUP = 0x80'u8 ## Keyboard Volume Up
  ccKeyVOLUMEDOWN = 0x81'u8 ## Keyboard Volume Down
  ## 0x82'u8  ccKeyboard Locking Caps Lock
  ## 0x83'u8  ccKeyboard Locking Num Lock
  ## 0x84  ccKeyboard Locking Scroll Lock
  ccKeyKPCOMMA = 0x85'u8 ## Keypad Comma
  ## 0x86'u8  ccKeypad Equal Sign
  ccKeyRO = 0x87'u8 ## Keyboard International1
  ccKeyKATAKANAHIRAGANA = 0x88'u8 ## Keyboard International2
  ccKeyYEN = 0x89'u8 ## Keyboard International3
  ccKeyHENKAN = 0x8a'u8 ## Keyboard International4
  ccKeyMUHENKAN = 0x8b'u8 ## Keyboard International5
  ccKeyKPJPCOMMA = 0x8c'u8 ## Keyboard International6
  ## 0x8d'u8  ccKeyboard International7
  ## 0x8e'u8  ccKeyboard International8
  ## 0x8f'u8  ccKeyboard International9
  ccKeyHANGEUL = 0x90'u8 ## Keyboard LANG1
  ccKeyHANJA = 0x91'u8 ## Keyboard LANG2
  ccKeyKATAKANA = 0x92'u8 ## Keyboard LANG3
  ccKeyHIRAGANA = 0x93'u8 ## Keyboard LANG4
  ccKeyZENKAKUHANKAKU = 0x94'u8 ## Keyboard LANG5
  ## 0x95'u8  ccKeyboard LANG6
  ## 0x96'u8  ccKeyboard LANG7
  ## 0x97'u8  ccKeyboard LANG8
  ## 0x98'u8  ccKeyboard LANG9
  ## 0x99'u8  ccKeyboard Alternate Erase
  ## 0x9a'u8  ccKeyboard SysReq/Attention
  ## 0x9b'u8  ccKeyboard Cancel
  ## 0x9c'u8  ccKeyboard Clear
  ## 0x9d'u8  ccKeyboard Prior
  ## 0x9e'u8  ccKeyboard Return
  ## 0x9f'u8  ccKeyboard Separator
  ## 0xa0'u8  ccKeyboard Out
  ## 0xa1'u8  ccKeyboard Oper
  ## 0xa2'u8  ccKeyboard Clear/Again
  ## 0xa3'u8  ccKeyboard CrSel/Props
  ## 0xa4'u8  ccKeyboard ExSel

  ## 0xb0'u8  ccKeypad 00
  ## 0xb1'u8  ccKeypad 000
  ## 0xb2'u8  Thousands Separator
  ## 0xb3'u8  Decimal Separator
  ## 0xb4'u8  Currency Unit
  ## 0xb5'u8  Currency Sub-unit
  ccKeyKPLEFTPAREN = 0xb6'u8 ## Keypad (
  ccKeyKPRIGHTPAREN = 0xb7'u8 ## Keypad )
  ## 0xb8'u8  ccKeypad {
  ## 0xb9'u8  ccKeypad }
  ## 0xba'u8  ccKeypad Tab
  ## 0xbb'u8  ccKeypad Backspace
  ## 0xbc'u8  ccKeypad A
  ## 0xbd'u8  ccKeypad B
  ## 0xbe'u8  ccKeypad C
  ## 0xbf'u8  ccKeypad D
  ## 0xc0'u8  ccKeypad E
  ## 0xc1'u8  ccKeypad F
  ## 0xc2'u8  ccKeypad XOR
  ## 0xc3'u8  ccKeypad ^
  ## 0xc4'u8  ccKeypad %
  ## 0xc5'u8  ccKeypad <
  ## 0xc6'u8  ccKeypad >
  ## 0xc7'u8  ccKeypad &
  ## 0xc8'u8  ccKeypad &&
  ## 0xc9'u8  ccKeypad |
  ## 0xca'u8  ccKeypad ||
  ## 0xcb'u8  ccKeypad :
  ## 0xcc'u8  ccKeypad #
  ## 0xcd'u8  ccKeypad Space
  ## 0xce'u8  ccKeypad @
  ## 0xcf'u8  ccKeypad !
  ## 0xd0'u8  ccKeypad Memory Store
  ## 0xd1'u8  ccKeypad Memory Recall
  ## 0xd2'u8  ccKeypad Memory Clear
  ## 0xd3'u8  ccKeypad Memory Add
  ## 0xd4'u8  ccKeypad Memory Subtract
  ## 0xd5'u8  ccKeypad Memory Multiply
  ## 0xd6'u8  ccKeypad Memory Divide
  ## 0xd7'u8  ccKeypad +/-
  ## 0xd8'u8  ccKeypad Clear
  ## 0xd9'u8  ccKeypad Clear Entry
  ## 0xda'u8  ccKeypad Binary
  ## 0xdb'u8  ccKeypad Octal
  ## 0xdc'u8  ccKeypad Decimal
  ## 0xdd'u8  ccKeypad Hexadecimal

  ccKeyLEFTCTRL = 0xe0'u8 ## Keyboard Left Control
  ccKeyLEFTSHIFT = 0xe1'u8 ## Keyboard Left Shift
  ccKeyLEFTALT = 0xe2'u8 ## Keyboard Left Alt
  ccKeyLEFTMETA = 0xe3'u8 ## Keyboard Left GUI
  ccKeyRIGHTCTRL = 0xe4'u8 ## Keyboard Right Control
  ccKeyRIGHTSHIFT = 0xe5'u8 ## Keyboard Right Shift
  ccKeyRIGHTALT = 0xe6'u8 ## Keyboard Right Alt
  ccKeyRIGHTMETA = 0xe7'u8 ## Keyboard Right GUI

  ccKeyMEDIA_PLAYPAUSE = 0xe8'u8
  ccKeyMEDIA_STOPCD = 0xe9'u8
  ccKeyMEDIA_PREVIOUSSONG = 0xea'u8
  ccKeyMEDIA_NEXTSONG = 0xeb'u8
  ccKeyMEDIA_EJECTCD = 0xec'u8
  ccKeyMEDIA_VOLUMEUP = 0xed'u8
  ccKeyMEDIA_VOLUMEDOWN = 0xee'u8
  ccKeyMEDIA_MUTE = 0xef'u8
  ccKeyMEDIA_WWW = 0xf0'u8
  ccKeyMEDIA_BACK = 0xf1'u8
  ccKeyMEDIA_FORWARD = 0xf2'u8
  ccKeyMEDIA_STOP = 0xf3'u8
  ccKeyMEDIA_FIND = 0xf4'u8
  ccKeyMEDIA_SCROLLUP = 0xf5'u8
  ccKeyMEDIA_SCROLLDOWN = 0xf6'u8
  ccKeyMEDIA_EDIT = 0xf7'u8
  ccKeyMEDIA_SLEEP = 0xf8'u8
  ccKeyMEDIA_COFFEE = 0xf9'u8
  ccKeyMEDIA_REFRESH = 0xfa'u8
  ccKeyMEDIA_CALC = 0xfb'u8

proc getEmacsKeyName*(key: KeyCode): string =
  case key:
    of ccKeyNone: "None"

    of ccKeyA: "a"
    of ccKeyB: "b"
    of ccKeyC: "c"
    of ccKeyD: "d"
    of ccKeyE: "e"
    of ccKeyF: "f"
    of ccKeyG: "g"
    of ccKeyH: "h"
    of ccKeyI: "i"
    of ccKeyJ: "j"
    of ccKeyK: "k"
    of ccKeyL: "l"
    of ccKeyM: "m"
    of ccKeyN: "n"
    of ccKeyO: "o"
    of ccKeyP: "p"
    of ccKeyQ: "q"
    of ccKeyR: "r"
    of ccKeyS: "s"
    of ccKeyT: "t"
    of ccKeyU: "u"
    of ccKeyV: "v"
    of ccKeyW: "w"
    of ccKeyX: "x"
    of ccKeyY: "y"
    of ccKeyZ: "z"

    of ccKey1: "1"
    of ccKey2: "2"
    of ccKey3: "3"
    of ccKey4: "4"
    of ccKey5: "5"
    of ccKey6: "6"
    of ccKey7: "7"
    of ccKey8: "8"
    of ccKey9: "9"
    of ccKey0: "0"

    of ccKeyENTER: "ENTER"
    of ccKeyESC: "ESC"
    of ccKeyBACKSPACE: "BACKSPACE"
    of ccKeyTAB: "TAB"
    of ccKeySPACE: "SPACE"
    of ccKeyMINUS: "MINUS"
    of ccKeyEQUAL: "EQUAL"
    of ccKeyLEFTBRACE: "("
    of ccKeyRIGHTBRACE: ")"
    of ccKeyBACKSLASH: "\\"
    of ccKeyHASHTILDE: "HASHTILDE"
    of ccKeySEMICOLON: ":"
    of ccKeyAPOSTROPHE: "'"
    of ccKeyGRAVE: "GRAVE"
    of ccKeyCOMMA: ","
    of ccKeyDOT: "."
    of ccKeySLASH: "/"
    of ccKeyCAPSLOCK: "CAPSLOCK"

    of ccKeyF1: "<F1>"
    of ccKeyF2: "<F2>"
    of ccKeyF3: "<F3>"
    of ccKeyF4: "<F4>"
    of ccKeyF5: "<F5>"
    of ccKeyF6: "<F6>"
    of ccKeyF7: "<F7>"
    of ccKeyF8: "<F8>"
    of ccKeyF9: "<F9>"
    of ccKeyF10: "<F10>"
    of ccKeyF11: "<F11>"
    of ccKeyF12: "<F12>"

    of ccKeySYSRQ: "<SYSRQ>"
    of ccKeySCROLLLOCK: "<SCROLLLOCK>"
    of ccKeyPAUSE: "<PAUSE>"
    of ccKeyINSERT: "<INSERT>"
    of ccKeyHOME: "<HOME>"
    of ccKeyPAGEUP: "<PAGEUP>"
    of ccKeyDELETE: "<DELETE>"
    of ccKeyEND: "<END>"
    of ccKeyPAGEDOWN: "<PAGEDOWN>"
    of ccKeyRIGHT: "<RIGHT>"
    of ccKeyLEFT: "<LEFT>"
    of ccKeyDOWN: "<DOWN>"
    of ccKeyUP: "<UP>"

    of ccKeyNUMLOCK: "<NUMLOCK>"
    of ccKeyKPSLASH: "<KPSLASH>"
    of ccKeyKPASTERISK: "<KPASTERISK>"
    of ccKeyKPMINUS: "<KPMINUS>"
    of ccKeyKPPLUS: "<KPPLUS>"
    of ccKeyKPENTER: "<KPENTER>"
    of ccKeyKP1: "<KP1>"
    of ccKeyKP2: "<KP2>"
    of ccKeyKP3: "<KP3>"
    of ccKeyKP4: "<KP4>"
    of ccKeyKP5: "<KP5>"
    of ccKeyKP6: "<KP6>"
    of ccKeyKP7: "<KP7>"
    of ccKeyKP8: "<KP8>"
    of ccKeyKP9: "<KP9>"
    of ccKeyKP0: "<KP0>"
    of ccKeyKPDOT: "<KPDOT>"

    of ccKey102ND: "<102ND>"
    of ccKeyCOMPOSE: "<COMPOSE>"
    of ccKeyPOWER: "<POWER>"
    of ccKeyKPEQUAL: "<KPEQUAL>"

    of ccKeyF13: "<F13>"
    of ccKeyF14: "<F14>"
    of ccKeyF15: "<F15>"
    of ccKeyF16: "<F16>"
    of ccKeyF17: "<F17>"
    of ccKeyF18: "<F18>"
    of ccKeyF19: "<F19>"
    of ccKeyF20: "<F20>"
    of ccKeyF21: "<F21>"
    of ccKeyF22: "<F22>"
    of ccKeyF23: "<F23>"
    of ccKeyF24: "<F24>"

    of ccKeyOPEN: "<OPEN>"
    of ccKeyHELP: "<HELP>"
    of ccKeyPROPS: "<PROPS>"
    of ccKeyFRONT: "<FRONT>"
    of ccKeySTOP: "<STOP>"
    of ccKeyAGAIN: "<AGAIN>"
    of ccKeyUNDO: "<UNDO>"
    of ccKeyCUT: "<CUT>"
    of ccKeyCOPY: "<COPY>"
    of ccKeyPASTE: "<PASTE>"
    of ccKeyFIND: "<FIND>"
    of ccKeyMUTE: "<MUTE>"
    of ccKeyVOLUMEUP: "<VOLUMEUP>"
    of ccKeyVOLUMEDOWN: "<VOLUMEDOWN>"
    # 0x82'u8  ccKeyboard Locking Caps Lock
    # 0x83'u8  ccKeyboard Locking Num Lock
    # 0x84  ccKeyboard Locking Scroll Lock
    of ccKeyKPCOMMA: "<KPCOMMA>"
    # 0x86'u8  ccKeypad Equal Sign
    of ccKeyRO: "<RO>"
    of ccKeyKATAKANAHIRAGANA: "<KATAKANAHIRAGANA>"
    of ccKeyYEN: "<YEN>"
    of ccKeyHENKAN: "<HENKAN>"
    of ccKeyMUHENKAN: "<MUHENKAN>"
    of ccKeyKPJPCOMMA: "<KPJPCOMMA>"
    # 0x8d'u8  ccKeyboard International7
    # 0x8e'u8  ccKeyboard International8
    # 0x8f'u8  ccKeyboard International9
    of ccKeyHANGEUL: "<HANGEUL>"
    of ccKeyHANJA: "<HANJA>"
    of ccKeyKATAKANA: "<KATAKANA>"
    of ccKeyHIRAGANA: "<HIRAGANA>"
    of ccKeyZENKAKUHANKAKU: "<ZENKAKUHANKAKU>"
    # 0x95'u8  ccKeyboard LANG6
    # 0x96'u8  ccKeyboard LANG7
    # 0x97'u8  ccKeyboard LANG8
    # 0x98'u8  ccKeyboard LANG9
    # 0x99'u8  ccKeyboard Alternate Erase
    # 0x9a'u8  ccKeyboard SysReq/Attention
    # 0x9b'u8  ccKeyboard Cancel
    # 0x9c'u8  ccKeyboard Clear
    # 0x9d'u8  ccKeyboard Prior
    # 0x9e'u8  ccKeyboard Return
    # 0x9f'u8  ccKeyboard Separator
    # 0xa0'u8  ccKeyboard Out
    # 0xa1'u8  ccKeyboard Oper
    # 0xa2'u8  ccKeyboard Clear/Again
    # 0xa3'u8  ccKeyboard CrSel/Props
    # 0xa4'u8  ccKeyboard ExSel

    # 0xb0'u8  ccKeypad 00
    # 0xb1'u8  ccKeypad 000
    # 0xb2'u8  Thousands Separator
    # 0xb3'u8  Decimal Separator
    # 0xb4'u8  Currency Unit
    # 0xb5'u8  Currency Sub-unit
    of ccKeyKPLEFTPAREN: "<KPLEFTPAREN>"
    of ccKeyKPRIGHTPAREN: "<KPRIGHTPAREN>"
    # 0xb8'u8  ccKeypad {
    # 0xb9'u8  ccKeypad }
    # 0xba'u8  ccKeypad Tab
    # 0xbb'u8  ccKeypad Backspace
    # 0xbc'u8  ccKeypad A
    # 0xbd'u8  ccKeypad B
    # 0xbe'u8  ccKeypad C
    # 0xbf'u8  ccKeypad D
    # 0xc0'u8  ccKeypad E
    # 0xc1'u8  ccKeypad F
    # 0xc2'u8  ccKeypad XOR
    # 0xc3'u8  ccKeypad ^
    # 0xc4'u8  ccKeypad %
    # 0xc5'u8  ccKeypad <
    # 0xc6'u8  ccKeypad >
    # 0xc7'u8  ccKeypad &
    # 0xc8'u8  ccKeypad &&
    # 0xc9'u8  ccKeypad |
    # 0xca'u8  ccKeypad ||
    # 0xcb'u8  ccKeypad :
    # 0xcc'u8  ccKeypad #
    # 0xcd'u8  ccKeypad Space
    # 0xce'u8  ccKeypad @
    # 0xcf'u8  ccKeypad !
    # 0xd0'u8  ccKeypad Memory Store
    # 0xd1'u8  ccKeypad Memory Recall
    # 0xd2'u8  ccKeypad Memory Clear
    # 0xd3'u8  ccKeypad Memory Add
    # 0xd4'u8  ccKeypad Memory Subtract
    # 0xd5'u8  ccKeypad Memory Multiply
    # 0xd6'u8  ccKeypad Memory Divide
    # 0xd7'u8  ccKeypad +/-
    # 0xd8'u8  ccKeypad Clear
    # 0xd9'u8  ccKeypad Clear Entry
    # 0xda'u8  ccKeypad Binary
    # 0xdb'u8  ccKeypad Octal
    # 0xdc'u8  ccKeypad Decimal
    # 0xdd'u8  ccKeypad Hexadecimal

    of ccKeyLEFTCTRL: "<LEFTCTRL>"
    of ccKeyLEFTSHIFT: "<LEFTSHIFT>"
    of ccKeyLEFTALT: "<LEFTALT>"
    of ccKeyLEFTMETA: "<LEFTMETA>"
    of ccKeyRIGHTCTRL: "<RIGHTCTRL>"
    of ccKeyRIGHTSHIFT: "<RIGHTSHIFT>"
    of ccKeyRIGHTALT: "<RIGHTALT>"
    of ccKeyRIGHTMETA: "<RIGHTMETA>"

    of ccKeyMEDIA_PLAYPAUSE: "<MEDIA_PLAYPAUSE>"
    of ccKeyMEDIA_STOPCD: "<MEDIA_STOPCD>"
    of ccKeyMEDIA_PREVIOUSSONG: "<MEDIA_PREVIOUSSONG>"
    of ccKeyMEDIA_NEXTSONG: "<MEDIA_NEXTSONG>"
    of ccKeyMEDIA_EJECTCD: "<MEDIA_EJECTCD>"
    of ccKeyMEDIA_VOLUMEUP: "<MEDIA_VOLUMEUP>"
    of ccKeyMEDIA_VOLUMEDOWN: "<MEDIA_VOLUMEDOWN>"
    of ccKeyMEDIA_MUTE: "<MEDIA_MUTE>"
    of ccKeyMEDIA_WWW: "<MEDIA_WWW>"
    of ccKeyMEDIA_BACK: "<MEDIA_BACK>"
    of ccKeyMEDIA_FORWARD: "<MEDIA_FORWARD>"
    of ccKeyMEDIA_STOP: "<MEDIA_STOP>"
    of ccKeyMEDIA_FIND: "<MEDIA_FIND>"
    of ccKeyMEDIA_SCROLLUP: "<MEDIA_SCROLLUP>"
    of ccKeyMEDIA_SCROLLDOWN: "<MEDIA_SCROLLDOWN>"
    of ccKeyMEDIA_EDIT: "<MEDIA_EDIT>"
    of ccKeyMEDIA_SLEEP: "<MEDIA_SLEEP>"
    of ccKeyMEDIA_COFFEE: "<MEDIA_COFFEE>"
    of ccKeyMEDIA_REFRESH: "<MEDIA_REFRESH>"
    of ccKeyMEDIA_CALC: "<MEDIA_CALC>"

const reverseLookup: Table[string, KeyCode] =
  static:
    var tbl = initTable[string, KeyCode]()
    for code in disjointIter(KeyCode):
      tbl[getEmacsKeyName(code)] = code

    tbl

proc fromEmacsKeyName*(key: string): KeyCode =
  reverseLookup[key]

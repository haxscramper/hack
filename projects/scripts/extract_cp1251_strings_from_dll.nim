import unicode except strip
import parseutils, sugar, encodings, strutils, sequtils, strformat
import hmisc/other/[hshell, hjson]

#================================  ----  =================================#

import nativesockets, strutils
import httpclient
import asyncdispatch
import sequtils

proc r_core_new(): pointer {.importc, dynlib: "libr_core.so".}
proc r_core_cmd_str(
  c: pointer, cmd: cstring): cstring {.importc, dynlib: "libr_core.so".}

proc toString(str: seq[char]): string =
  result = newStringOfCap(len(str))
  for ch in str:
    add(result, ch)
  return result

proc fromString(str: string): seq[char] =
  return toSeq(str.items)

type R2PipeApi = ref object
  lib: pointer

proc cmd*(this: R2PipeApi, c: string): string =
  if this.lib == nil:
    this.lib = r_core_new()
  return $r_core_cmd_str(this.lib, c)

proc cmdj*(r: R2PipeApi, c: string): JsonNode =
  return parseJson(cmd(r, c))

#================================  ----  =================================#


func fromCP1251(num: int): string =
  case num:
  of 0: ""
  of 128: "Ђ" # CYRILLIC CAPITAL LETTER DJE
  of 129: "Ѓ" # CYRILLIC CAPITAL LETTER GJE
  of 130: "‚" # SINGLE LOW-9 QUOTATION MARK
  of 131: "ѓ" # CYRILLIC SMALL LETTER GJE
  of 132: "„" # DOUBLE LOW-9 QUOTATION MARK
  of 133: "…" # HORIZONTAL ELLIPSIS
  of 134: "†" # DAGGER
  of 135: "‡" # DOUBLE DAGGER
  of 136: "€" # EURO SIGN
  of 137: "‰" # PER MILLE SIGN
  of 138: "Љ" # CYRILLIC CAPITAL LETTER LJE
  of 139: "‹" # SINGLE LEFT-POINTING ANGLE QUOTATION MARK
  of 140: "Њ" # CYRILLIC CAPITAL LETTER NJE
  of 141: "Ќ" # CYRILLIC CAPITAL LETTER KJE
  of 142: "Ћ" # CYRILLIC CAPITAL LETTER TSHE
  of 143: "Џ" # CYRILLIC CAPITAL LETTER DZHE
  of 144: "ђ" # CYRILLIC SMALL LETTER DJE
  of 145: "‘" # LEFT SINGLE QUOTATION MARK
  of 146: "’" # RIGHT SINGLE QUOTATION MARK
  of 147: "“" # LEFT DOUBLE QUOTATION MARK
  of 148: "”" # RIGHT DOUBLE QUOTATION MARK
  of 149: "·" # BULLET
  of 150: "–" # EN DASH
  of 151: "—" # EM DASH
  of 152: " " # UNDEFINED
  of 153: "™" # TRADE MARK SIGN
  of 154: "љ" # CYRILLIC SMALL LETTER LJE
  of 155: "›" # SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
  of 156: "њ" # CYRILLIC SMALL LETTER NJE
  of 157: "ќ" # CYRILLIC SMALL LETTER KJE
  of 158: "ћ" # CYRILLIC SMALL LETTER TSHE
  of 159: "џ" # CYRILLIC SMALL LETTER DZHE
  of 160: " " # NO-BREAK SPACE
  of 161: "Ў" # CYRILLIC CAPITAL LETTER SHORT U
  of 162: "ў" # CYRILLIC SMALL LETTER SHORT U
  of 163: "Ј" # CYRILLIC CAPITAL LETTER JE
  of 164: "¤" # CURRENCY SIGN
  of 165: "Ґ" # CYRILLIC CAPITAL LETTER GHE WITH UPTURN
  of 166: "¦" # BROKEN BAR
  of 167: "§" # SECTION SIGN
  of 168: "Ё" # CYRILLIC CAPITAL LETTER IO
  of 169: "©" # COPYRIGHT SIGN
  of 170: "Є" # CYRILLIC CAPITAL LETTER UKRAINIAN IE
  of 171: "«" # LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
  of 172: "¬" # NOT SIGN
  of 173: " " # SOFT HYPHEN

  of 174: "®" # REGISTERED SIGN
  of 175: "Ї" # CYRILLIC CAPITAL LETTER YI
  of 176: "°" # DEGREE SIGN
  of 177: "±" # PLUS-MINUS SIGN
  of 178: "І" # CYRILLIC CAPITAL LETTER
  of 179: "і" # CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
  of 180: "ґ" # CYRILLIC SMALL LETTER GHE WITH UPTURN
  of 181: "µ" # MICRO SIGN
  of 182: "¶" # PILCROW SIGN
  of 183: "·" # MIDDLE DOT
  of 184: "ё" # CYRILLIC SMALL LETTER IO
  of 185: "№" # NUMERO SIGN
  of 186: "є" # CYRILLIC SMALL LETTER UKRAINIAN IE
  of 187: "»" # RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  of 188: "ј" # CYRILLIC SMALL LETTER JE
  of 189: "Ѕ" # CYRILLIC CAPITAL LETTER DZE
  of 190: "ѕ" # CYRILLIC SMALL LETTER DZE
  of 191: "ї" # CYRILLIC SMALL LETTER YI
  of 192: "А" # CYRILLIC CAPITAL LETTER A
  of 193: "Б" # CYRILLIC CAPITAL LETTER BE
  of 194: "В" # CYRILLIC CAPITAL LETTER VE
  of 195: "Г" # CYRILLIC CAPITAL LETTER GHE
  of 196: "Д" # CYRILLIC CAPITAL LETTER DE
  of 197: "Е" # CYRILLIC CAPITAL LETTER IE
  of 198: "Ж" # CYRILLIC CAPITAL LETTER ZHE
  of 199: "З" # CYRILLIC CAPITAL LETTER ZE
  of 200: "И" # CYRILLIC CAPITAL LETTER I
  of 201: "Й" # CYRILLIC CAPITAL LETTER SHORT I
  of 202: "К" # CYRILLIC CAPITAL LETTER KA
  of 203: "Л" # CYRILLIC CAPITAL LETTER EL
  of 204: "М" # CYRILLIC CAPITAL LETTER EM
  of 205: "Н" # CYRILLIC CAPITAL LETTER EN
  of 206: "О" # CYRILLIC CAPITAL LETTER O
  of 207: "П" # CYRILLIC CAPITAL LETTER PE
  of 208: "Р" # CYRILLIC CAPITAL LETTER ER
  of 209: "С" # CYRILLIC CAPITAL LETTER ES
  of 210: "Т" # CYRILLIC CAPITAL LETTER TE
  of 211: "У" # CYRILLIC CAPITAL LETTER U
  of 212: "Ф" # CYRILLIC CAPITAL LETTER EF
  of 213: "Х" # CYRILLIC CAPITAL LETTER HA
  of 214: "Ц" # CYRILLIC CAPITAL LETTER TSE
  of 215: "Ч" # CYRILLIC CAPITAL LETTER CHE
  of 216: "Ш" # CYRILLIC CAPITAL LETTER SHA
  of 217: "Щ" # CYRILLIC CAPITAL LETTER SHCHA
  of 218: "Ъ" # CYRILLIC CAPITAL LETTER HARD SIGN
  of 219: "Ы" # CYRILLIC CAPITAL LETTER YERU
  of 220: "Ь" # CYRILLIC CAPITAL LETTER SOFT SIGN
  of 221: "Э" # CYRILLIC CAPITAL LETTER E
  of 222: "Ю" # CYRILLIC CAPITAL LETTER YU
  of 223: "Я" # CYRILLIC CAPITAL LETTER YA
  of 224: "а" # CYRILLIC SMALL LETTER A
  of 225: "б" # CYRILLIC SMALL LETTER BE
  of 226: "в" # CYRILLIC SMALL LETTER VE
  of 227: "г" # CYRILLIC SMALL LETTER GHE
  of 228: "д" # CYRILLIC SMALL LETTER DE
  of 229: "е" # CYRILLIC SMALL LETTER IE
  of 230: "ж" # CYRILLIC SMALL LETTER ZHE
  of 231: "з" # CYRILLIC SMALL LETTER ZE
  of 232: "и" # CYRILLIC SMALL LETTER I
  of 233: "й" # CYRILLIC SMALL LETTER SHORT I
  of 234: "к" # CYRILLIC SMALL LETTER KA
  of 235: "л" # CYRILLIC SMALL LETTER EL
  of 236: "м" # CYRILLIC SMALL LETTER EM
  of 237: "н" # CYRILLIC SMALL LETTER EN

  of 238: "о" # CYRILLIC SMALL LETTER O
  of 239: "п" # CYRILLIC SMALL LETTER PE
  of 240: "р" # CYRILLIC SMALL LETTER ER
  of 241: "с" # CYRILLIC SMALL LETTER ES
  of 242: "т" # CYRILLIC SMALL LETTER TE
  of 243: "у" # CYRILLIC SMALL LETTER U
  of 244: "ф" # CYRILLIC SMALL LETTER EF
  of 245: "х" # CYRILLIC SMALL LETTER HA
  of 246: "ц" # CYRILLIC SMALL LETTER TSE
  of 247: "ч" # CYRILLIC SMALL LETTER CHE
  of 248: "ш" # CYRILLIC SMALL LETTER SHA
  of 249: "щ" # CYRILLIC SMALL LETTER SHCHA
  of 250: "ъ" # CYRILLIC SMALL LETTER HARD SIGN
  of 251: "ы" # CYRILLIC SMALL LETTER YERU
  of 252: "ь" # CYRILLIC SMALL LETTER SOFT SIGN
  of 253: "э" # CYRILLIC SMALL LETTER E
  of 254: "ю" # CYRILLIC SMALL LETTER YU
  of 255: "я" # CYRILLIC SMALL LETTER YA
  else:
    $char(num)

proc conv(instr: string): string =
  var idx = 0
  while idx < instr.len:
    if idx + 1 == instr.len:
      result &= instr[idx]
      break

    if instr[idx .. idx + 1] == r"\u":
      let buf = instr[idx + 2 .. idx + 5]
      idx += 6
      var num: int
      discard parseHex(buf, num)
      result &= fromCP1251(num)
    else:
      result &= instr[idx]
      idx += 1

#================================  ----  =================================#

const indll = "Comhlp.dll"

let j = runShell(&"rabin2 -zj {indll}").stdout.parseJson()

var buf: string

let r2p = R2PipeApi();
discard r2p.cmd(&"o {indll}")

for elem in j["strings"]:
  echo r2p.cmd("s " & $elem["vaddr"] & "; psu").conv()

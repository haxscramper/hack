import terminal, sequtils, strutils
export terminal

type
  MessageType* = enum
    mLog
    mInfo
    mWarn
    mError

  MessageStyle* = enum
    sDefault
    sVerbose
    sGtest
    sBright
    sLog

  ColoredString* = object
    str*: string
    fg*: ForegroundColor
    bg*: BackgroundColor
    style*: set[Style]


proc toRed*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgRed)

proc toGreen*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgGreen)

proc toYellow*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgYellow)

proc toWhite*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgWhite)

proc toCyan*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgCyan)

proc toMagenta*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgMagenta)

proc toDefault*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgDefault)

proc debug*(str: ColoredString) =
  echo "str: ", str.str, " fg: ",
     str.fg, " bg: ",
     str.bg, " style:",
     str.style

proc `$`*(colored: ColoredString): string =
  let fgCode = if colored.fg.int != 0:
      ansiForegroundColorCode(
        fg = colored.fg,
        bright = styleBright in colored.style)
    else:
      ""

  let bgCode = if colored.bg.int != 0:
      ansiStyleCode(
        colored.bg.int +
        (if styleBright in colored.style: 60 else: 0))
      else:
        ""

  toSeq(colored.style).mapIt(ansiStyleCode(it)).join &
    fgCode &
    bgCode &
    colored.str &
    ansiStyleCode(0)


proc len*(str: ColoredString): int = str.str.len

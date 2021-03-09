import types, terminal, options, sequtils, strutils, parseopt
import hmisc/helpers, os, times, math
import hmisc/termformat



const styleTable*: seq[
  tuple[
    k: seq[string],
    v: Style]] =
  @[
      (@["b", "bold", "styleBright"], styleBright),
      (@["u", "underline", "styleUnderline",
         "underscore", "styleUnderscore"],
       styleUnderscore),
      (@["i", "italic", "styleItalic"], styleItalic),
      (@["d", "dimmed", "styleDim", "dim"], styleDim),
      (@["r", "reversed", "styleReversed"], styleReverse),
      (@["f", "flicker", "blink", "styleBlink"], styleBlink),
      (@["h", "hidden", "styleHidden"], styleHidden),
      (@["s", "strike", "strikethrough", "styleStrikethough"],
       styleStrikethrough)
  ]

proc styleFromName*(styleStr: string): Option[Style] =
  matchWith[string, Style](
    val = styleStr, tbl = styleTable)


const fgColorTable*: seq[
    tuple[
      k: seq[string],
      v: ForegroundColor]] =
  @[
      (@["d", "black", "fgBlack"], fgBlack),
      (@["r", "red", "fgRed"], fgRed),
      (@["g", "green", "fgGreen"], fgGreen),
      (@["y", "yellow", "fgYellow"], fgYellow),
      (@["b", "blue", "fgBlue"], fgBlue),
      (@["m", "magenta", "fgMagent"], fgMagenta),
      (@["c", "cyan", "fgCyan"], fgCyan),
      (@["w", "white", "fgWhite"], fgWhite)
  ]

proc fgColorFromName*(colorStr: string): Option[ForegroundColor] =
  matchWith[string, ForegroundColor](
    val = colorStr, tbl = fgColorTable)

const bgColorTable*: seq[
  tuple[
    k: seq[string],
    v: BackgroundColor]] =
  @[
      (@["d", "black", "bgBlack"], bgBlack),
      (@["r", "red", "bgRed"], bgRed),
      (@["g", "green", "bgGreen"], bgGreen),
      (@["y", "yellow", "bgYellow"], bgYellow),
      (@["b", "blue", "bgBlue"], bgBlue),
      (@["m", "magenta", "bgMagent"], bgMagenta),
      (@["c", "cyan", "bgCyan"], bgCyan),
      (@["w", "white", "bgWhite"], bgWhite)
  ]

proc bgColorFromName*(colorStr: string): Option[BackgroundColor] =
  matchWith[string, BackgroundColor](
    val = colorStr, tbl = bgColorTable)

proc getPrefixString*(
  messageType: MessageType,
  level: int,
  style: MessageStyle): string =
    defer:
      if style == sLog:
        result = $(now().format("[HH:mm:ss]")) & result

    result = case messageType:
      of mLog:
        case level:
        of 0:
          case style:
          of sDefault : "-  "
          of sVerbose : "Log 0"
          of sGtest   : "[          ]"
          of sLog  : "[L0]"
          of sBright  : "---"
        of 1:
          case style:
          of sDefault : "---"
          of sVerbose : "Log 1"
          of sGtest   : "[----------]"
          of sLog  : "[L1]"
          of sBright  : "---"
        of 2:
          case style:
          of sDefault : "***"
          of sVerbose : "Log 2"
          of sGtest   : "[**********]"
          of sLog  : "[L2]"
          of sBright  : "---"
        of 3:
          case style:
          of sDefault : ">>>"
          of sVerbose : "Log 3"
          of sGtest   : "[>>>>>>>>>>]"
          of sLog  : "[L3]"
          of sBright  : "---"
        else: ""
      of mInfo:
        case level
        of 0:
          case style
          of sDefault : "  >"
          of sVerbose : "Inf 0"
          of sGtest : "[->->->->->]"
          of sLog : "[I0]"
          of sBright : "~~~"
        of 1:
          case style
          of sDefault : " ->"
          of sVerbose : "Inf 1"
          of sGtest : "[->->->->->]"
          of sLog : "[I1]"
          of sBright : "~~~"
        of 2:
          case style
          of sDefault : "-->"
          of sVerbose : "Inf 2"
          of sGtest : "[->>->>->>-]"
          of sLog : "[I2]"
          of sBright : "III"
        of 3:
          case style
          of sDefault : "-->"
          of sVerbose : "Inf 3"
          of sGtest : "[->>->>->>-]"
          of sLog : "[I3]"
          of sBright : "III"
        else : ""
      of mWarn:
        case level
        of 0:
          case style
          of sDefault : "==>"
          of sVerbose : "Wrn 0"
          of sGtest : "[=>>=>>=>>=]"
          of sLog : "[W0]"
          of sBright : ">>>"
        of 1:
          case style
          of sDefault : "==>"
          of sVerbose : "Wrn 1"
          of sGtest : "[=>>=>>=>>=]"
          of sLog : "[W1]"
          of sBright : ">>>"
        of 2:
          case style
          of sDefault : "=>>"
          of sVerbose : "Wrn 2"
          of sGtest : "[=>>=>>=>>=]"
          of sLog : "[W2]"
          of sBright : "WWW"
        of 3:
          case style
          of sDefault : "=>>"
          of sVerbose : "Wrn 3"
          of sGtest : "[=>>=>>=>>=]"
          of sLog : "[W3]"
          of sBright : "WWW"
        else : ""
      of mError:
        case level
        of 0:
          case style
          of sDefault : "###"
          of sVerbose : "Err 0"
          of sGtest :   "[##########]"
          of sLog :  "[E0]"
          of sBright :  "!!!"
        of 1:
          case style
          of sDefault : "!!!"
          of sVerbose : "Err 1"
          of sGtest :   "[!!!!!!!!!!]"
          of sLog :  "[E1]"
          of sBright :  "!!!"
        of 2:
          case style
          of sDefault : "!!!"
          of sVerbose : "Err 2"
          of sGtest :   "[!!!!!!!!!!]"
          of sLog :  "[E2]"
          of sBright :  "EEE"
        of 3:
          case style
          of sDefault : "!!!"
          of sVerbose : "Err 3"
          of sGtest :   "[!!!!!!!!!!]"
          of sLog :  "[E3]"
          of sBright :  "EEE"
        else :          ""

proc getStyle*(
  messageType: MessageType,
  messageLevel: int): tuple[
    fg: ForegroundColor,
    bg: BackgroundColor,
    styles: set[Style]] =
    let noStyle: set[Style] = {}
    return case messageType
      of mLog: (fgDefault,  bgDefault, noStyle)
      of mInfo:
        case messageLevel:
          of 0: (fgGreen,   bgDefault, {styleDim})
          of 1: (fgBlue,    bgDefault, {styleDim})
          of 2: (fgBlue,  bgDefault, noStyle)
          of 3: (fgCyan,     bgDefault, noStyle)
          else: (fgDefault, bgDefault, noStyle)
      of mWarn:
        case messageLevel
          of 0: (fgYellow,   bgDefault, {styleBlink, styleDim})
          of 1: (fgYellow,  bgDefault, {styleBlink, styleBright})
          of 2: (fgMagenta,  bgDefault, {styleBlink, styleDim})
          of 3: (fgMagenta,     bgDefault, {styleBlink, styleBright})
          else: (fgDefault, bgDefault, noStyle)
      of mError:
        case messageLevel
          of 0: (fgGreen,   bgDefault, {styleBright, styleBlink})
          of 1: (fgWhite,   bgDefault, {styleBlink,  styleReverse})
          of 2: (fgYellow,  bgDefault, {styleBlink,  styleReverse})
          of 3: (fgRed,     bgDefault, {styleBlink,  styleReverse})
          else: (fgDefault, bgDefault, noStyle)


proc getPrefix*(
  mtype: MessageType = mLog,
  level: int = 0,
  style: MessageStyle = sVerbose):
    ColoredString =

  result.str = getPrefixString(mtype, level, style)
  let (fg, bg, st)= getStyle(mtype, level)

  if style != sLog:
    result.fg = fg
    result.bg = bg
    result.style = st

proc getPrefix*(
  mtype: tuple[t: MessageType, l: int],
  style: MessageStyle): ColoredString =
    getPrefix(
      mtype = mtype.t,
      level = mtype.l,
      style = style)

proc printString*(str: string, addNewline: bool = true): void =
  stdout.write(str & (if addNewline: "\n" else: ""))

proc printAllStyles*(): void =
  for msgStyle in sDefault..sBright:
    echo msgStyle
    for msgType in MessageType:
      stdout.write("  ")
      for level in 0..3:
        let str = getPrefix(msgType, level, msgStyle)
        printString($str, level == 3)
        if level != 3:
          stdout.write("  ")


const prefixStyleTable:
  seq[tuple[
    k: seq[string],
    v: MessageStyle]] =
    @[
      (@["g", "gtest"], sGtest)
    ]

proc prefixStyleFromName*(str: string): Option[MessageStyle] =
  matchWith[string, MessageStyle](
    val = str, tbl = prefixStyleTable)

proc printAllArgs*(): void =
  echo "# parameters: ", paramCount()
  for ii in 1 .. paramCount():    # 1st param is at index 1
    echo "param ", ii, ": ", paramStr(ii)

  echo ""
  # Using parseopt module to extract short and long options and arguments
  for kind, key, value in getOpt():
    case kind
    of cmdArgument:
      echo "Got arg ", ": \"", key, "\""
    of cmdLongOption, cmdShortOption:
      echo "Got a \"", key, "\" option with value: \"", value, "\""
    of cmdEnd:
      discard


proc printLine*(
  lprefix: ColoredString,
  line: string | ColoredString,
  ellipis: bool = false,
  indentSize: int = 0,
  prefixSpacing: string = " ",
  idx: int = 0
         ): void =
    var prefix = lprefix
    prefix.str = if idx == 0:
                   prefix.str
                 else:
                   " ".repeat(max((prefix.str.len) - 4, 0)) &
                     ".".repeat(3)


    echo "$#$#$#$#" % [
      " ".repeat(indentSize),
      $prefix,
      $prefixSpacing,
      $line
    ]


proc ceWrite(
  str: string,
  mtype: MessageType,
  level: int,
  style: MessageStyle,
  indent: int,
  doReflow: bool = true): void =
    let prefix = getPrefix(mtype, level, style)
    if doReflow:
      for idx, line in str.justifyFitTerminal(padding = (prefix.len + indent, 2)):
        printLine(prefix, line, idx != 0, indent, idx = idx)
    else:
      printLine(prefix, str, 0 != 0, indent, idx = 0)

proc ceError*(
  str: string, lv: int = 0, st: MessageStyle = sDefault,
  ind: int = 0): void =
    ceWrite(str = str,
      mtype = mError, level = lv, style = st, indent = ind)

proc ceLog*(
  str: string, lv: int = 0, st: MessageStyle = sDefault,
  ind: int = 0): void =
    ceWrite(str = str,
            mtype = mLog, level = lv, style = st, indent = ind)

proc ceWarn*(
  str: string, lv: int = 0, st: MessageStyle = sDefault,
  ind: int = 0): void =
    ceWrite(str = str,
            mtype = mWarn, level = lv, style = st, indent = ind)

proc ceInfo*(
  str: string, lv: int = 0, st: MessageStyle = sDefault,
  ind: int = 0): void =
    ceWrite(str = str,
            mtype = mInfo, level = lv, style = st, indent = ind)

# TODO DOC

proc ceUserWarn*(str: string, ind = 0, doReflow: bool = true): void =
  ## Use when stopping program due to expected reasons (for example
  ## when user is prompted Y/n to create file and chooses no. In that
  ## case you need to issue a warning that no files were created, but
  ## otherwise this is expected situation)
  ceWrite(str = str, mtype = mWarn, level = 1, style = sBright, indent = ind, doReflow = doReflow)

proc ceUserInfo0*(str: string, ind = 0, doReflow: bool = true): void =
  ceWrite(str = str, mtype = mInfo, level = 0, style = sBright, indent = ind, doReflow = doReflow)

proc ceUserInfo2*(str: string, ind = 0, doReflow: bool = true): void =
  ceWrite(str = str, mtype = mInfo, level = 2, style = sDefault, indent = ind, doReflow = doReflow)

proc ceUserLog0*(str: string, ind = 0, doReflow: bool = true): void =
  ## Fancier echo, nothing useful
  ceWrite(str = str, mtype = mLog, level = 0, style = sDefault, indent = ind, doReflow = doReflow)

proc ceUserError0*(str: string, ind: int = 0, doReflow: bool = true): void =
  ## When multiple errors of that type are expected. For example when
  ## parsing configuration file and discarding several options due to
  ## broken configuraion.
  ceWrite(str = str, mtype = mError, level = 0, style = sBright, indent = ind, doReflow = doReflow)

proc printSeparator(typ: string = "upper"): void =
  case typ:
    of "upper": echo "--> <--\n"
    of "lower": echo "\n--> <--"
    else: echo "--- ---"


when isMainModule:
  for st in sDefault..sVerbose:
    for typ in MessageType:
      echo "------------"
      for l in 0..3:
        ceWrite(
          str = $st & " " & $typ & " " & $l,
          level = l,
          mtype = typ,
          style = st,
          indent = 0)

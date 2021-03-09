import terminal, times, sequtils, strutils, options, parseopt, tables, os, math
import macros

import hmisc/termformat
import hmisc/helpers
import hargparse
import colechopkg/types
import colechopkg/lib
import strformat

parseArgs:
#~#== Color style
  opt:
    name: "use-error"
    opt: ["-e", "--error", "+takes_value"]
    takes: [0, 1, 2, 3]
    default: "0"
    help: "Use error style"
    parseto: int
  opt:
    name: "use-info"
    opt: ["-i", "--info", "+takes_value"]
    takes: [0, 1, 2, 3]
    default: "0"
    help: "Use info style"
    parseto: int
  opt:
    name: "use-log"
    opt: ["-l", "--log", "+takes_value"]
    takes: [0, 1, 2, 3]
    default: "0"
    help: "Use log style"
    parseto: int
  opt:
    name: "use-warn"
    opt: ["-w", "--warn", "+takes_value"]
    takes: [0, 1, 2, 3]
    help: "Use warn style"
    default: "0"
    parseto: int
#~#== Prefix styles
  opt:
    name: "style-gtest"
    opt: ["-g", "--gtest"]
    help: "Use gtest style prefix"
  opt:
    name: "style-log"
    opt: ["-L", "--Log"]
    help: "Use logging style prefix"
  opt:
    name: "style-verbose"
    opt: ["-v", "--verbose"]
    help: "Use verbose style prefix"
  opt:
    name: "style-bright"
    opt: ["-b", "--bright"]
    help: "Use bright style prefix"
#~#== Misc options
  opt:
    name: "style-opts"
    opt: ["-s", "--style", "+takes_value"]
    takes: styleTable.mapIt(it.k).concat
    help: "Additional styling options"
  opt:
    name: "indent"
    opt: ["-I", "--indent", "+takes_value"]
    takes: 0..16
    help: "Set indentation level"
    parseto: int
  opt:
    name: "bg-col"
    opt: ["-b", "--background", "--bg-col", "+takes_value"]
    takes: bgColorTable.mapIt(it.k).concat
    help: "Set background color"
  opt:
    name: "fg-col"
    opt: ["-f", "--foreground", "--fg-col", "+takes_value"]
    takes: fgColorTable.mapIt(it.k).concat
    help: "Set foreground color"
  opt:
    name: "rtrim-prefix"
    opt: ["--rtrim", "--rtr", "+takes_value"]
    # TODO add support for verbatim help messages that would not be
    # altered
    help: """
Show only <n> last characters in prefix. With `--rtr:2` instead
`[ ERROR 0 ]:` you will get `         ]:` with the same color.
"""
    parseto: int # TODO show <n> in help message summary
    default: "3"
  opt:
    name: "style-uniform"
    opt: ["-u", "--uniform"]
    help: "Use the same color scheme for prefix and message"
  opt:
    name: "prefix"
    opt: ["-p", "--prefix"]
    help: """
Prefix displayed in log message before all other text. Should be used
for differentiating between different outputs in `log` style. In other
modes it is prepended to the actual message as-is. In `log` style it
is left-justified to at lest 12 characters and wrapped in parenthesis.
"""

if hasErrors:
  quit(1)


if "get-help".kp:
  cmdPrintHelp(helpTable)
  quit(0)

let prefStyle: MessageStyle =
  if "style-gtest".kp:
    sGtest
  elif "style-log".kp:
    sLog
  elif "style-bright".kp:
    sBright
  elif "style-verbose".kp:
    sVerbose
  else:
    sDefault

# FIXME I still have to manually type strings twice! Need to find some
# way to eliminate this. Maybe inject `it` in the scope when checking
# for key presence?
let mtype: tuple[t: MessageType, l: int] =
  if "use-info".kp:
    (mInfo, parseTo[int]("use-info".k))
  elif "use-log".kp:
    (mLog, parseTo[int]("use-log".k))
  elif "use-warn".kp:
    (mWarn, parseTo[int]("use-warn".k))
  elif "use-error".kp:
    (mError, parseTo[int]("use-error".k))
  else:
    (mLog, 0)

let indent =
  if "indent".kp and prefStyle != sLog:
    parseTo[int]("indent".k)
  else:
    0


var prefix = getPrefix(mtype, prefStyle)


if prefStyle == sLog:
  let indentOpt =
    if "indent".kp: parseTo[int]("indent".k)
    else: 0

  if indentOpt != 0:
    prefix.str &= "[$#]" % ($indentOpt).alignLeft(2, '_')

if "rtrim-prefix".kp and prefStyle != sLog:
  prefix.str = prefix.str.replaceN(
    prefix.str.len -
    parseTo[int]("rtrim-prefix".k))

proc len(str: ColoredString): int = str.str.len

let progPrefix =
  if prefStyle == sLog and "prefix".kp:
    let tmp = "prefix".k.toStr()

    if tmp.len > 12: tmp
    else: tmp.alignLeft(12, '.')

  else: ""

let message = argParsed
  .join(" ")
  .justifyFitTerminal((prefix.len + indent + 1 + progPrefix.len, 0))

for idx, line in message:
  if "style-uniform".kp:
    printLine(
      prefix,
      ColoredString(
        str: line,
        fg: prefix.fg,
        bg: prefix.bg,
        style: prefix.style),
      idx != 0,
      indent)
  else:
    if prefStyle == sLog and "prefix".kp:
      printLine(prefix, line, idx != 0, indent, &"({progPrefix}) ")
    else:
      printLine(prefix, line, idx != 0, indent)

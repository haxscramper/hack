import parseopt, macros, sequtils, strutils, strformat, tables, options, os
import math
import terminal
import algorithm

import hmisc/strparser
import hmisc/termformat
import hmisc/helpers

export macros
export tables

#~#==== Type definitions
type
  CmdArgKind* = enum
    cmdArgFlag
    cmdArgOpt
    cmdArgArg

  CmdArg* = object
    case kind*: CmdArgKind:
      of cmdArgFlag:
        count*: int
      of cmdArgOpt, cmdArgArg:
        vals*: seq[string]

  CmdParseResults* = object
    table*: Table[string, CmdArg]

macro ternary(predicate, body: untyped): untyped =
  echo treeRepr(predicate)
  echo treeRepr(body)

  var tClause: NimNode
  var fClause: NimNode

  for node in body:
    if node[0] == ident"t":
      tClause = node[1]
    else:
      fClause = node[1]

  nnkStmtList.newTree(
    nnkIfStmt.newTree(
      nnkElifBranch.newTree(predicate, tClause),
      nnkElse.newTree(fClause)))

type
  ParseOptionError* = enum
    errCannotParse
    errTooMuchValues
    errMissingArgument

  OptionHelp* = object
    flags*: seq[string]
    types*: Option[string]
    messg*: string
    takesVal*: bool

#~#=== Helper macro


proc toInt*(val: CmdArg): int = parseInt(val.vals[0])
proc toStr*(val: CmdArg): string = val.vals[0]
proc toBool*(val: CmdArg): bool = val.vals[0].toBool()

proc cmdParseTo*(val: string, t: string): Option[string] =
  some(val)

proc cmdParseTo*(str: string, t: int): Option[int] =
  try:
    some(parseInt(str))
  except ValueError:
    none(int)



proc parseTo*[int](val: CmdArg): int =
  parseInt(val.vals[0])

# TODO generate this functions automatically based on cmdParseTo


#~#=== Error reporting

# TODO @idea: use colecho pretty-printing for colorizing error
# messages. Add support for writing mixed sequence of strings and
# colored strings (write echo wrapper). Simple rst-based message
# coloring? In case I need to write manpage of provide more colorul
# help message (for example underline each expected value in list);
proc writeErrors(
  err: ParseOptionError,
  help: string,
  optName: string,
  msg: varargs[string, `$`]):
    void =

  proc mecho(id: string, padTo: int, m: seq[string]): void =
    echo id.alignLeft(padTo, '_'), ": ", m[0]
    if m.len > 1:
      echo m[1..^1].mapIt(" ".repeat(padTo + 2) & it).join("\n")


  let msgWidth = 10
  echo ""
  mecho("error", msgWidth, @[$err])
  mecho("parsing", msgWidth, @[optName])
  mecho(
    "opt_help",
    msgWidth,
    help.justifyFitTerminal((msgWidth, 10)))
  mecho(
    "message",
    msgWidth,
    msg.join(" ").justifyFitTerminal((msgWidth, 10)))


#~#=== Helper procs
#~#== Check if item is allowed
proc isPresentIn[T](what: T, where: openArray[T]): bool =
  return where.find(what) != -1

proc isPresentIn[T](what: T, where: HSlice[T, T]): bool =
  return what in where

#~#== Convert to debug strings
proc toDebugString[T](what: openArray[T]): string =
  return what.mapIt("'" & $it & "'").join(", ")

proc toDebugString[T](what: T): string =
  return $what


proc getKeysCompare*(opts: seq[string], key = "key"): NimNode =
  ## Generate `((key == "val1" or key == "val2") or key == "val3")
  ## ...` for sequence of values
  opts
  .filterIt(
    it.startsWith("-") or
    it.startsWith("--"))
  .mapIt(
    it.strip(
      trailing = false,
      chars = {'-'}))
  .mapIt(
    infix(
      ident(key), "==", newStrLitNode(it)))
  .foldl(infix(a, "or", b))


#~#=== Error checking generation
proc getDefaultValueChecker*(options: Table[string, NimNode]): NimNode =
  ## Generate code for checking whether or not value is supplied and
  ## providing default value if this needed and allowed
  if options.hasKey("default"):
    let defaultVal = options["default"]
    quote do:
      let val1 {.inject.} = if val.len == 0:
                              `defaultVal`
                            else:
                              val
  else:
    quote do:
      let val1 {.inject.} = val

proc getMissingValueChecker*(
  options: Table[string, NimNode]): NimNode =
  ## Generate code for checking for missining value. If "+takes_value"
  ## is present in list of options checking code will be generated,
  ## otherwise empty node will be returned.
  # TODO check for name
  let identifierExpr: NimNode = options["name"]
  # TODO Check for "opt" and generate name from "name" if options are
  # missing
  let optOptions: seq[string] = options["opt"].mapIt(it.strVal)
  if "+takes_value" in optOptions:
    quote do:
      if val1.len == 0:
        let dashPref = "--"
          # case kind:
          #   of cmdShortOption: "-"
          #   else: "--"

        writeErrors(
          err = errCannotParse,
          help = helpMessage,
          optName = `identifierExpr`,
          "Missing value for option {", key,
          "}. To pass value use ",
          dashPref & key & ":<val> or " & dashPref & key & "=<val>")

        continue
  else:
    nnkEmpty.newNimNode()

proc getParseErrorChecker*(
  options: Table[string, NimNode]): NimNode =
  ##[

Generate pars error checking code. If options contain key "parseto"
generate call to parsing function and check for it's return value. If
no such key is present empty `Option[T]` generation will be returned.
To parse code proc `cmdParseTo(str, T): Option[T]` is used. Second
argument is used only to distinguis between multiple overloads of the
function and it's type should be equal to the type of the returned
value. Call to parsing function is not wrapped in try/catch block and
success of parsing is checked **only** based on presence/absence of
the value in resulted `Option[T]`.

For example, if you want to have an option that is parsed to
`int` you might want to define following proc:

.. code-block:: nim
  proc cmdParseTo*(str: string, t: int): Option[int] =
    try:
      some(parseInt(str))
    except ValueError:
      none(int)

  ]##




  if options.hasKey("parseto"):
    let parseToType = options["parseto"]
    let callParser =
          nnkCall.newTree(
            nnkBracketExpr.newTree(
              newIdentNode("cmdParseTo"),
              parseToType
            ), ident"val1")

    # TODO more meaningful error messages if parser function is not
    # present

    let identifierExpr = options["name"]
    let typeStr = newStrLitNode(parseToType.strVal)

    result = quote do:
      var tmp: `parseToType`

      let parsedOpt {.inject.}: Option[`parseToType`] =
        cmdParseTo(val1, tmp)

      if parsedOpt.isNone() and val1.len != 0:
        hasErrors = true
        if val1.len != 0:
          writeErrors(
            err = errCannotParse,
            help = helpMessage,
            optName = `identifierExpr`,
            "Argument", key, "has unexpected value {", val1,
            "} and cannot be parsed to", `typeStr`)
        else:
          writeErrors(
            err = errCannotParse,
            help = helpMessage,
            optName = `identifierExpr`,
            "Argument", key, "is missing value")
      elif parsedOpt.isNone() and val1.len == 0:


        continue
  else:
    result = quote do:
          let parsedOpt {.inject.}: Option[string] = some(val)

proc getAllowedValuesChecker(
  options: Table[string, NimNode]): NimNode =
  let identifierExpr = options["name"]

  if options.hasKey("takes"):
    let takesExpr = options["takes"]
    result = quote do:
      let takes = `takesExpr`
      if not isPresentIn(parsedOpt.get(), takes):
        hasErrors = true
        writeErrors(
          err = errCannotParse,
          help = helpMessage,
          optName = `identifierExpr`,
          "Argument", key,
                    "has unexpected value: {", val1,
                    "}. Allowed values are [", toDebugString(takes),
                    "]")
        continue
  else:
    result = nnkEmpty.newNimNode()


proc getOnSuccessAction(options: Table[string, NimNode]): NimNode =
  let identifierExpr = options["name"]
  quote do:
    let identifier = `identifierExpr`
    # echo "Adding ", identifier
    optParsed[identifier] = CmdArg(kind: cmdArgOpt, vals: @[val1])


proc startsWith(s: string, prefixes: openArray[string]): bool =
  for pref in prefixes:
    if s.startsWith(pref):
      return true

  return false

#~#=== Default options
proc getTypeSigil(str: Option[string]): string =
  if str.isNone():
    result = "v"
  else:
    result = case str.get():
      of "string": "s"
      of "int": "n"
      else: "v"

  result = "<" & result & ">"

proc getHelpOptionParser(): NimNode =
  var helpGenerator = quote do:
    optParsed["get-help"] = CmdArg(kind: cmdArgFlag, count: 1)

  result = nnkElifBranch.newTree(
    @["-h", "--help", "-?"].getKeysCompare(),
    helpGenerator)

proc cmdPrintHelp*(
  helpTable: Table[string, OptionHelp],
  maxHelpWidth: int = 80): void =
  # TODO write in more functional style
  var optionEntry: seq[
    tuple[flags: seq[string], help: OptionHelp]]

  for name, opt in helpTable:
    var flags: seq[string] = opt.flags .filterIt(it.startsWith(["-", "--"]))
    flags = flags.sortedByIt(it.len)
    if opt.takesVal:
      flags[^1] = flags[^1] & ":" & getTypeSigil(opt.types)

    optionEntry.add((flags: flags, help: opt))

  # Longest set of options
  let maxWidth: int = optionEntry .mapIt(
    it.flags .mapIt(it.len + 1) .sum()) .max()

  # Longest single option
  let minWidth: int = optionEntry .mapIt(
    it.flags .mapIt(it.len) .max) .max

  # Total width of flag column
  let flagColWidth =
    max(
      minWidth, # At least this wide
      min( # Maybe more, but no more than 1/3 of the screen
    @[maxWidth,
    toInt(terminalWidth() / 3),
    toInt((maxHelpWidth - 1) / 3)]))

  let totalWidth = min(terminalWidth(), maxHelpWidth)

  let final = optionEntry
  .mapIt(
    (it.flags.join(" ")
       .justifyFitTerminal(maxWidth = flagColWidth),
     it.help.messg.replace("\n", " ")
        .justifyFitTerminal(
          maxWidth = totalWidth - flagColWidth - 4)))

  # TODO compute help message lenght after justification of the flags
  # section to minimize wasted spacing

  # TODO store flags not in hashtable but in sequence of tuples to
  # preserve order of insertions when displaying help messages

  # TODO argument groups (in help generations)
  for entry in final:
    let flag = entry[0]
    let help = entry[1]
    for line in 0 ..< max(flag.len, help.len):
      echo " $# $#" % [
        if line < flag.len:
          flag[line].alignLeft(flagColWidth)
        else: " ".repeat(flagColWidth),
        if line < help.len:
          help[line]
        else: " ".repeat(flagColWidth)
      ]

    if flag.len != 1 or help.len != 1:
      echo ""

proc getHelpTable*(body: NimNode): Table[string,OptionHelp] {.compileTime.} =
  for arg in body:
    if arg[0] == ident"opt" and arg[1].kind == nnkStmtList:

      var options: Table[string, NimNode]
      # TODO `mapIt` to table
      for tpl in arg[1]:
        let identName: string = tpl[0].strVal
        options[identName] = tpl[1][0]

      let optOptions = options["opt"].mapIt(it.strVal)
      let name: string = options["name"].strVal()
      result[name] = OptionHelp(
        flags: optOptions,
        types: if options.hasKey("parseto"):
                 some(options["parseto"].strVal)
               else: none(string),
                 # TODO account for help-generating procs
        messg: options["help"].strVal,
        takesVal: "+takes_value" in optOptions)



#~#=== Main parser generation
proc getOptionParserBranch*(optConfig: NimNode): NimNode =
  ## Generate new `Elif` branch for catching option described by
  ## `optConfig`
  var options: Table[string, NimNode]
  for tpl in optConfig:
    let identName: string = tpl[0].strVal
    options[identName] = tpl[1][0]


  let helpMessageExpr = options["help"]

  var optParser = quoteDoInterpolStmt:
    let helpMessage {.inject.} = `helpMessageExpr`
    `"getDefaultValueChecker(options)"`

    `"getMissingValueChecker(options)"`
    `"getParseErrorChecker(options)"`
    `"getAllowedValuesChecker(options)"`

    `"getOnSuccessAction(options)"`

  let optFlags: seq[string] = options["opt"].mapIt(it.strVal)
  result = newTree(
    nnkElifBranch, optFlags.getKeysCompare(), optParser)

proc getArgumentParserCase(): NimNode =
  quote do:
    argParsed.add(key)

proc getEndParserCase(): NimNode =
  quote do:
    echo "End"

proc getOptionParserCase(body: NimNode): NimNode =
  ## Get code for processing short and long options. Generate
  ## top-level if statement and one elif branch for each option entry
  ## in body.
  var ifKeyStmt = newNimNode(nnkIfStmt)
  for arg in body:
    if arg[0] == ident"opt" and arg[1].kind == nnkStmtList:
      ifKeyStmt.add(getOptionParserBranch(arg[1]))

  ifKeyStmt.add(getHelpOptionParser())

  # Add top-level if statement
  result = ifKeyStmt


macro parseArgs*(body: untyped): untyped =
  let optParserCase = getOptionParserCase(body)
  let argParserCase = getArgumentParserCase()
  let endParserCase = getEndParserCase()

  # TODO this is very hacky solution that needs to be replaced with
  # something more elegant. Right now I don't really know how to do
  # this.
  let bodyNodeGen: NimNode = body.astGenRepr().parseExpr()

  result = quote do:
    const helpTable {.inject.} = getHelpTable(`bodyNodeGen`)

    var
      optParsed {.inject.}: Table[string, CmdArg]
      argParsed {.inject.}: seq[string]
      hasErrors {.inject.}: bool = false
      foundDoubleDash: bool = false

    for kind {.inject.}, key {.inject.}, val {.inject.} in getOpt():
      if key == "" and val == "":
        foundDoubleDash = true
        continue

      if foundDoubleDash:
        let prefix =
          if kind == cmdShortOption: "-"
          elif kind == cmdLongOption: "--"
          else: ""

        argParsed.add(prefix & key & val)
        continue

      # Insert top-level case for argument kind
      case kind
        of cmdShortOption, cmdLongOption:
          `optParserCase`
        of cmdArgument:
          `argParserCase`
        of cmdEnd:
          `endParserCase`


    if optParsed.hasKey("get-help"):
      cmdPrintHelp(helpTable)
      quit(0)

  "aragparse_loop.nim.tmp".writeFile(
    $toStrLit(result))

# TODO support for passing variadic arguments as well as arguments
# that require multiple values
template k*(key: string): CmdArg = optParsed[key]
template kp*(key: string): bool = optParsed.hasKey(key)

when isMainModule:
  echo "Running argparse.nim tests"

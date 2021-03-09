import tables
import macros
import hargparse
import hmisc/helpers
import hmisc/strparser

type
  OperationMode* = enum
    watchChanges
    doRunDev
    createFiles
    runSingleBuild
    defaultMode
    debugDump

  CmdParsed* = object
    optParsed*: Table[string, CmdArg]
    argParsed*: seq[string]
    maxRepeat*: Opt[int]
    waitUtil*: bool
    verbose*: bool
    case kind*: OperationMode:
      of doRunDev:
        targetFile*: string
      of watchChanges:
        targetFiles*: seq[string]
      of createFiles:
        testShName*: string
        fileToCreate*: string
      of runSingleBuild:
        buildUname*: string
        newFile*: string
      of defaultMode, debugDump:
        nil


proc parseCMDLine*(): CmdParsed =
  parseArgs:
    opt:
      name: "create-test"
      opt: ["--create"]
      help: "Generate test.sh or update if already exits"
    opt:
      name: "listen"
      opt: ["--listen"]
      help: "Listen for changes and emit them"
    opt:
      name: "uname"
      opt: ["--uname", "+takes_value"]
      help: "Unique name of build config"
    opt:
      name: "verbose-mode"
      opt: ["--verbose", "-v"]
      help: "Parse configuration verbosely"
    opt:
      name: "help"
      opt: ["--help", "-h", "-?"]
      help: "Print help message"

  if "help".kp():
    cmdPrintHelp(helpTable)

  result =
    if argParsed.len >= 2 and argParsed[0] == "dev":
      CmdParsed(kind: doRunDev, targetFile: argParsed[1])

    elif argParsed.len >= 1 and argParsed[0] == "debug":
      CmdParsed(kind: debugDump)

    elif argParsed.len >= 1 and argParsed[0] == "watch":
      CmdParsed(
        kind: watchChanges,
        targetFiles: argParsed[1].toStrSeq())
    elif argParsed.len >= 1 and argParsed[0] == "build":
      CmdParsed(
        kind: runSingleBuild,
        buildUname: "uname".k.toStr())
    elif "create-test".kp:
      let files: (string,string) = toTuple[string]("create-test".k.toStr())
      #files[1].writeFile(generateTestScript(files[0]))
      CmdParsed(
        kind: createFiles,
        testShName: files[1],
        fileToCreate: files[0])
    else:
      CmdParsed(kind: defaultMode)

  if "test".kp:
    result.maxRepeat = "test".k.toInt()

  result.verbose = "verbose-mode".kp()

  echo result

  result.waitUtil =
    if "wait-util".kp:
      "wait-util".k.toBool()
    else: false

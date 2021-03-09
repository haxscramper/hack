import parsetoml, parseutils, os
import sequtils, strformat, strutils
import hmisc/[defensive, helpers]
import times
import tables

initDefense()

type
  MissingConfError = ref object of CatchableError
    key: string

proc expand(
  instr: string,
  context: TableRef[string, string] = newTable[string, string]()
     ): string =
  for t, value in interpolatedFragments(instr):
    case t:
      of ikStr, ikDollar: result &= value
      of ikVar, ikExpr:
        case value:
          of "date": result &= now().format("yyyy-MM-dd")
          else:
            if value in context:
              result &= context[value]
            else:
              result &= getEnv(value)


proc getConf(conf: TomlValueRef, name: string): TomlValueRef =
  if not conf.hasKey(name):
    raise MissingConfError(
      msg: &"Configuration file is missing {name}",
      key: name
    )
  else:
    conf[name]


proc getConf(conf: TomlValueRef, name: string, default: string): string =
  if not conf.hasKey(name): default
  else: conf[name].getStr()

type
  FileAction = enum
    faRemove
    faMove

  MoveConfig = object
    kind: FileAction
    destination: string
    suffix: seq[string]

proc main() =
  let confpath = "~/.config/hax-config/config/indir-cleaner.toml".expandTilde()
  showInfo("Configuration file is", confpath)

  let config = confpath.parseFile()

  let inDir = config.getConf("input_dir").getStr().expand()
  let outDir = config.getConf("output_dir").getStr().expand()
  let suffix = config.getConf("suffix").getStr().expand()

  showInfo("Default input dir:", inDir)
  showInfo("Output dir:", outDir)
  showInfo("Dir suffix: ", suffix)

  var actions: seq[MoveConfig]
  for conf in config.getConf("pattern").getElems():
    actions.add MoveConfig(
      destination:
        joinpath(
          conf.hasKey("output_dir").tern(
            conf["output_dir"].getStr().expand({
              "samedir" : inDir
            }.newTable()),
            outDir
          ),
          conf.getConf("dir_prefix", ""),
          suffix
        ),
      kind: conf.hasKey("action").tern(
        (conf["action"].getStr() == "remove").tern(faRemove, faMove),
        faMove
      ),
      suffix: conf.getConf("file_suffix").getElems().mapIt(it.getStr)
    )

  for kind, file in inDir.walkDir(relative = false):
    if kind == pcFile:
      for act in actions:
        if act.suffix.anyOfIt(file.endsWith(it)):
          case act.kind:
            of faRemove:
              showWarn(&"Removing file '{file}'")
              removeFile(file)
            of faMove:
              let (_, filename) = file.splitPath()
              createDir(act.destination)
              let target = os.joinpath(act.destination, filename)
              showLog(&"Moving file '{file}' to '{target}'")
              moveFile(file, target)

  showInfo("All files moved")


when isMainModule:
  main()

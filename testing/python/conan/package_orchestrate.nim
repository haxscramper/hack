import parsetoml
import options
import sequtils
import hargparse
import shell
import macros
import os

import sequtils
import colechopkg/lib
import strutils

# IDEA if `--no-interactive` options is not present and some arguments
# are missing ask user to provide input.

const cppnames = @["c++", "cpp", "cxx"]
# var testing = true

proc showError(msgs: varargs[string, `$`]) =
  ceUserError0("PCR: " & msgs.join(" "))

proc showLog(msgs: varargs[string, `$`]) =
  ceUserLog0("PCR: " & msgs.join(" "))
proc showInfo(msgs: varargs[string, `$`]) =
  ceUserInfo2("PCR: " & msgs.join(" "))
proc showWarn(msgs: varargs[string, `$`]) =
  ceUserWarn("PCR: " & msgs.join(" "))

template optOrDefault(optname: untyped, default: string): string =
  if optname.kp(): optname.k().toStr()
  else: default



proc strvalOrDie(conf: TomlValueRef, path: seq[string]): string =
  var current: seq[string]
  var nodenow = conf
  for sub in path:
    current.add sub
    if nodenow.hasKey(sub):
      nodenow = nodenow[sub]
    else:
      let jpath = current.mapIt(&"[\"{it}\"]").join("")
      showError(&"Confguration is missing key at path {jpath}")
      quit 1

  result = nodenow.getStr()

proc strvalOrDie(conf: TomlValueRef, key: string): string =
  strvalOrDie(conf, @[key])


proc strvalOrDefault(
  conf: TomlValueRef,
  path: seq[string],
  default: string,
  logVal = true
     ): string =
  var current: seq[string]
  var nodenow = conf
  for sub in path:
    current.add sub
    if nodenow.hasKey(sub):
      nodenow = nodenow[sub]
    else:
      let jpath = current.mapIt(&"[\"{it}\"]").join("")
      showWarn(&"Confguration is missing key at path {jpath}, using default value \"{default}\"")
      return default

  result = nodenow.getStr()
  if logVal:
    let jpath = current.mapIt(&"[\"{it}\"]").join("")
    showInfo(&"Configuration has value of \"{result}\" on path {jpath}")

const printNone: set[DebugOutputKind] = {}
const printCommand: set[DebugOutputKind] = {dokCommand}


type CmdTable = Table[string, CmdArg]

proc parseCmdline(): (seq[string], CmdTable) =
  parseArgs:
    opt:
      name: "conffile"
      opt: ["--conffile", "+takes_value"]
      help: "Location of the configuration file. Default is 'conffile.toml'"
    opt:
      name: "lang"
      opt: ["--lang", "+takes_value"]
      help: "Programming language to create package for"
      takes: concat(cppnames)
    opt:
      name: "name"
      opt: ["--name", "+takes_value"]
      help: "Name of the package"
    opt:
      name: "version"
      opt: ["--version", "+takes_value"]
      help: "Starting version of the package"
    opt:
      name: "type"
      opt: ["--type", "+takes_value"]
      help: "Type of the package (library/app)"
      takes: @["staticlib", "app", "library"]
    opt:
      name: "buildsystem"
      opt: ["--buildsystem", "+takes_value"]
      help: "Build system to use for the package"
      takes: @["qmake"]
    opt:
      name: "testing"
      opt: ["--testing", "+takes_value"]
      help: "Run in test mode - ! ONLY FOR DEVELOPMENT PURPOSES !"
      takes: @["true", "false"]
    opt:
      name: "password"
      opt: ["--password", "+takes_value"]
      help: "Password if one of the options will need it"
    opt:
      name: "no-example"
      opt: ["--no-example"]
      help: "Do not create example files"

  result = (argParsed, optParsed)

func parseCmdArgImpl(arg: CmdArg, t: string): string = arg.vals.join()

template parseCmdArg(arg, typespec: untyped): untyped =
  var typeSelector: typespec
  parseCmdArgImpl(arg, typeSelector)

macro withOpt*(head: untyped, bodies: varargs[untyped]): untyped =
  assert head.kind == nnkInfix
  # defer: echo result.toStrLit()

  let elseBranch =
    if bodies.len == 2: bodies[1][0]
    else: quote do: discard

  let onKey = bodies[0]


  # TODO Refactor into single case
  if head[1].kind == nnkStrLit and head[2].kind == nnkIdent:
    let keyName = head[1]
    let varIndent = head[2]

    result =
      quote do:
        if optParsed.hasKey(`keyName`):
          let `varIndent` {.inject.} = optParsed[`keyName`]
          `onKey`
        else:
          `elseBranch`
  elif head[1].kind == nnkPar and head[2].kind == nnkPar:
    let names = toSeq(head[1].children)
    let vars = toSeq(head[2].children)

    type
      OptPair = object
        name: NimNode
        varsym: NimNode
        vartype: Option[NimNode]

    let parsed = zip(names, vars).mapIt(
      block:
        let name = it[0]
        let sym = it[1]
        let (varsym, vartype) =
          if sym.kind == nnkBracketExpr: (sym[0], some(sym[1]))
          else: (sym, none(NimNode))
        OptPair(name: name, varsym: varsym, vartype: vartype)
    )

    let condition = parsed.mapIt(it.name).mapIt((quote do: optParsed.hasKey(`it`))).foldr(
      newPar(newTree(nnkInfix, ident("and"), a, b))
    )

    let variables = parsed.mapIt(
      block:
        let varsym = it.varsym
        let name = it.name
        if it.vartype.isSome():
          let vartype = it.vartype.get()
          quote do:
            let `varsym`: `vartype` = parseCmdArg(optParsed[`name`], `vartype`)
        else:
          quote do:
            let `varsym`: CmdArg = optParsed[`name`]
    ).newStmtList()

    result =
      quote do:
      if `condition`:
        `variables`
        `onKey`
      else:
        `elseBranch`

func isEmpty(str: string): bool =
  str.len == 0 or str.allIt(it in @[' ', '\t'])

proc deindent(multiline: string): string =
  let seplines = multiline.split('\n')
  var indent = 0
  for c in seplines[0]:
    if c == ' ': inc indent
    else: break

  seplines.mapIt(if it.isEmpty(): it else: it[indent..^1]).join("\n")

proc showwritefile(filename, body: string): void =
  let content = body.deindent()
  showInfo("Writing to file '" & filename & "'")
  echo content
  filename.writeFile(content)

proc createQmakeCppExample(ptype, project: string, makeExample: bool = true) =
  if ptype in @["staticlib", "library"]:
    if ptype == "library":
      showWarn("Project type is 'library', assuming static library")

    if makeExample:
      "header.hpp".showwritefile """
      void printNumber(int number);
      """

      "source.cpp".showwritefile """
      #include <iostream>

      void print(int number) {
        std::cout << "printing number " << number << "\n";
      }
      """

      (project & ".pro").showwritefile """
      TEMPLATE = lib
      CONFIG += staticlib
      TARGET = $1

      HEADERS *= $$$$PWD/header.hpp
      SOURCES *= $$$$PWD/source.cpp
      """ % [project]
    else:
      showInfo(&"""
      Example has been disabled - to avoid issues with build/deployment
      please ensure that following conditions are met: 'TARGET' in project
      file is '{project}', name of the qmake project file is '{project}.pro'.
      This is not mandatory but mismatch /might/ result in some issues.
      """)

    "conffile.toml".showwritefile &"""
    [meta]
    language = "c++"

    [pkgconfig]
    name = "{project}"
    version = "0.1"
    user = "author"
    channel = "testing"
    package_type = "library"

    [buildconfig]
    package_manager = "conan"
    conan_build_folder = "build"
    conan_reqs_folder = "conan"
    build_system = "qmake"
    copy_to_build = [ "*.hpp", "*.cpp", "*.pro" ]
    libtype = "static"

    [exportconfig]
    library_globs = [ "*.a" ]
    header_folder = "include/{project}"
    header_globs = [ "*.hpp" ]
    """



proc createConanfile(): void =
  "conanfile.py".showwritefile """
  common = __import__("conan_toml")
  class ConanToml(common.TomlConfPackage):
      pass
  """

proc createCppPackage(optParsed: CmdTable) =
  showLog("Creating cpp package")
  var pkgname = ""
  var pkgversion = ""
  var pkgauthor = ""
  var uploadchannel = ""

  withopt ("name", "version") as (name[string], version[string]):
    pkgname = name
    pkgversion = version
    createConanfile()
  else:
    showError("Missing 'name' and 'version' options")
    quit 1

  withopt ("type", "buildsystem") as (ptype[string], buildsystem[string]):
    case buildsystem:
      of "qmake":
        showLog(&"Creating qmake {ptype} package")
        createQmakeCppExample(ptype, pkgname, makeExample = not "no-example".kp())
        let (res, err, code) = shellVerboseErr {dokCommand}:
          conan install ". --install-folder conan"

      else:
        showError(&"Unknown build system '{buildsystem}'")
  else:
    showError("Missing build system or project type")


proc createPackage(optParsed: CmdTable) =
  showLog("Creating package")
  withopt "lang" as lang:
    if lang.toStr() in cppnames:
      createCppPackage(optParsed)
  else:
    showError("Missing 'lang' option from configuration")

proc testPackage(optParsed: CmdTable) =
  showLog("Testing package")


proc publishConanCppPackage(optParsed: CmdTable, tomlconf: TomlValueRef): void =
  showLog("Publishing conan package")

  let builddir = tomlconf.strvalOrDefault(
    @["buildconfig", "conan_build_folder"], "build")

  let reqsdir = tomlconf.strvalOrDefault(
    @["buildconfig", "conan_reqs_folder"], "conan")

  let pkgname = tomlconf.strvalOrDie(@["pkgconfig", "name"])
  let version = tomlconf.strvalOrDie(@["pkgconfig", "version"])
  let channel = tomlconf.strvalOrDefault(@["pkgconfig", "channel"], "testing")
  let author = tomlconf.strvalOrDefault(@["pkgconfig", "author"], "demo")
  let remote = tomlconf.strvalOrDefault(@["pkgconfig", "remote"], "local")
  let reference = pkgname & "/" & version
  showInfo(&"Package refrence is '{reference}'")


  showInfo(&"""
  Publishing conan package. Build dir is '{builddir}', conan configuration
  will be taken from '{reqsdir}'
  """)

  block: # Copy to local cache
    let (res, err, code) = shellVerboseErr printCommand:
      conan "export ."

    if code != 0:
      showError("Error while running 'conan export'. Output:")
      echo res
      echo err
      quit 1

    else:
      showLog(&"""
      Succesfully copied recipe to local cache.
      """)

  block: # Build binary package for recipe
    let (res, err, code) = shellVerboseErr printCommand:
      conan "create" ("--test-build-folder="$builddir) "." ($reference)

    if code != 0:
      showError("Error while running 'conan create'. Output:")
      echo res
      echo err
      quit 1

    else:
      showLog(&"""Succesfully ran 'conan create'.""")

  var doUpload = false
  block:
    if "password".kp():
      let password = "password".k.toStr()
      showInfo(&"Loggin in as user '{author}'")
      let (res, err, code) = shellVerboseErr:
        conan user ($author) "-r" ($remote) "-p" ($password)

      if code != 0:
        showError("Failed to login into remote")
      else:
        showInfo("Remote login ok")
        doUpload = true
    else:
      showWarn(&"""
      No password were provided - upload to remote should be done manually.
      To do this run 'conan upload -r={remote} --confirm {reference} --force'
      """)

  if doUpload: # Upload binary packge to remote if login is ok
    let (res, err, code) = shellVerboseErr printCommand:
      conan "upload" ("-r="$remote) "--confirm" ($reference) "--force"

    if code != 0:
      showError("Error while running 'conan uplload'. Output:")
      echo res
      echo err

    else:
      showLog(&"""Succesfully ran 'conan upload'.""")
      showInfo(&"""
      Now your package is available in remote {remote}.
      To install it add '{reference}@{author}/{channel}' to
      '[requires]' section in your 'conanfile.txt'
      """)



proc publishPackage(optParsed: CmdTable) =
  showLog("Publishing package")

  showInfo("Current directory is", getCurrentDir())
  let conffile = optOrDefault("conffile", "conffile.toml")
  showInfo("Using configuration file", conffile)
  let tomlconf = conffile.readFile().string().parseString()

  let lang = tomlconf["meta"]["language"].getStr()
  let pacman = tomlconf["buildconfig"]["package_manager"].getStr()

  if lang in cppnames:
    case pacman:
      of "conan": publishConanCppPackage(optParsed, tomlconf)

proc buildConanCppPackage(optParsed: CmdTable, tomlconf: TomlValueRef): void =
  showLog("Building conan package")
  let builddir = tomlconf.strvalOrDefault(@["buildconfig", "conan_build_folder"], "build")
  let reqsdir = tomlconf.strvalOrDefault(@["buildconfig", "conan_reqs_folder"], "conan")

  showInfo(&"""
  Running 'conan build'. Build dir is '{builddir}', conan configuration
  will be taken from '{reqsdir}'
  """)

  let (res, err, code) = shellVerboseErr printNone:
    conan build "." ("--build-folder="$builddir) ("--install-folder="$reqsdir)

  if code != 0:
    showError("Error while running 'conan build'. Output:")
    echo res
    echo err

  else:
    showLog(&"""
    Succesfully ran 'conan build'. Build artifacts are now located in the
    '{builddir}' directory.
    """)

proc buildPackage(optParsed: CmdTable) =
  showLog("Building package")
  showInfo("Current directory is", getCurrentDir())
  let conffile = optOrDefault("conffile", "conffile.toml")
  showInfo("Using configuration file", conffile)
  let tomlconf = conffile.readFile().string().parseString()

  let lang = tomlconf["meta"]["language"].getStr()
  let pacman = tomlconf["buildconfig"]["package_manager"].getStr()

  if lang in cppnames:
    case pacman:
      of "conan": buildConanCppPackage(optParsed, tomlconf)

proc main() =
  let (argParsed, optParsed) = parseCmdline()
  for k, v in optParsed:
    showLog(&"{k}: {v.vals.join()}")

  if argParsed.len() < 1:
    showError("Missing command")
  else:
    case argParsed[0]:
      of "create": createPackage(optParsed)
      of "build": buildPackage(optParsed)
      of "publish": publishPackage(optParsed)
      of "test": testPackage(optParsed)

when isMainModule:
  main()

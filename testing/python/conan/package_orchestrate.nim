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
const testing = true

proc showError(msgs: varargs[string, `$`]) = ceUserError0("PCR: " & msgs.join(" "))
proc showLog(msgs: varargs[string, `$`]) = ceUserLog0("PCR: " & msgs.join(" "))
proc showInfo(msgs: varargs[string, `$`]) = ceUserInfo2("PCR: " & msgs.join(" "))

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
      opt: ["--name", "+take_value"]
      help: "Name of the package"

  when testing:
    argParsed = @["create"]

  result = (argParsed, optParsed)

func parseCmdArgImpl(arg: CmdArg, t: string): string =
  return "test"

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
  showInfo("Writing to file " & filename)
  echo content
  filename.writeFile(content)

proc createQmakeCppExample(ptype, project: string) =
  if ptype == "staticlib":
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

    "conffile.toml".showwritefile &"""
    [meta]
    language = "c++"

    [pckgconfig]
    name = "{project}"
    version = "0.1"
    user = "demo"
    channel = "testing"
    package_type = "library"

    [buildconfig]
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
    shell:
      # conan new ($pkgname)/($version)
      conan install "." --install-folder install
  else:
    when testing:
      pkgname = "exampleqt"
      createConanfile()
    else:
      showError("Missing 'name' and 'version' options")

  withopt ("type", "buildsystem") as (ptype[string], buildsystem[string]):
    case buildsystem:
      of "qmake": createQmakeCppExample(ptype, pkgname)

  else:
    when testing:
      createQmakeCppExample("staticlib", "exampleqt")
      shell:
        conan install ". --install-folder install"
        conan build ". --build-folder build --install-folder install"
        conan package ". --build-folder build --install-folder install"
        conan create "--test-build-folder=build . demo/testing"
        conan "export ."

      showInfo(
        &"Done. Use 'conan upload -r=local --skip-upload {pkgname}/{pkgversion}'")


proc createPackage(optParsed: CmdTable) =
  showLog("Creating package")


  withopt "lang" as lang:
    if lang.toStr() in cppnames:
      createCppPackage(optParsed)
  else:
    when testing:
      createCppPackage(optParsed)
    else:
      showError("Missing 'lang' option from configuration")

proc main() =
  let (argParsed, optParsed) = parseCmdline()
  when testing:
    removeDir("/tmp/conan")
    createDir("/tmp/conan")
    setCurrentDir("/tmp/conan")
  if argParsed.len() < 1:
    showError("Missing command")
  else:
    case argParsed[0]:
      of "create": createPackage(optParsed)

when isMainModule:
  main()

import colechopkg/types
import colechopkg/lib

import parsetoml
import moustachu

import options
import strformat
import sequtils
import strutils
import os
import posix

type
  BuildOption* = object
    name*: string
    uname*: string
    files*: seq[string]
    buildCommand*: string
    runCommand*: string



proc checkList*(list: openArray[
  tuple[p: bool,m: string,l: MessageType]]):
    bool =

  result = true
  for it in list:
    if not it.p:
      if it.l == mError: ceUserError0(it.m)
      if it.l == mWarn: ceUserWarn(it.m)
      result = false


proc parseSingleBuild(
  build: TomlValueRef,
  langExt: string,
  c: var Context,
  parseConf: tuple[verbose: bool] = (verbose: false)
     ): Option[BuildOption] =

  if not build.hasKey("name"):
    ceUserError0("Missing name")
    return

  let name = build["name"]

  if not checkList(
    # IDEA contracts + toml validation DSL. This can be similar to
    # conditional expressions for argparse2
    [(build.hasKey("ext") and build["ext"].getStr() == langExt,
      "", mLog),
     (build.hasKey("ext"),
      fmt"Build '{name}' is missing 'ext'", mWarn),
     (build.hasKey("uname"),
      fmt"Build '{name}' is missing 'uname'", mError),
     (build.hasKey("build_command"),
      fmt"Build '{name}' is missing 'build_command'", mError)
    ]):
    return

  if build.hasKey("build_file"):
    c["build_file"] = build["build_file"].getStr().render(c)


  let buildCommand = build["build_command"].getStr().render(c)
  let runCommand = "$#" %
      [if build.hasKey("run_command"):
        build["run_command"].getStr().render(c)
      else:
        "./{{input_file}}".render(c)]
  let watchedFiles: seq[string] = concat(
    if build.hasKey("file_globs"):
      let globs = build["file_globs"]
      if globs.kind == Array:
        globs.getElems()
        .mapIt(
          it
          .getStr()
          .render(c))
        .mapIt(
          toSeq(it.walkPattern))
        .concat()
      elif globs.kind == String:
        toSeq(globs.getStr().walkPattern())
      else:
        ceUserError0(fmt"""
Incorred format for file globs when parsing
'{build["name"].getStr()}'. Expected string
or array of strings""")
        quit(1)
    else:
      @[]
    ,
    @["{{input_file}}".render(c)])

  result = some(BuildOption(
    name: build["name"].getStr(),
    uname: build["uname"].getStr(),
    files: watchedFiles,
    buildCommand: buildCommand,
    runCommand: runCommand
  ))


proc parseBuildOpts*(
  buildConf: string,
  langExt: string,
  context: var Context,
  buildOpts: seq[BuildOption] = @[],
  parseConf: tuple[verbose: bool] = (verbose: false)
     ): seq[BuildOption] =

  ceUserInfo0("Parsing config")

  result = buildOpts

  let buildConfToml = parsetoml.parseFile(buildConf)

  for build in buildConfToml["build"].getElems():
    let res = parseSingleBuild(build, langExt, context, parseConf)
    if res.isSome:
      result.add(res.get())

proc getBuildOpts*(inputFile: string): seq[BuildOption] =
  # var context: Context = newContext()

  # context["input_file"] = inputFile
  # context["fsm_build_bin_dir"] = getCallPath().splitPath()[0]
  # let ext = inputFile.getLastExt()
  # let conf = getCallPath().splitPath()[0] & "/config/build_commands.toml"

  # result = conf.parseBuildOpts(langExt = ext, context = context, parseConf = (verbose: false))

  result = @[]



proc dumpDebugInfo*(): void =
  echo ":no"

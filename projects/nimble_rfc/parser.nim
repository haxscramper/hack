import
  ./hast_common,
  ./pnode_parse,
  ./nimble_types

import compiler/ast except nkStrKinds

import std/[parsecfg, streams, tables, sets, strutils, sequtils]
from std/options as std_opt import Option, none, some
import std/os as std_os

import
  hmisc/core/all,
  hmisc/other/oswrap


import fusion/matching
{.experimental: "caseStmtMacros".}


proc multiSplit(s: string): seq[string] =
  for chunk in split(s, {char(0x0A), char(0x0D), ','}):
    result.add chunk.strip()

  for i in countdown(result.len()-1, 0):
    if len(result[i]) < 1:
      result.del(i)

  if len(result) < 1:
    if s.strip().len != 0:
      return @[s]

    else:
      return @[]


proc parsePackageInfoCfg*(
  text: string, path: string = "<file>"): Option[PackageInfo] =

  var fs = newStringStream(text)
  var p: CfgParser
  open(p, fs, path)
  defer: close(p)
  var currentSection = ""
  var res = initPackageInfo(AbsFile(path))
  while true:
    var ev = next(p)
    case ev.kind
      of cfgEof: return some(res)
      of cfgSectionStart: currentSection = ev.section
      of cfgKeyValuePair:
        case currentSection.normalize
          of "package":
            case ev.key.normalize
              of "name":         res.name        = ev.value
              of "version":      res.version     = Version(version: ev.value)
              of "author":       res.author      = ev.value
              of "description":  res.description = ev.value
              of "license":      res.license     = ev.value
              of "srcdir":       res.srcDir      = ev.value
              of "bindir":       res.binDir      = ev.value
              of "skipdirs":     res.skipDirs.add(ev.value.multiSplit)
              of "skipfiles":    res.skipFiles.add(ev.value.multiSplit)
              of "skipext":      res.skipExt.add(ev.value.multiSplit)
              of "installdirs":  res.installDirs.add(ev.value.multiSplit)
              of "installfiles": res.installFiles.add(ev.value.multiSplit)
              of "installext":   res.installExt.add(ev.value.multiSplit)
              of "bin":
                for i in ev.value.multiSplit:
                  var (src, bin) = if '=' notin i: (i, i) else:
                    let spl = i.split('=', 1)
                    (spl[0], spl[1])

                  if std_os.splitFile(src).ext == ".nim":
                    return

                  if res.backend == "js":
                    bin = bin.addFileExt(".js")

                  else:
                    bin = bin.addFileExt(ExeExt)

                  res.bin[bin] = src

              of "backend":
                res.backend = ev.value.toLowerAscii()
                case res.backend.normalize
                  of "javascript":
                    res.backend = "js"

                  else:
                    discard

              of "nimbletasks":
                for i in ev.value.multiSplit:
                  res.nimbleTasks.incl(i.normalize)

              of "beforehooks":
                for i in ev.value.multiSplit:
                  res.preHooks.incl(i.normalize)

              of "afterhooks":
                for i in ev.value.multiSplit:
                  res.postHooks.incl(i.normalize)

              else:
                return

          of "deps", "dependencies":
            case ev.key.normalize
              of "requires":
                for v in ev.value.multiSplit:
                  res.requires.add(parseRequires(v.strip))

              else:
                return

          else:
            return

      of cfgOption:
        return

      of cfgError:
        return



type
  NimsParseFail* = ref object of ArgumentError

proc getStrValues(node: PNode): seq[string] =
  var values: seq[string]
  if (node.kind == nkPrefix and node[1].kind == nkBracket) or
     node.kind == nkBracket:

    let node = (if node.kind == nkPrefix: node[1] else: node)

    for arg in node:
      if arg.kind in nkStrKinds:
        values.add arg.getStrVal()

      else:
        raise NimsParseFail(
          msg: "Cannot get item value from " & $node.kind)

  elif node.kind in nkStrKinds:
    values.add node.getStrVal()

  elif node.kind in {nkIdent, nkCall, nkWhenStmt, nkInfix}:
    raise NimsParseFail(
      msg: "Cannot get property value from " & $node.treeRepr())

  else:
    raise newImplementKindError(node, $node.treeRepr())

  return values

type
  ExtraPackageInfo* = object
    cfgManifest*: bool
    nimsManifest*: bool
    node*: PNode



proc parsePackageInfoNims*(
    text: string,
    fails: var Table[string, NimsParseFail],
    extra: var ExtraPackageInfo,
    path: string = "<file>"
  ): Option[PackageInfo] =

  ##[
* TODO edge cases


#+begin_src nim
  namedBin["name"] = "expr" # nwnt
  version       = pkgVersion # fae

  # paravim
  installExt    = @[
    "nim", "txt", "ttf", "glsl", "c", "h",
    when defined(windows):
      "dll"
    elif defined(macosx):
      "dylib"
    elif defined(linux):
      "so"
  ]

  # metar
  include metar/version

  version = metarVersion

  # plz
  version     = CompileDate.replace("-", ".")
#+end_src

]##

  proc parseStmts(
      node: PNode, res: var PackageInfo,
      fails: var Table[string, NimsParseFail]) =

    case node.kind:
      of nkCommand, nkCall:
        if node[0].kind == nkDotExpr:
          if node[0][1].getStrVal() == "add":
            let values = node[1].getStrValues()
            case node[0][0].getStrVal().normalize:
              of "installext":   res.installExt.add   values
              of "skipdirs":     res.skipDirs.add     values
              of "installdirs":  res.installDirs.add  values
              of "skipext":      res.skipExt.add      values
              of "skipfiles":    res.skipFiles.add    values
              of "installfiles": res.installFiles.add values

          else:
            discard

        else:
          case node[0].getStrVal().normalize:
            of "requires":
              for arg in node[1 ..^ 1]:
                case arg.kind:
                  of nkStrKinds:
                    for req in arg.getStrVal().multiSplit():
                      res.requires.add parseRequires(req)

                  of nkStmtList:
                    for dep in arg:
                      for req in dep.getStrVal().multiSplit():
                        res.requires.add parseRequires(req)

                  of nkIdent, nkExprEqExpr, nkInfix:
                    # Expr eq expr first encountered in `fision`
                    fails["requires"] = NimsParseFail(
                      msg: "Cannot get property requires from " & $arg.kind)

                  of nkPrefix:
                    for req1 in arg.getStrValues():
                      for req2 in req1.multiSplit():
                        res.requires.add parseRequires(req2)


                  else:
                    raise newImplementError(
                      "Unhandled node kind for requires argument: \n" &
                        $treeRepr(arg))

            of "task": res.nimbleTasks.incl node[1].getStrVal()
            of "after": res.postHooks.incl node[1].getStrVal()
            of "before": res.preHooks.incl node[1].getStrVal()
            of "foreigndep": res.foreignDeps.add node[1].getStrVal()
            of "switch", "mkdir", "hint", "writefile", "exec":
              discard

            else:
              discard

      of nkAsgn:
        if node[0].kind == nkBracketExpr:
          let property = node[0][0].getStrVal().normalize
          case property:
            of "namedbin":
              res.bin[node[0][1].getStrVal()] = node[1].getStrValues()[0]

            else:
              discard

        elif node[0].kind == nkIdent:
          let property = node[0].getStrVal().normalize
          template tryStr(expr: untyped): untyped =
            try:
              expr

            except NimsParseFail as e:
              fails[property] = e
              ""



          case property:
            of "version":
              case node[1].kind:
                of nkStrKinds:
                  res.version = Version(version: node[1].getStrVal())

                of nkDotExpr:
                  if node[1][1].getStrVal() == "NimVersion":
                    res.version = Version(version: NimVersion)

                  else:
                    raise newImplementError(
                      "Unhandled node structure: " & $treeRepr(node[1]))

                of nkIdent, nkBracketExpr, nkCall, nkCommand:
                  fails[property] = NimsParseFail(
                    msg: "Cannot get version value from " & $node[1].kind)

                else:
                  raise newImplementError(
                    "Unhandled node structure: " & $treeRepr(node[1]))


            of "license": res.license         = tryStr node[1].getStrValues()[0]
            of "description": res.description = tryStr node[1].getStrValues()[0]
            of "srcdir": res.srcDir           = tryStr node[1].getStrValues()[0]
            of "packagename": res.name        = tryStr node[1].getStrValues()[0]
            of "bindir": res.name             = tryStr node[1].getStrValues()[0]
            of "author": res.author           = tryStr node[1].getStrValues()[0]
            of "backend": res.backend         = tryStr node[1].getStrValues()[0]
            of "name": res.name               = tryStr node[1].getStrValues()[0]
            of "bin", "installext", "skipdirs", "installdirs", "skipext",
               "skipfiles", "installfiles":

              try:
                let values = node[1].getStrValues()

                case property:
                  of "bin":
                    if values.len > 0:
                      res.bin[values[0]] = values[0]

                  of "installext":   res.installExt   = values
                  of "skipdirs":     res.skipDirs     = values
                  of "installdirs":  res.installDirs  = values
                  of "skipext":      res.skipExt      = values
                  of "skipfiles":    res.skipFiles    = values
                  of "installfiles": res.installFiles = values

              except NimsParseFail as e:
                fails[property] = e


            of "namedbin":
              var maps: seq[tuple[key, value: string]]

              case node[1]:
                of Call[DotExpr[TableConstr[all @args], _]]:
                  for pair in args:
                    res.bin[pair[0].getStrVal()] = pair[1].getStrVal()

                else:
                  echo node[1]
                  raise newImplementError(
                    "Unhandled node structure: \n" &
                      $treeRepr(node[1]))

            of "mode":
              discard


            else:
              discard

      of nkWhenStmt:
        for branch in node:
          case branch.kind:
            of nkElifBranch, nkElifExpr: parseStmts(branch[1], res, fails)
            of nkElseExpr, nkElse: parseStmts(branch[0], res, fails)
            else:
              raise newImplementError(
                "Unhandled node structure: " & $treeRepr(branch))

      of nkCommentStmt, nkImportStmt, nkFromStmt, nkIncludeStmt,
         nkConstSection, nkVarSection, nkLetSection, nkTypeSection,
         nkProcDeclKinds, nkLiteralKinds, nkDiscardStmt:
        discard

      of nkStmtList:
        for stmt in node:
          parseStmts(stmt, res, fails)

      of nkIfStmt:
        discard

      of nkPrefix, nkIdent, nkPragma:
        discard

      of nkBracket:
        # First found in https://github.com/h3rald/litestore/blob/b47f906761bf98c768d814ff124379ada24af6f6/litestore.nimble#L1
        discard

      else:
        raise newImplementError(
          "Unhandled configuration file element: \n" & $treeRepr(node))




  var res = initPackageInfo(AbsFile(path))

  let node = parsePNodeStr(text)
  extra.node = node

  if isNil(node):
    return none(PackageInfo)

  for stmt in node:
    parseStmts(stmt, res, fails)

  return some(res)

proc parsePackageInfo*(
    configText: string,
    fails: var Table[string, NimsParseFail],
    extra: var ExtraPackageInfo,
    path: AbsFile = AbsFile("<file>")
  ): PackageInfo =

  var parseRes = parsePackageInfoCfg(configText, path.string)
  if parseRes.isSome():
    extra.cfgManifest = true
    return parseRes.get()


  parseRes = parsePackageInfoNims(configText, fails, extra, path.string)
  if parseRes.isSome():
    extra.nimsManifest = true
    return parseRes.get()

  else:
    raise newException(
      PackageParseError,
      "Cannot parse package at path: " & $path)


proc parsePackageInfo*(
    configText: string, path: AbsFile = AbsFile("<file>")
  ): PackageInfo =

  var fails: Table[string, NimsParseFail]
  var extra: ExtraPackageInfo
  return parsePackageInfo(configText, fails, extra, path)

proc getPackageInfo*(path: AbsFile, absPath: bool = true): PackageInfo =
  let configText = readFile(path)
  assertExists(path)
  result = parsePackageInfo(configText, path)
  if absPath and not oswrap.isAbsolute(result.packageFile):
    result.packageFile = path.dir() /. string(result.packageFile)


proc nimbleSearchDir*(): AbsDir = ~".nimble" / "pkgs"

proc getRequires*(file: AbsFile): seq[PkgTuple] =
  getPackageInfo(file).requires

proc resolvePackage*(pkg: PkgTuple): AbsDir =
  ## Resolve package `pkg` constraints to absolute directory
  discard

proc findPackage*(
    name: string,
    version: VersionRange,
    searchDir: AbsDir = nimbleSearchDir(),
    options: Options = initDefaultNimbleOptions(),
  ): Option[PackageInfo] =

  var pkgList {.global.}: seq[PackageInfo] = @[]

  once:
    pkgList = getInstalledPkgsMin(searchDir, options)

  let dep: PkgTuple = (name: name, ver: version)

  let resolvedDep = resolveAlias(dep, options)
  var pkg: PackageInfo
  var found = findPkg(pkgList, resolvedDep, pkg)
  if not found and resolvedDep.name != dep.name:
    found = findPkg(pkgList, dep, pkg)

  if found:
    return some(pkg)

proc projectFile*(info: PackageInfo): AbsFile =
  AbsFile(info.packageFile)

proc projectPath*(info: PackageInfo): AbsDir =
  AbsDir(info.packageFile.AbsFile().dir())

proc projectImportPath*(info: PackageInfo): AbsDir =
  AbsDir(info.getRealDir())

proc resolveNimbleDeps*(
    file: AbsFile,
    searchDir: AbsDir = nimbleSearchDir(),
    options: Options = initDefaultNimbleOptions()
  ): seq[PackageInfo] =

  let info = getPackageInfo(file)
  for dep in info.requires:
    let pkg = findPackage(dep.name, dep.ver, searchDir, options)

    if pkg.isNone():
      if dep.name != "nim":
        raise newImplementError("")

    else:
      result.add(pkg.get())
      result.add(
        pkg.get().projectFile().resolveNimbleDeps(
          searchDir, options))

proc fromMinimal*(packages: seq[PackageInfo]): seq[PackageInfo] =
  for package in packages.deduplicate():
    var info = parsePackageInfo(package.projectFile().readFile())
    info.packageFile = package.packageFile
    info.isInstalled = package.isInstalled
    info.isLinked = package.isLinked
    info.name = package.name
    result.add info

proc findNimbleFile*(dir: AbsDir): Option[AbsFile] =
  for dir in parentDirs(dir):
    for file in walkDir(dir, AbsFile):
      if ext(file) in ["nimble", "babel"]:
        return some(file)

proc getPackageInfo*(dir: AbsDir, absPath: bool = true): PackageInfo =
  let file = dir.findNimbleFile()
  if file.isNone():
    # REFACTOR use error dereived from `hmisc.PathError`
    raise newGetterError(
      "Could not find nimble package in directory " & $dir)

  else:
    result = getPackageInfo(file.get(), absPath)

proc getNimblePaths*(
    file: AbsFile, searchDir: AbsDir = nimbleSearchDir()
  ): seq[AbsDir] =
  var nimbleFile = findNimbleFile(file.dir())
  if nimbleFile.isSome():
    for dep in resolveNimbleDeps(nimbleFile.get(), searchDir):
      result.add AbsDir(dep.getRealDir())

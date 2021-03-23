

import common

import std/[
  httpclient, strformat, uri, strutils, sets, hashes, tables,
  deques, times, macros, tables, parseutils, parsecfg, streams,
  sequtils
]

import compiler/[
  nimeval, llstream, ast, modulegraphs, vmdef, vm,
  passes, idents, options, lineinfos
]

import compiler/renderer except `$`

import hnimast/[hast_common, pnode_parse]
import hasts/graphviz_ast

import hmisc/other/[oswrap, hjson, hshell, colorlogger]
import hmisc/types/[colorstring]
import hmisc/hexceptions
import hmisc/helpers
import fusion/matching
import hpprint
import ./test6

import nimblepkg/[packageinfo, version]
import nimblepkg/common as nimble_common
import haxdoc/compiler_aux

{.experimental: "caseStmtMacros".}


type
  InfoStatus = enum
    isParseOk
    isNimsFail
    isParseFail

  CommitStat = object
    commitHash: string
    date: string
    case ok: InfoStatus
      of isParseOk:
        info: PackageInfo

      of isNimsFail:
        nimsFail: NimsParseFail

      of isParseFail:
        parseFail: ref NimbleError

  RepoStat = object
    packageListConfig: JsonNode
    commits: seq[CommitStat]

proc toJson(j: JsonNode): JsonNode = j
proc toJson(v: Version): JsonNode = toJson(v.string)
proc toJson(s: cstring): JsonNode = toJson($s)
proc toJson(info: PackageInfo): JsonNode =
  hjson.toJson(info, @["nimbleTasks", "postHooks", "preHooks"])


when isMainModule:
  startHax()
  startColorLogger()

  let url = "https://raw.githubusercontent.com/nim-lang/packages/master/packages.json"
  let file = RelFile("packages.json")

  let client = newHttpClient()

  if not exists(file):
    client.downloadFile(url, file.string)

  let js = parseJson(file)
  let cloneDir = ~"tmp" / "nimrepos"
  mkDir(cloneDir)

  var packageMap: Table[AbsDir, JsonNode]

  let tryClone = false

  withDir cloneDir:
    withEnv({ $$GIT_TERMINAL_PROMPT : "0"}):
      for val in js.getElems():
        if val.matches({ "method" : (getStr: "git") }):
          let url = val["url"].getStr().dropSuffix("/").addSuffix(".git")
          let outDir = parseUri(url).
            path.AbsFile().splitFile2().file.RelFile().withExt("").getStr()

          let fullOut = cwd() / outDir
          let tryClone = tryClone and not exists(fullOut)
          if tryClone:
            try:
              execShell(shCmd(git, clone, $url, $outDir))
              packageMap[fullOut] = val
            except:
              discard

          else:
            if exists(fullOut):
              packageMap[fullOut] = val

  var cnt = 0
  for repoDir, config in packageMap:
    let statFile = repoDir /. "stats.json"
    inc cnt
    if exists(statFile):
      notice repoDir, "has been parsed already"
      continue

    elif endsWith($repoDir, "Nim"):
      warn "Force skipping main repo"
      continue

    var repoStat = RepoStat(packageListConfig: config)
    info repoDir
    logIndented: withDir repoDir:
      let commits = runShell(shCmd(git, `rev-list`, --all)).stdout.strip().split("\n")
      for commitIdx, commit in commits:
        execShell(shCmd(git, checkout, -q, $commit))
        let part = &"[{commitIdx}/{commits.len}] ({cnt}/{packageMap.len})"
        for file in walkDir(cwd(), AbsFile):
          if file.ext() == "nimble":
            try:
              let info = parsePackageInfo(file.readFile())
              repoStat.commits.add CommitStat(ok: isParseOk, info: info)
              debug commit, "ok", part

            except NimsParseFail as fail:
              repoStat.commits.add CommitStat(ok: isNimsFail, nimsFail: fail)
              warn commit, "nims fail", part

            except NimbleError as fail:
              repoStat.commits.add CommitStat(ok: isParseFail, parseFail: fail)
              warn commit, "total fail", part

            except:
              err repoDir
              raise

            repoStat.commits[^1].commitHash = commit
            repoStat.commits[^1].date = evalShellStdout(
              shCmd(git, show, -s, date=iso, format="%ad"))

            break


          # let (stdout, stderr, code) = runShell(shCmd(git, clone, $url), false)
          # if code != 0:
          #   if not stderr.startsWith("fatal: destination path"):
          #     echo "badurl ", url, " written as ", val["url"]
          #     echo stderr
          # else:
          #   echo "cloned ", url


    statFile.writeFile(toJson(repoStat).pretty)
    info "Wrote", statFile
    # stats.add repoStat

  var totalCommits = 0
  var res = JsonNode(kind: JArray)
  for repoDir, config in packageMap:
    let statFile = repoDir /. "stats.json"
    if not exists(statFile):
      continue

    let spec = statFile.parseJson()
    res.add spec

  writeFile(cloneDir /. "total.json", pretty(res))
  info "Done full ecosystem stat"

  # var resJson = JsonNode(kind: JArray)
  # for repo in stats:
  #   resJson.add toJson(repo)

  # "res.json".writeFile(pretty(resJson))

# proc getParseContent(name: string, raw: URI, web: string) =
#   try:
#     stats.downTime = cpuTime()
#     var content = getContent($raw, web)
#     stats.downTime = cpuTime() - stats.downTime

#     stats.evalTime = cpuTime()

#     var usesWhen = false

#     when evalNims:
#       content = "proc requires*(args: varargs[string]) = discard\n" &
#         content

#       try:
#         graph.processCode(m, content)
#       except:
#         discard


#     else:
#       try:
#         let info = parsePackageInfo(content)

#         for dep in info.requires:
#           requires.add dep.name
#           inc stats.depStats[dep.ver.kind]

#       except NimbleError:
#         echo content
#         stats.errCannotParse.add name
#         return

#       except:
#         printSeparator(name)
#         echo content.indent(4)
#         echo getCurrentExceptionMsg()
#         stats.errCannotParse.add name
#         pprintStackTrace()
#         printSeparator("--")
#         return

#     stats.evalTime = cpuTime() - stats.evalTime
#     stats.totalTime = cpuTime() - stats.totalTime

#     proc toMs(s: float): int = int(s * 1000)
#     if requires.len == 0 and ("requires" in content):
#       echo "No requirements"
#       raiseImplementError("")

#     reqTable[name] = requires
#     requires = @[]
#     stats.okList.add name


#   except HttpRequestError as e:
#     errUrls.incl $raw
#     if flushEach:
#       errUrlsFile.writeFile(errUrls.toJson().toPretty())

#     stats.httpErrList.add (name, web)
#     echo toRed(name.pad), " failed http request"

#   except ArgumentError as e:
#     echo toRed(name.pad), " cannot parse"
#     echo e.msg.indent(2)
#     stats.errCannotParse.add name

# proc joinPath*(T: typedesc, args: varargs[string]): T =
#   when result is AbsFile: result = AbsFile joinPath(args)
#   elif result is AbsDir: result = AbsDir joinPath(args)
#   elif result is RelFile: result = RelFile joinPath(args)
#   elif result is RelDir: result = RelFile joinPath(args)

#   else:
#     static:
#       {.error: "zzz".}

# proc `==`*(j: JsonNode, str: string): bool =
#   j.kind == JString and j.getStr() == str

# proc endsWith*(j: JsonNode, strs: openarray[string]): bool =
#   j.kind == JString and anyIt(strs, j.getStr().endsWith(it))

# proc nimbleUrlForWeb(name, web: string): Option[URI] =
#   let nimble = &"{web}/{name}.nimble"
#   var raw = parseUri(nimble)

#   if raw.hostname != "github.com":
#     echo toYellow(name.pad), " does not use github code hosting (", web, ")"
#     stats.noGhList.add name

#     # raiseImplementError("")

#   else:
#     raw.hostname = "raw.githubusercontent.com"
#     let spl = split(raw.path, "/")
#     raw.path = join(spl[0 .. 2] & @["master"] & spl[3 ..^ 1], "/")
#     result = some(raw)

#     if $raw in cache:
#       return

#     else:
#       raw = parseUri(web)
#       let (dir, base, _) = AbsFile(raw.path).splitFile()
#       raw.path = $joinPath(AbsDir, "repos", dir.string, base, "contents")
#       raw.hostname = "api.github.com"

#       try:
#         let contentList = getContent($raw, $raw).parseJson()

#         for entry in contentList:
#           if entry.matches({
#             "type" : "file",
#             "name" : it.endsWith([".nimble", ".babel"]),
#             "download_url": (asStr: @download)
#           }):
#             result = some(parseURI(download))
#             echo "download URL from github API: ", result.get()
#             return


#         echo "Could not find .nimble for web ", web
#         # raiseImplementError("")

#       except HttpRequestError as e:
#         echov toRed(web)
#         echo e.msg.indent(2)



# for pack in parseJson(file):
#   inc stats.totalPack
#   if { "name" : (asStr: @name), "url" : (asStr: @web) } ?= pack:
#     stats.totalTime = cpuTime()
#     let raw = nimbleUrlForWeb(name, web)
#     if raw.isSome():
#       if $get(raw) notin errUrls:
#         getParseContent(name, raw.get, web)

#         echo toGreen(name.pad), " ok"

#       else:
#         stats.httpErrList.add (name, web)
#         echo toMagenta(name.pad), " is known to cause http error"
#       # echo "  " & web
#       # echo e.msg.indent(2)

#     # sleep(100)


# var graph = makeDotGraph()
# graph.styleNode = makeRectConsolasNode()
# graph.rankdir = grdLeftRight
# graph.attrs["dpi"] = "300"

# proc getName(pack: string): string =
#   pack[0 ..< pack.skipWhile(IdentChars)]

# var known: HashSet[string]

# proc optAddPackage(package: string) =
#   let package = getName(package)
#   if package in known:
#     discard

#   else:
#     known.incl package
#     graph.addNode(makeDotNode(hash(package), package))


# for package, deps in reqTable:
#   optAddPackage(package)
#   for dep in deps:
#     if dep.getName() notin ["nim"]:
#       optAddPackage(dep)
#       graph.addEdge makeDotEdge(
#         hash(package.getName()),
#         hash(dep.getName()),
#         dep
#       )


# graph.toPng("/tmp/deps.png")

# echo &"""
# total package count:      {stats.totalPack}
# processing ok:            {stats.okList.len}
# not using github:         {stats.noGhList.len}
# http error when getting:  {stats.httpErrList.len}
# configuration parse fail: {stats.errCannotParse.len}
# """

# proc toStr(rangeKind: VersionRangeEnum): string =
#   case rangeKind:
#     of verLater: "> V"
#     of verEarlier: "< V"
#     of verEqLater: ">= V"
#     of verEqEarlier: "<= V"
#     of verIntersect: "> V & < V"
#     of verEq: "V"
#     of verAny: "*"
#     of verSpecial: "#head"

# for rangeKind in VersionRangeEnum:
#   echo &"{rangeKind:<15} ({rangeKind.toStr():^10}): {stats.depStats[rangeKind]}"

# echo "Finished"

# if not flushEach:
#   cachefile.writeFile(cache.toJson().toPretty())
#   errUrlsFile.writeFile(errUrls.toJson().toPretty())

# for wh in stats.whenList:
#   echo wh.url

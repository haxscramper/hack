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

import hmisc/other/[oswrap, hjson]
import hmisc/types/[colorstring]
import hmisc/hexceptions
import hmisc/helpers
import fusion/matching
import hpprint
import test6

{.experimental: "caseStmtMacros".}

startHax()

let url = "https://raw.githubusercontent.com/nim-lang/packages/master/packages.json"
let file = RelFile("packages.json")

let client = newHttpClient()

if not exists(file):
  client.downloadFile(url, file.string)


proc implementRoutine*(
    graph: ModuleGraph;
    pkg, module, name: string;
    impl: proc (a: VmArgs) {.closure, gcsafe.}
  ) =

  let vm = PCtx(graph.vm)
  vm.registerCallback(pkg & "." & module & "." & name, impl)

var requires: seq[string]

const evalNims = false

import nimblepkg/common as nimble_common

import nimblepkg/[version]
import haxdoc/[compiler_aux]

when evalNims:
  var (graph, m) = newGraphStdin(withEval = false)

  graph.config.structuredErrorHook =
    proc(config: ConfigRef; info: TLineInfo; msg: string; level: Severity) =
      echo msg
      echo info

  type
    CutoutContext = ref object of PPassContext
      module: PSym

  # graph.registerPass(makePass(
  #   (
  #     proc(graph: ModuleGraph, module: PSym): PPassContext {.nimcall.} =
  #       return CutoutContext(module: module)
  #   ),
  #   (
  #     proc(c: PPassContext, n: PNode): PNode {.nimcall.} =

  #       var ctx = CutoutContext(c)

  #       if sfMainModule in ctx.module.flags:
  #         result = n

  #       else:
  #         result = nnkDiscardStmt.newPTree()
  #   ),
  #   (
  #     proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
  #       discard
  #   )
  # ))

  graph.registerPass(evalPass)

  graph.implementRoutine("*", "stdin", "requires",
    proc(args: VmArgs) {.nimcall, gcsafe.} =
      # echo "\e[32m###################\e[39m"
      for idx in 0 ..< args.rc - 1:
        # echo treeRepr(args.getNode(idx))
        for node in args.getNode(idx):
          requires.add node.getStrVal()
  )



  proc processCode(graph: ModuleGraph, m: PSym, str: string) =
    var lineQue = initDeque[string]()
    for line in split(str, '\n'):
      lineQue.addLast line

    proc scriptReader(s: PLLStream, buf: pointer, bufLen: int): int =
      return readLine(lineQue, s, buf, bufLen)

    processModule(graph, m, llStreamOpenStdIn(scriptReader))

  graph.processCode(
    m,
    """
proc requires*(args: varargs[string]) = discard
""")

else:
  proc processCode(code: string): bool =
    discard



var cache: Table[string, string]

let cachefile = RelFile("test7_cache.json")

if fileExists(cacheFile):
  cache = cacheFile.parseJson().to(Table[string, string])

const flushEach = false

proc getContent(url: string, web: string): string =
  if web notin cache:
    echo "non-cached `getContent()`"
    cache[web] = client.getContent(url)

  if flushEach:
    cachefile.writeFile(cache.toJson().toPretty())

  return cache[web]


var cnt = 0
var reqTable: Table[string, seq[string]]
var stats: tuple[
  downTime, evalTime, totalTime: float,
  okList, noGhList, errCannotParse: seq[string],
  totalPack: int,
  whenList, httpErrList: seq[tuple[name, url: string]],
  depStats: array[VersionRangeEnum, int]
]

proc pad(str: string): string = alignLeft(str, 20)

var errUrls: HashSet[string]
let errUrlsFile = RelFile("test7_error_urls.json")
if fileExists(errUrlsFile):
  for entry in errUrlsFile.parseJson():
    errUrls.incl entry.asStr()

proc toJson*[T](s: HashSet[T]): JsonNode =
  result = newJArray()
  mixin toJson
  for item in s:
    result.add toJson(item)


proc getParseContent(name: string, raw: URI, web: string) =
  try:
    stats.downTime = cpuTime()
    var content = getContent($raw, web)
    stats.downTime = cpuTime() - stats.downTime

    stats.evalTime = cpuTime()

    var usesWhen = false

    when evalNims:
      content = "proc requires*(args: varargs[string]) = discard\n" &
        content

      try:
        graph.processCode(m, content)
      except:
        discard


    else:
      try:
        let info = parsePackageInfo(content)

        for dep in info.requires:
          requires.add dep.name
          inc stats.depStats[dep.ver.kind]

      except NimbleError:
        echo content
        stats.errCannotParse.add name
        return

      except:
        printSeparator(name)
        echo content.indent(4)
        echo getCurrentExceptionMsg()
        stats.errCannotParse.add name
        pprintStackTrace()
        printSeparator("--")
        return

    stats.evalTime = cpuTime() - stats.evalTime
    stats.totalTime = cpuTime() - stats.totalTime

    proc toMs(s: float): int = int(s * 1000)
    if requires.len == 0 and ("requires" in content):
      echo "No requirements"
      raiseImplementError("")

    reqTable[name] = requires
    requires = @[]
    stats.okList.add name


  except HttpRequestError as e:
    errUrls.incl $raw
    if flushEach:
      errUrlsFile.writeFile(errUrls.toJson().toPretty())

    stats.httpErrList.add (name, web)
    echo toRed(name.pad), " failed http request"

  except ArgumentError as e:
    echo toRed(name.pad), " cannot parse"
    echo e.msg.indent(2)
    stats.errCannotParse.add name

proc joinPath*(T: typedesc, args: varargs[string]): T =
  when result is AbsFile: result = AbsFile joinPath(args)
  elif result is AbsDir: result = AbsDir joinPath(args)
  elif result is RelFile: result = RelFile joinPath(args)
  elif result is RelDir: result = RelFile joinPath(args)

  else:
    static:
      {.error: "zzz".}

proc `==`*(j: JsonNode, str: string): bool =
  j.kind == JString and j.getStr() == str

proc endsWith*(j: JsonNode, strs: openarray[string]): bool =
  j.kind == JString and anyIt(strs, j.getStr().endsWith(it))

proc nimbleUrlForWeb(name, web: string): Option[URI] =
  let nimble = &"{web}/{name}.nimble"
  var raw = parseUri(nimble)

  if raw.hostname != "github.com":
    echo toYellow(name.pad), " does not use github code hosting (", web, ")"
    stats.noGhList.add name

    # raiseImplementError("")

  else:
    raw.hostname = "raw.githubusercontent.com"
    let spl = split(raw.path, "/")
    raw.path = join(spl[0 .. 2] & @["master"] & spl[3 ..^ 1], "/")
    result = some(raw)

    if $raw in cache:
      return

    else:
      raw = parseUri(web)
      let (dir, base, _) = AbsFile(raw.path).splitFile()
      raw.path = $joinPath(AbsDir, "repos", dir.string, base, "contents")
      raw.hostname = "api.github.com"

      try:
        let contentList = getContent($raw, $raw).parseJson()

        for entry in contentList:
          if entry.matches({
            "type" : "file",
            "name" : it.endsWith([".nimble", ".babel"]),
            "download_url": (asStr: @download)
          }):
            result = some(parseURI(download))
            echo "download URL from github API: ", result.get()
            return


        echo "Could not find .nimble for web ", web
        # raiseImplementError("")

      except HttpRequestError as e:
        echov toRed(web)
        echo e.msg.indent(2)



for pack in parseJson(file):
  inc stats.totalPack
  if { "name" : (asStr: @name), "url" : (asStr: @web) } ?= pack:
    stats.totalTime = cpuTime()
    let raw = nimbleUrlForWeb(name, web)
    if raw.isSome():
      if $get(raw) notin errUrls:
        getParseContent(name, raw.get, web)

        echo toGreen(name.pad), " ok"

      else:
        stats.httpErrList.add (name, web)
        echo toMagenta(name.pad), " is known to cause http error"
      # echo "  " & web
      # echo e.msg.indent(2)

    # sleep(100)


var graph = makeDotGraph()
graph.styleNode = makeRectConsolasNode()
graph.rankdir = grdLeftRight
graph.attrs["dpi"] = "300"

proc getName(pack: string): string =
  pack[0 ..< pack.skipWhile(IdentChars)]

var known: HashSet[string]

proc optAddPackage(package: string) =
  let package = getName(package)
  if package in known:
    discard

  else:
    known.incl package
    graph.addNode(makeDotNode(hash(package), package))


for package, deps in reqTable:
  optAddPackage(package)
  for dep in deps:
    if dep.getName() notin ["nim"]:
      optAddPackage(dep)
      graph.addEdge makeDotEdge(
        hash(package.getName()),
        hash(dep.getName()),
        dep
      )


graph.toPng("/tmp/deps.png")

echo &"""
total package count:      {stats.totalPack}
processing ok:            {stats.okList.len}
not using github:         {stats.noGhList.len}
http error when getting:  {stats.httpErrList.len}
configuration parse fail: {stats.errCannotParse.len}
"""

proc toStr(rangeKind: VersionRangeEnum): string =
  case rangeKind:
    of verLater: "> V"
    of verEarlier: "< V"
    of verEqLater: ">= V"
    of verEqEarlier: "<= V"
    of verIntersect: "> V & < V"
    of verEq: "V"
    of verAny: "*"
    of verSpecial: "#head"

for rangeKind in VersionRangeEnum:
  echo &"{rangeKind:<15} ({rangeKind.toStr():^10}): {stats.depStats[rangeKind]}"

echo "Finished"

if not flushEach:
  cachefile.writeFile(cache.toJson().toPretty())
  errUrlsFile.writeFile(errUrls.toJson().toPretty())

for wh in stats.whenList:
  echo wh.url

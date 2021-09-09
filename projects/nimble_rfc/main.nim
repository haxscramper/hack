{.define(ssl).}

import jsony

const commitPlot = on
# TODO build commit graph that shows cumulative commits for projects
# started in year X in different colors. "What fraction of year X's commits
# come from repositories that started in year Y"

# TODO register new package version and requirement changes. Build a tree
# with `Table[<package>, Table[<major>, Table[<minor>, Table[<patch>, <leave time>]]]]`
# and record each 'leave' event - new major/minor/patch version. Collect full
# list of each requirement for each package and when it was left, and then track average
# time when upgrading from major/minor/patch versions for each package.

# Use ordered table to retain correct sequence of changes. ...

# For package events it would be better to just store everything in the
# sequence. This way I don't have to mix events from all package manifests
# in a single table, and can get more detailed info about number of skipped
# patch versions.

type
  VerChangeEventKind = enum
    vceChangePatch
    vceChangeMinor
    vceChangeMajor

  VerChangeEvent = object
    kind: VerChangeEventKind

  VerChangeTable = Table[
    string, Map[ # Package name
      int, Map[ # Major version
        int, Map[ # Minor version
          int, seq[VerChangeEvent #[ List of events ]# ]]]]]

when commitPlot:
  import ggplotnim
  import ggplotnim, chroma, times
  import scinim / signals



import
  hmisc/other/[oswrap, hshell, hlogger, hpprint, sqlite_extra],
  hmisc/algo/[htemplates, halgorithm],
  hmisc/core/all,
  hmisc/wrappers/[treesitter]


# if commitPlot and exists(AbsFile "/tmp/commits.csv"):
#   plot()



import
  nimblepkg/[version]

import
  std/[
    net, httpclient, tables, strutils, sets, times, options,
    strformat, sequtils, uri, algorithm, parseutils, sugar,
    hashes
  ]

import
  hnimast/[hast_common, pnode_parse, compiler_aux, nimble_aux]

import
  nimblepkg/[common, packageinfo, version]

import
  htsparse/bash/bash

{.passl: "-lstdc++".}

startHax()

proc getCommitTimes*(dir: AbsDir): seq[int64] =
  let cmd = makeGnuShellCmd("git").withIt do:
    it.arg "log"
    it.opt "pretty", "format:%ct"

  for line in cmd.eval().split({'\n'}):
    result.add parseInt(line)

import std/stats as runstats

type
  Package* = ref object
    stars: int
    name: string
    url: string
    `method`: string
    tags: seq[string]
    description: string
    license: string
    web: string
    doc: string

  NimbleVer = object
    kind: string
    ver: string

  NimbleRequires = object
    name: string
    str: string
    ver: NimbleVer

  NimbleDump = object
    name: string
    version: string
    author: string
    desc: string
    license: string
    skipDirs: seq[string]
    skipFiles: seq[string]
    skipExt: seq[string]
    installDirs: seq[string]
    installFiles: seq[string]
    installExt: seq[string]

    requires: seq[NimbleRequires]
    binDir: string
    srcDir: string
    backend: string


  StatFlag = enum
    sPNodeFailed
    sNimbleFailed
    sCloneNeedsPrompt
    sGenericCloneFail
    sNoRepository
    sCloneOk
    sNoUrl

  StdUseFile = object
    modules: Table[string, int]

  MetaUse = enum
    metaCanonical
    metaFromIdent
    metaSpecial

  ScopeKind = enum
    scopeTop
    scopeWhen
    scopeTask
    scopeOther

  MetaTable = Table[string, array[MetaUse, array[ScopeKind, int]]]

  Conf = object
    extraRepos: bool
    dir: AbsDir
    packageDir: AbsDir
    commitCsv: AbsFile
    doStdStats: bool
    doDownload: bool
    doUpdate: bool
    searchCache: AbsFile
    newSearch: bool
    maxSearchPages: int
    doDownloadNimble: bool
    newNimbleList: bool
    doDownloadGithubSearch: bool
    metaUses: bool
    versionDb: bool
    execStore: bool
    showParseFail: bool
    requiresStats: bool
    maxPackages: int
    maxFiles: int
    newDf: bool


  Stat = object
    commitTimes: seq[int64]
    packageDir: AbsDir

    stdUseStat: seq[StdUseFile]
    package: Package
    info: PackageInfo
    flags: set[StatFlag]

    reqList: seq[(string, VersionRange)]

    metaFields: MetaTable
    execStrs: seq[string]
    nimbleEvalTime: Option[float]
    pnodeEvalTime: Option[float]

when commitPlot:
  proc plot(file: string) =
    let df = readCsv(file)
    let versions = {
      "2014-12-29": "0.10.2",
      "2015-05-04": "0.11.2",
      "2015-10-27": "0.12.0",
      "2016-01-18": "0.13.0",
      "2016-06-09": "0.14.2",
      "2017-01-08": "0.16.0",
      "2017-09-07": "0.17.2",
      "2018-03-01": "0.18.0",
      "2019-06-17": "0.20.2",
      "2019-09-23": "1.0.0",
      "2020-04-03": "1.2.0",
      "2020-10-16": "1.4.0"
    }

    var plt = ggplot(df, aes("days", "count")) +
      geom_line() +
      scale_y_continuous() +
      scale_x_date(isTimestamp = true,
                   formatString = "MMM-yyyy",
                   dateSpacing = initDuration(weeks = 52)) +
      geom_smooth(smoother = "poly",
                  polyOrder = 7,
                  color = some(parseHex("FF0000"))) +
      geom_smooth(span = 0.64) +
      ylab("Commit count") + xlab("Date") +
      ggtitle("Daily commit counts in all nimble repositories")

    let red = some(parseHex("FF0000"))
    let transparent = color(0.0, 0.0, 0.0, 0.0)
    for (d, v) in versions:
      let d = d.parse("yyyy-MM-dd").totime.toUnix
      let dx = d.float - 86400 * 30
      plt = plt +
        annotate(
          v, x = dx, y = 400, rotate = -90.0, backgroundColor = transparent)

    plt + ggsave("/tmp/smooth_test.png", width = 1000, height = 750)


proc plot(l: HLogger, stats: seq[Stat], conf: Conf,) =
  var commitTimes: seq[int64]
  if conf.newDf:
    l.info "Processing commits"
    for stat in stats:
      commitTimes.add stat.commitTimes

    l.info "Total commit count", commitTimes.len


  when commitPlot:
    var df =
      if conf.newDf or not exists(conf.commitCsv):
        var countTable: CountTable[int64]
        for time in commitTimes:
          let day = ( time div (60 * 60 * 24) ) * ( 60 * 60 * 24 )
          countTable.inc day
        var days, count: seq[int]
        l.info countTable.len

        let commits = sortedByIt(toSeq(countTable.pairs()), it[0])
        for (key, val) in commits:
          days.add key.int
          count.add val.int

        seqsToDf(days, count)

      else:
        l.info "Reusing CSV file"
        readCsv(conf.commitCsv.getStr())

    let file: string = conf.commitCsv.getStr()
    df.writeCsv(file)
    l.info "Wrote DF to", file
    plot(file)




proc joinPath*(T: typedesc, args: varargs[string]): T =
  when result is AbsFile: result = AbsFile joinPath(args)
  elif result is AbsDir: result = AbsDir joinPath(args)
  elif result is RelFile: result = RelFile joinPath(args)
  elif result is RelDir: result = RelFile joinPath(args)

  else:
    static:
      {.error: "zzz".}

# proc `==`*(j: JsonNode, str: string): bool =
#   j.kind == JString and j.getStr() == str

# proc endsWith*(j: JsonNode, strs: openarray[string]): bool =
#   j.kind == JString and anyIt(strs, j.getStr().endsWith(it))


# proc getName(pack: string): string =
#   pack[0 ..< pack.skipWhile(IdentChars)]

# proc toJson*[T](s: HashSet[T]): JsonNode =
#   result = newJArray()
#   mixin toJson
#   for item in s:
#     result.add toJson(item)


proc dumpHook*[E: enum](s: var string, es: set[E]) =
  s.add "["
  var idx = 0
  for e in es:
    if idx > 0:
      s.add ", "

    s.add "\"" & $e & "\""
    inc idx

  s.add "]"

proc dumpHook*(s: var string, o: Version) = dumpHook(s, o.string)

type
  GitStat = object

proc getGit*(dir: AbsDir): GitStat =
  var s = "["
  let cmd = makeGnuShellCmd("git").withIt do:
    it.opt "C", dir
    it.opt "pretty", """format:'{%n  "commit": "%H",%n  "abbreviated_commit": "%h",%n  "tree": "%T",%n  "abbreviated_tree": "%t",%n  "parent": "%P",%n  "abbreviated_parent": "%p",%n  "refs": "%D",%n  "encoding": "%e",%n  "subject": "%s",%n  "sanitized_subject_line": "%f",%n  "body": "%b",%n  "commit_notes": "%N",%n  "verification_flag": "%G?",%n  "signer": "%GS",%n  "signer_key": "%GK",%n  "author": {%n    "name": "%aN",%n    "email": "%aE",%n    "date": "%aD"%n  },%n  "commiter": {%n    "name": "%cN",%n    "email": "%cE",%n    "date": "%cD"%n  }%n},'"""

  s.add evalShellStdout(cmd)
  s[^1] = ']'


proc taggedCommits*(): seq[string] =
  for line in shellCmd(
    git, log, --tags, "--no-walk", pretty = "%H").eval().split({'\n'}):

    result.add line

proc parseVersion*(v: string): tuple[major, minor, patch: int] =
  var s = strutils.strip(v).split(".")
  for i in mitems(s):
    i = i[0 ..< i.skipWhile(Digits)]

  if s.len == 2:
    return (s[0].parseInt(), s[1].parseInt(), 0)

  else:
    return (s[0].parseInt(), s[1].parseInt(), s[2].parseInt())


proc toShellStr(node: PNode): string =
  if node.kind in {nkIdent}:
    result = "$" & node.getStrVal()

  elif node.kind in nkStringKinds:
    result = node.getStrVal()

  elif node.kind in nkTokenKinds:
    result = $node

  else:
    for idx, sub in node:
      if node.kind in {nkPrefix, nkInfix} and
         idx == 0:
        continue

      result &= sub.toShellStr()

      if node.kind in {nkPrefix, nkInfix} and idx == 1:
        result &= " <&&> "

      else:
        result &= " "

proc recordExecs*(node: PNode, execs: var seq[string]) =
  case node.kind:
    of nkTokenKinds:
      discard

    of nkCall, nkCommand:
      if node[0].kind in {nkIdent} and
         node[0].getStrVal() == "exec":
        execs.add node[1].toShellStr()

      else:
        for sub in node:
          sub.recordExecs(execs)

    else:
      for sub in node:
        sub.recordExecs(execs)

var extraTop: array[TNodeKind, int]

proc recordFields*(
    node: PNode, meta: var MetaTable, scope: ScopeKind = scopeTop) =

  template inc(name: string, kind: MetaUse): untyped =
    if name notin meta:
      meta[name] = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]

    inc meta[name][kind][scope]

  template unhandledNode(node: PNode): untyped =
    if scope in {scopeWhen, scopeTop}:
      if node.kind notin {nkStmtList, nkCommentStmt}:
        inc extraTop[node.kind]

      # if node.kind in {nkIfStmt}:
      #   echo node


  case node.kind:
    of nkTokenKinds:
      discard

    of nkCall, nkCommand:
      let str = node[0].safeStrVal().normalize()
      case str:
        of "requires", "foreigndep":
          var kind = metaCanonical
          for arg in node[1 ..^ 1]:
            case arg.kind:
              of nkStringKinds: discard
              of nkIdent:       kind = metaFromIdent
              else:             kind = metaSpecial

          inc str, kind

        of "task", "before", "after":
          for sub in node:
            recordFields(sub, meta, scopeTask)

        else:
          unhandledNode(node)


    of nkAsgn:
      let str = node[0].safeStrVal().normalize()

      case str:
        of "author", "version", "packagename", "license",
           "description", "srcdir", "bindir", "backend":
          case node[1].kind:
            of nkStringKinds: inc str, metaCanonical
            of nkIdentKinds:  inc str, metaFromIdent
            else:             inc str, metaSpecial

        of "bin", "skipdirs", "skipext", "installfiles", "installdirs",
           "skipfiles", "installext":
          case node[1].kind:
            of nkPrefix:
              if node[1][0].eqIdent("@") and
                 node[1][1].kind == nkBracket:
                inc str, metaCanonical

              else:
                inc str, metaSpecial

            else:
              inc str, metaSpecial

        of "namedbin":
          inc str, metaCanonical

        else:
          unhandledNode(node)

    else:
      let newtop =
        case scope:
          of scopeTop:
            case node.kind:
              of nkStmtList: scopeTop
              of nkWhenStmt: scopeWhen
              else: scopeOther

          of scopeOther, scopeWhen:
            scopeOther

          of scopeTask:
            scopeTask

      if node.kind in nkAllDeclKinds + nkStmtBlockKinds + {
        nkImportStmt, nkIncludeStmt}:

        unhandledNode(node)

      for sub in node:
        recordFields(sub, meta, newtop)

proc logFields(l: HLogger, stats: seq[Stat]) =
  var
    metaFields: MetaTable
    canonCount, totalCount, totalRequires, cmdRequires: int

  for stat in stats:
    for key, uses in stat.metaFields:
      if key notin metaFields:
        metaFields[key] = uses

      else:
        for use, scopeMetrics in uses:
          for scope, count in scopeMetrics:
            metaFields[key][use][scope] += count

  l.info
  const
    name = 14
    sep = 7
    calign = (sep * 3, '_', '|')

  echo " " |<< 16,
     "toplevel" |<> calign,
     "when"  |<> calign,
     "task" |<> calign,
     "other" |<> calign

  stdout.write " " |<< 16
  for i in 0 .. 3:
    stdout.write "canon" |>> sep, "ident" |>> sep, "spec" |>> sep

  l.write("\n")

  for (key, val) in sortedByIt(
    toSeq(pairs(metaFields)),
    it[1][metaCanonical][scopeTop]
  ):

    stdout.write ($key |<< (name, '.')) & "  "
    for item in MetaUse:
      for scope in ScopeKind:
        let cnt = val[item][scope]
        stdout.write $cnt |>> sep

        inc totalCount, cnt
        if item == metaCanonical:
          inc canonCount, cnt

        if key == "requires":
          inc totalRequires, cnt
          if item == metaCanonical:
            inc cmdRequires, cnt


    stdout.write("\n")

  l.info &"Total metadata fields found {totalCount}, {canonCount} of which are canonical",
    &"({float(canonCount) / float(totalCount) * 100:5.3f}%)"



proc logRequires(l: HLogger, stats: seq[Stat]) =
  var requires: seq[PkgTuple]
  for stat in stats:
    requires.add stat.info.requires

  var cnt: array[VersionRangeEnum, int]
  for (name, ver) in requires:
    inc cnt[ver.kind]


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
    l.info &"{rangeKind:<15} ({rangeKind.toStr():^10}): {cnt[rangeKind]}"

proc logExecStrs(l: HLogger, stats: seq[Stat]) =
  var execStrs: seq[string]
  for package in stats:
    execStrs.add package.execStrs

  l.info "`exec` has been used", execStrs.len(), "times in",
    stats.len(), "packages"

  var cmdCounts: CountTable[string]

  proc count(tree: BashNode, s: string) =
    case tree.kind:
      of bashCommandName:
        cmdCounts.inc s[tree.slice()]

      else:
        for sub in tree:
          count(sub, s)



  var ampCount = 0
  for ex in execStrs:
    # l.info ex
    if "<&&>" in ex:
      inc ampCount

    let tree = ex.parseBashString()
    count(tree, ex)

  l.info "out of which", ampCount, "commands contained &"

  for (k, v) in toSeq(cmdCounts.pairs()).sortedByIt(-it[1]):
    if v > 10:
      l.info &"{k:<10} {v}"

proc logStdStats(l: HLogger, stats: seq[Stat]) =
  var
    totalFiles: int
    totalUsage: Table[string, int]
    perPackage: Table[string, int]

  for stat in stats:
    totalFiles += stat.stdUseStat.len
    var used: HashSet[string]
    for file in stat.stdUseStat:
      for module, count in file.modules:
        used.incl module
        totalUsage.mgetOrPut(module, 0) += count

    for module in used:
      inc perPackage.mgetOrPut(module, 0)


  let
    total: seq[(string, int)] =
      collect(newSeq, for it in pairs(totalUsage): it).sortedByIt(it[1])
    sep = 21

  echo "module" |<< sep,
    &"per file / {totalFiles}" |<< sep,
    "in % files" |<< sep,
    &"per package / {stats.len}" |<< sep,
    "in % packages" |<< sep

  for (name, count) in total:
    echo name |<< sep,
      $count |<< sep,
      &"{count / totalFiles:5.4f}" |<< sep,
      $perPackage[name] |<< sep,
      &"{perPackage[name] / stats.len:5.4f}"


proc fileStat(node: PNode): StdUseFile =
  const stdModules = toHashSet [

    "channels",
    "compilesettings",
    "decls",
    "editdistance",
    "effecttraits",
    "enumerate",
    "enumutils",
    "exitprocs",
    "genasts",
    "importutils",
    "isolation",
    "jsbigints",
    "jsfetch",
    "jsformdata",
    "jsheaders",
    "jsonutils",
    "logic",
    "monotimes",
    "packedsets",
    "setutils",
    "sha1",
    "socketstreams",
    "stackframes",
    "strbasics",
    "sums",
    "sysrand",
    "tasks",
    "tempfiles",
    "time_t",
    "varints",
    "vmutils",
    "with",
    "wordwrap",
    "wrapnils" ,
    "epoll",
    "inotify",
    "kqueue",
    "linux",
    "posix",
    "posix_freertos_consts",
    "posix_haiku",
    "posix_linux_amd64",
    "posix_linux_amd64_consts",
    "posix_macos_amd64",
    "posix_nintendoswitch",
    "posix_nintendoswitch_consts",
    "posix_openbsd_amd64",
    "posix_other",
    "posix_other_consts",
    "posix_utils",
    "termios",
    "algorithm",
    "async",
    "asyncdispatch",
    "asyncfile",
    "asyncftpclient",
    "asyncfutures",
    "asynchttpserver",
    "asyncmacro",
    "asyncnet",
    "asyncstreams",
    "base64",
    "bitops",
    "browsers",
    "cgi",
    "colors",
    "complex",
    "cookies",
    "coro",
    "cstrutils",
    "db_common",
    "distros",
    "dynlib",
    "encodings",
    "endians",
    "fenv",
    "future",
    "hashes",
    "htmlgen",
    "htmlparser",
    "httpclient",
    "httpcore",
    "json",
    "lenientops",
    "lexbase",
    "logging",
    "marshal",
    "math",
    "md5",
    "memfiles",
    "mersenne",
    "mimetypes",
    "nativesockets",
    "net",
    "nimprof",
    "nimtracker",
    "oids",
    "options",
    "os",
    "osproc",
    "oswalkdir",
    "parsecfg",
    "parsecsv",
    "parsejson",
    "parseopt",
    "parsesql",
    "parseutils",
    "parsexml",
    "pathnorm",
    "pegs",
    "prelude",
    "punycode",
    "random",
    "rationals",
    "reservedmem",
    "ropes",
    "segfaults",
    "selectors",
    "smtp",
    "ssl_certs",
    "ssl_config",
    "stats",
    "streams",
    "streamwrapper",
    "strformat",
    "strmisc",
    "strscans",
    "strtabs",
    "strutils",
    "sugar",
    "terminal",
    "times",
    "typetraits",
    "unicode",
    "unittest",
    "uri",
    "volatile",
    "xmlparser",
    "xmltree",
    "alloc",
    "ansi_c",
    "arc",
    "arithm",
    "arithmetics",
    "assertions",
    "assign",
    "atomics",
    "avltree",
    "basic_types",
    "bitmasks",
    "cellseqs_v1",
    "cellseqs_v2",
    "cellsets",
    "cgprocs",
    "channels_builtin",
    "chcks",
    "comparisons",
    "coro_detection",
    "countbits_impl",
    "cyclebreaker",
    "deepcopy",
    "dollars",
    "dyncalls",
    "embedded",
    "exceptions",
    "excpt",
    "fatal",
    "formatfloat",
    "gc",
    "gc2",
    "gc_common",
    "gc_hooks",
    "gc_interface",
    "gc_ms",
    "gc_regions",
    "hti",
    "inclrtl",
    "indexerrors",
    "integerops",
    "io",
    "iterators",
    "iterators_1",
    "jssys",
    "memalloc",
    "memory",
    "memtracker",
    "mmdisp",
    "nimscript",
    "orc",
    "osalloc",
    "platforms",
    "profiler",
    "repr",
    "repr_v2",
    "reprjs",
    "schubfach",
    "seqs_v2",
    "seqs_v2_reimpl",
    "setops",
    "sets",
    "stacktraces",
    "strmantle",
    "strs_v2",
    "syslocks",
    "sysspawn",
    "sysstr",
    "threadlocalstorage",
    "threads",
    "timers"

  ]

  proc modname(n: PNode): seq[string] =
    case n.kind:
      of nkIdent: result = @[n.getStrVal()]
      of nkInfix: result = modname(n[2])
      of nkBracket:
        for arg in n:
          result.add modname(arg)

      of nkPragmaExpr:
        result = modname(n[0])

      else:
        raise newImplementKindError(n, n.treeRepr())

  proc aux(node: PNode, table: var Table[string, int]) =
    case node.kind:
      of nkStmtList:
        for sub in node:
          aux(sub, table)

      of nkFromStmt:
        if node[0] =~ {nkInfix, nkIdent}:
          for name in node[0].modname():
            if name in stdModules:
              inc table.mgetOrPut(name, 0)

      of nkInfix:
        if node[1] =~ nkPrefix and node[1][0] =~ "./":
          discard

        elif node[1] =~ (nkIdent, "std"):
          for name in node.modname():
            inc table.mgetOrPut(name, 0)

      of nkImportStmt:
        for sub in node:
          aux(sub, table)

      else:
        discard

  var table: Table[string, int]
  aux(node, table)
  return StdUseFile(modules: table)

proc updateStdStats(stat: var Stat, dir: AbsDir, l: HLogger) =
  for file in walkDir(dir, AbsFile, exts = @["nim"], recurse = true):
    try:
      let node = parsePnodeStr(file.readFile())
      if not isNil(node):
        stat.stdUseStat.add node.fileStat()

    except:
      # Error: unhandled exception: over- or underflow [OverflowDefect]
      # while parsing `bgfx.nim`. dEfECTsShoUldNotBeCAuGht
      discard


proc dbRegister(l: HLogger, pack: seq[Stat], db: var DbConn) =
  let
    packageTable = "packages"
    requiresTable = "requires"

  db.exec packageTable.newTable({
    "id": sq(int) & sqPrimaryKey,
    "name": sq(string),
    "major": sq(int),
    "minor": sq(int),
    "patch": sq(int),
  })

  db.exec requiresTable.newTable({
    "id": sq(int)
  })


  var insert = db.prepare packageTable.newInsert({
    "id": 1, "name": 2, "major": 3, "minor": 4, "patch": 5})

  db.disableSync()
  db.journalMemory()
  db.beginTransaction()

  when false:
    var knownVersions: HashSet[string]
    withDir dir:
      let relPath = file.splitFile2().file
      var okCount, failCount, gitFail: int
      for commit in taggedCommits():
        var fails: Table[string, NimsParseFail]
        try:
          let content = shellCmd(git, show, &"{commit}:{relPath}").eval()
          let start = cpuTime()
          var extra: ExtraPackageInfo
          let info = content.parsePackageInfo(fails, extra, file)
          allVersionStat.push cpuTime() - start

          let id = info.name & info.version
          if id notin knownVersions and
             "version" notin fails and
             info.version.len > 0:
            knownVersions.incl id
            insert.bindParam(1, hashes.hash(id))
            insert.bindParam(2, info.name)

            # assert info.version.len > 0, content
            # l.info info.version
            let (major, minor, patch)  = info.version.parseVersion()
            insert.bindParam(3, major)
            insert.bindParam(4, minor)
            insert.bindParam(5, patch)

            db.doExec(insert)

            # for (name, ver) in info.requires:


          inc okCount

        except NimbleError:
          l.fail file.name(), commit
          inc failCount

        except ShellError as sh:
          inc gitFail
          # l.fail file.name(), sh.errstr.strip()
          # discard
          # l.debug content
          # raise

      l.success file.name(), okCount

  insert.finalize()
  db.endTransaction()
  db.close()


proc parseManifest(
    l: HLogger, stat: var Stat,
    failTable: var Table[string, seq[string]],
    conf: Conf
  ) =
  for file in stat.packageDir.findFilesWithExt(@["nimble", "babel"]):
    try:
      var
        fails: Table[string, NimsParseFail]
        extra: ExtraPackageInfo

      let
        start = cpuTime()
        text = file.readFile()

      stat.info = text.parsePackageInfo(fails, extra, file)

      stat.pnodeEvalTime = some cpuTime() - start
      if fails.len > 0:
        for key in fails.keys():
          failTable.mgetOrPut(key, @[]).add stat.info.name

        if conf.showParseFail:
          l.warn stat.info.name, "failed to parse", $toSeq(fails.keys())

      if extra.nimsManifest:
        if conf.execStore: extra.node.recordExecs(stat.execStrs)
        if conf.metaUses: extra.node.recordFields(stat.metaFields)

    except NimbleError:
      if conf.showParseFail:
        l.fail file.name(), "failed pnode evaluation"

      stat.flags.incl sPnodeFailed

    except Exception as e:
      l.debug file.readFile()
      l.logStackTrace(e)
      raise e

proc downloadPackages(
    l: HLogger,
    packages: seq[Package],
    failedFile, statsFile: AbsFile, packageDir: AbsDir
  ): Table[AbsDir, Package] =
  var errUrls: HashSet[string]
  var nimbleUrls: HashSet[string]

  if exists(failedFile):
    errUrls = readFile(failedFile).fromJson(HashSet[string])

  l.dump packages.len
  var cmds: seq[(ShellCmd, Package)]
  var updateCmds: seq[(ShellCmd, Package)]
  var stats: seq[Stat]
  for pack in packages:
    nimbleUrls.incl pack.url
    if pack.url.len == 0:
      l.warn "Package", pack.name, "has empty url"

      stats.add Stat(package: pack, flags: {sNoUrl})
      continue

    if pack.url in errUrls:
      l.notice pack.name, "is known to fail download"

    else:
      let resDir = packageDir / pack.name
      result[resDir] = pack
      if not exists(resDir):
        let cmd = makeGnuShellCmd("git").withIt do:
          it.cmd "clone"
          it.arg pack.url
          it.arg resDir

        cmds.add((cmd, pack))

      else:
        stats.add Stat(package: pack, flags: {sCloneOk})

  if false #[ extra repos ]#:
    for pair in readFile("repos.json").fromJson(seq[seq[string]]):
      let
        name = pair[0]
        url = parseUri(pair[1] & ".git")

      if pair[1] & ".git" notin nimbleUrls:
        let
          resDir = packageDir / AbsFile(url.path).splitFile().name
          pack = Package(name: name.split("/")[1], url: $url)

        result[resDir] = pack

        if false #[ extra repos ]#:
          if not exists(resDir):
            let cmd = makeGnuShellCmd("git").withIt do:
              it.cmd "clone"
              it.arg $url
              it.arg resDir

            cmds.add((cmd, pack))


  var failCnt = 0
  withEnv({ $$GIT_TERMINAL_PROMPT : "0"}):
    for (res, data) in runShellResult(cmds):
      if not res.resultOk:
        let err = res.execResult.stderr
        if "terminal prompts disabled" in err:
          l.fail data.name, "terminal prompt needed"
          stats.add Stat(package: data, flags: {sCloneNeedsPrompt})
          errUrls.incl data.url

        elif "SSL certificate problem" in err:
          l.fail data.name, "ssl certificate"
          stats.add Stat(package: data, flags: {sGenericCloneFail})
          errUrls.incl data.url

        elif "repository either does not exist" in err:
          l.fail data.name, "no repository"
          stats.add Stat(package: data, flags: {sNoRepository})
          errUrls.incl data.url

        elif "Cloning into" in err and
             "..." in err and
             "not valid: is this a git repository?" in err:
          errUrls.incl data.url

        elif "already exists and is not" in err:
          discard

        else:
          l.warn err

      else:
        l.info "Cloned", data.name

  writeFile(failedFile, errUrls.toJson())
  writeFile(statsFile, stats.toJson())
  l.done "Package download finished"
  l.dump failedFile
  l.dump statsFile

type
  GhItem = object
    name: string
    fork: bool
    git_url: string
    stargazers_count: int
    description: string

  GhSearch = object
    total_count: int
    items: seq[GhItem]

  GhRateStat = object
    limit: int
    remaining: int
    reset: int
    used: int

  GhRateGroup = object
    core, search: GhRateStat

  GhRate = object
    resources: GhRateGroup
    rate: GhRateStat


proc addNimblePackages(conf: Conf, l: HLogger, packages: var Table[string, Package]) =
  let file = RelFile("packages.json")
  if not exists(file) or conf.newNimbleList:
    let url = "https://raw.githubusercontent.com/nim-lang/packages/master/packages.json"
    let client = newHttpClient()
    client.downloadFile(url, file.string)

  for pack in file.readFile().fromJson(seq[Package]):
    packages[pack.url] = pack

proc addSearchPackages(conf: Conf, l: HLogger, packages: var Table[string, Package]) =
  const url = "https://api.github.com/search/repositories?q=language:nim&per_page=$1&page=$2"
  let client = newHttpClient()
  # var pages: seq[()]

  proc getRate(): GhRateStat =
    let j = client.getContent("https://api.github.com/rate_limit")
    return j.fromJson(GhRate).resources.search

  var allPages: Table[string, seq[GhItem]]
  proc waitRate(cutoff: int = 1) =
    let rate = getRate()
    if rate.remaining == cutoff:
      let waitTo = rate.reset
      let now = now().toTime().toUnix()
      let diff = (waitTo - now) + 2
      l.info "Time limit on free search API reached, waiting", diff, "seconds"
      conf.searchCache.writeFile(allPages.toJson())
      sleep(int(diff * 1000))

    assert cutoff < getRate().remaining

  if conf.searchCache.exists():
    allPages = conf.searchCache.readFile().fromJson(typeof(allPages))
    l.info "loaded", allPages.len, "pages from cache"


  waitRate()
  let init = client.getContent(url % ["1", "1"]).fromJson(GhSearch)
  for pageNum in 1 .. min(init.totalCount + 2, conf.maxSearchPages):
    # For unauthenticated requests, the rate limit allows you to make
    # up to 10 requests per minute. (and 2s extra timeout because I
    # had bugs otherwise)
    if $pageNum in allPages:
      l.info "Already processed page", pageNum

    else:
      waitRate()

      l.wait "Processing search page", pageNum

      let getUrl = url % ["100", $pageNum]
      try:
        let page = client.getContent(getUrl)
        let search = page.fromJson(GhSearch)

        l.success
        allPages[$pageNum] = search.items

      except HttpRequestError as err:
        l.fail "Processing page", pageNum
        l.pdump getRate()
        l.dump getUrl
        l.debug err.msg
        # TODO https://stackoverflow.com/questions/37602893/github-search-limit-results
        #
        # It seems like neither authorized no anauthorized API calls
        # can really featch more than 1000 results, which means I would
        # have to get smarter with queries. Searching for all
        # repositories that were created in particular timeframe
        # (starting from 2004 and to current time, in one month chunks
        # (I really doubt there is more than a thousand nim
        # repositories created per month, so we are mostlyt safe
        # here))
        break

  conf.searchCache.writeFile(allPages.toJson())

  for page, pageItems in allpages:
    for item in pageItems:
      if item.gitUrl notin packages and not item.fork:
        packages[item.gitUrl] = Package(
          stars: item.stargazersCount,
          name: item.name,
          url: item.gitUrl,
          description: item.description)

      else:
        packages[item.gitUrl].stars = item.stargazersCount



proc main(conf: Conf) =
  var l = newTermLogger()
  var packMap: Table[AbsDir, Package]
  if conf.doDownload:
    var packages: Table[string, Package]
    if conf.doDownloadNimble:
      conf.addNimblePackages(l, packages)
      l.info "packages after adding nimble:", packages.len

    if conf.doDownloadGithubSearch:
      conf.addSearchPackages(l, packages)
      l.info "packages after adding github search:", packages.len

    packMap = l.downloadPackages(
      packageDir = conf.packageDir,
      packages = collect(newSeq, for _, v in packages: v),
      failedFile = conf.dir /. "failed.json",
      statsFile = conf.dir /. "stats.json"
    )

  if conf.doUpdate:
    var cmds: seq[(ShellCmd, AbsDir)]
    for dir in walkDir(conf.packageDir, AbsDir):
      let cmd = makeGnuShellCmd("git").withIt do:
        it.opt "C", " ", $dir
        it.cmd("pull")

      cmds.add(cmd, dir)

    withEnv({ $$GIT_TERMINAL_PROMPT : "0"}):
      for (res, dir) in runShellResult(cmds):
        if not res.resultOk:
          let (stdout, stderr, code) = split(res)
          if "Already up to date" in res.execResult.stdout:
            l.success dir.name(), "already up to date"

          elif "Updating" in stdout and "Fast-forward" in stdout:
            l.success dir.name(), "updated"
            l.info stdout

          elif "terminal prompts disabled" in stderr:
            discard

          else:
            l.dump stderr
            l.dump stdout
            l.dump dir
            break

        else:
          l.success "updated", dir.name()


  var
    failTable: Table[string, seq[string]]
    parseStats: seq[Stat]

  let commits = commitPlot and (conf.newDf or not exists(conf.commitCsv))
  if commits:
    l.info "Getting commit stats"

  for dir in walkDir(conf.packageDir, AbsDir):
    if conf.maxPackages < globalTick():
      break

    var stat = Stat(
      package: packMap.mgetOrPut(
        dir,
        Package(name: dir.name())),
      packageDir: dir
    )

    assertRef stat.package

    if commits:
      stat.commitTimes.add getCommitTimes(dir)

    parseStats.add stat

  if commitPlot:
    plot(l, parseStats, conf)

  for stat in mitems(parseStats):
    l.parseManifest(stat, failTable, conf)

    if conf.doStdStats and globalTick() < conf.maxFiles:
      l.info $globalTick() |<< 4, stat.package.name
      updateStdStats(stat, stat.packageDir, l)

  if conf.doStdStats:
    logStdStats(l, parseStats)

  l.done

  var pnodeStat: RunningStat
  for stat in parseStats:
    if stat.pnodeEvalTime.isSome():
      pnodeStat.push stat.pnodeEvalTime.get()

  if conf.showParseFail:
    l.info "failed to parse"
    l.indented:
      for key, val in failTable:
        l.debug key, $val

  for (kind, num) in sortedByIt(toSeq(pairs(extraTop)), it[1]):
    if num > 0:
      l.info ($kind)[2 ..^ 1] |<< 15, num


  if pnodeStat.n > 0:
    l.info "Processed", pnodeStat.n, "packages via pnode in", &"{pnodeStat.sum:5.3f}"
    l.info "average processing time:", &"{pnodeStat.mean():5.3f}"

  l.info "done"

let conf = Conf(
  dir:                    cwd() / "main",
  packageDir:             cwd() / "main/packages",
  searchCache:            cwd() /. "search-cache.json",
  newSearch:              true,
  maxSearchPages:         120,
  newDf: true,
  commitCsv:              AbsFile("/tmp/commit_csv.csv"),
  doStdStats:             false,

  doDownload:             false,
  doUpdate:               false,
  doDownloadNimble:       true,
  newNimbleList:          false,
  doDownloadGithubSearch: false,

  metaUses:               false,
  versionDb:              false,
  execStore:              false,
  showParseFail:          false,
  requiresStats:          false,
  maxPackages:            4_000,
  maxFiles:               45_000
)


main(conf)

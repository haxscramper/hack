{.define(ssl).}

import jsony

const commitGraph = off

when commitGraph:
  import ggplotnim

import
  hmisc/other/[oswrap, hshell, hlogger, hpprint, sqlite_extra],
  hmisc/algo/[htemplates, halgorithm],
  hmisc/[hdebug_misc, base_errors],
  hmisc/wrappers/[treesitter]


import
  nimblepkg/[version]

import
  std/[
    net, httpclient, tables, strutils, sets, times, options,
    strformat, sequtils, uri, algorithm, parseutils, sugar
  ]

import
  hnimast/[hast_common, pnode_parse, compiler_aux, nimble_aux]

import
  htsparse/bash/bash

{.passl: "-lstdc++".}

startHax()

import std/stats as runstats

type
  Package* = ref object
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

  Stat = object
    stdUseStat: seq[StdUseFile]
    package: Package
    flags: set[StatFlag]

    reqList: seq[(string, VersionRange)]

    nimbleEvalTime: Option[float]
    pnodeEvalTime: Option[float]

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

proc getCommitTimes*(dir: AbsDir): seq[int64] =
  let cmd = makeGnuShellCmd("git").withIt do:
    it.arg "log"
    it.opt "pretty", "format:%ct"

  for line in cmd.eval().split({'\n'}):
    result.add parseInt(line)

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

type
  MetaUse = enum
    metaCanonical
    metaFromIdent
    metaSpecial

  ScopeKind = enum
    scopeTop
    scopeWhen
    scopeTask
    scopeOther

type
  MetaTable = Table[string, array[MetaUse, array[ScopeKind, int]]]

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

let
  doStdStats = false
  doDownload = false
  metaUses = true
  versionDb = false
  execStore = false
  parseFail = false
  requiresStats = false

when isMainModule:

  let url = "https://raw.githubusercontent.com/nim-lang/packages/master/packages.json"

  let file = RelFile("packages.json")
  block:
    if not exists(file):
      let client = newHttpClient()
      client.downloadFile(url, file.string)

  let
    packages = file.readFile().fromJson(seq[Package])

  var l = newTermLogger()

  let
    dir = cwd() / "main"
    packageDir = dir / "packages"
    failedFile = dir /. "failed.json"
    statsFile = dir /. "stats.json"

  l.dump failedFile

  var errUrls: HashSet[string]

  if exists(failedFile):
    errUrls = readFile(failedFile).fromJson(HashSet[string])

  var cmds: seq[(ShellCmd, Package)]
  var packMap: Table[AbsDir, Package]
  var nimbleUrls: HashSet[string]

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
      packMap[resDir] = pack
      if not exists(resDir):
        let cmd = makeGnuShellCmd("git").withIt do:
          it.cmd "clone"
          it.arg pack.url
          it.arg resDir

        cmds.add((cmd, pack))

      else:
        stats.add Stat(package: pack, flags: {sCloneOk})

  let extraRepos = false
  for pair in readFile("repos.json").fromJson(seq[seq[string]]):
    let
      name = pair[0]
      url = parseUri(pair[1] & ".git")

    if pair[1] & ".git" notin nimbleUrls:
      let
        resDir = packageDir / AbsFile(url.path).splitFile().name
        pack = Package(name: name.split("/")[1], url: $url)

      packMap[resDir] = pack

      if extraRepos:
        if not exists(resDir):
          let cmd = makeGnuShellCmd("git").withIt do:
            it.cmd "clone"
            it.arg $url
            it.arg resDir

          cmds.add((cmd, pack))


  if doDownload:
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

          elif "Cloning into" in err and "..." in err:
            l.success data.name

          elif "already exists and is not" in err:
            discard

          else:
            l.warn err

        else:
          l.info "Cloned", data.name

  if doDownload:
    writeFile(failedFile, errUrls.toJson())
    writeFile(statsFile, stats.toJson())
    l.done "Package download finished"
    l.dump failedFile
    l.dump statsFile

  var
    failTable: Table[string, seq[string]]
    nimbleStat: RunningStat
    allVersionStat: RunningStat
    parseStats: seq[Stat]
    commitTimes: seq[int64]



  var db = sqliteOpenNew(dir /. "stats.sqlite")

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


  var
    knownVersions: HashSet[string]
    execStrs: seq[string]
    cmdRequires: int
    totalRequires: int
    requires: seq[PkgTuple]
    metaFields: MetaTable


  for dir in walkDir(packageDir, AbsDir):
    if commitGraph:
      withDir dir:
        commitTimes.add dir.getCommitTimes()


    var stat = Stat(package: packMap.getOrDefault(dir))
    for file in dir.findFilesWithExt(@["nimble", "babel"]):
      if versionDb:
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
                insert.bindParam(1, hash(id))
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

      else:
        let tryNimble = nimbleStat.n < 0
        if tryNimble:
          withDir dir:
            try:
              let start = cpuTime()
              let dump = runShell(shellCmd(nimble, dump, --json))
              stat.nimbleEvalTime = some cpuTime() - start
              nimbleStat.push stat.nimbleEvalTime.get()


            except ShellError as e:
              l.fail file.name(), "failed evaluation"
              stat.flags.incl sNimbleFailed

        block:
          try:
            var
              fails: Table[string, NimsParseFail]
              extra: ExtraPackageInfo

            let
              start = cpuTime()
              text = file.readFile()
              info = text.parsePackageInfo(fails, extra, file)

            requires.add info.requires


            stat.pnodeEvalTime = some cpuTime() - start
            if fails.len > 0:
              for key in fails.keys():
                failTable.mgetOrPut(key, @[]).add info.name

              if parseFail:
                l.warn info.name, "failed to parse", $toSeq(fails.keys())

            if extra.nimsManifest:
              if execStore: extra.node.recordExecs(execStrs)
              if metaUses: extra.node.recordFields(metaFields)

            if doStdStats and globalTick() < 30_000:
              l.info $globalTick() |<< 4, stat.package.name
              updateStdStats(stat, dir, l)


          except NimbleError:
            l.fail file.name(), "failed pnode evaluation"
            stat.flags.incl sPnodeFailed

          except Exception as e:
            l.debug file.readFile()
            l.logStackTrace(e)
            raise e

      parseStats.add stat


  insert.finalize()
  db.endTransaction()
  db.close()

  l.done

  var pnodeStat: RunningStat

  for stat in parseStats:
    if stat.pnodeEvalTime.isSome():
      pnodeStat.push stat.pnodeEvalTime.get()

  if parseFail:
    l.info "failed to parse"
    l.indented:
      for key, val in failTable:
        l.debug key, $val

  for (kind, num) in sortedByIt(toSeq(pairs(extraTop)), it[1]):
    if num > 0:
      l.info ($kind)[2 ..^ 1] |<< 15, num

  var canonCount, totalCount: int

  if metaUses:
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
        # $val[metaFromIdent][0] |>> sep,
        # $val[metaSpecial][0]   |>> sep,
        # $val[metaCanonical][1] |>> sep,
        # $val[metaFromIdent][1] |>> sep,
        # $val[metaSpecial][1]   |>> sep

  l.info &"Total metadata fields found {totalCount}, {canonCount} of which are canonical",
    &"({float(canonCount) / float(totalCount) * 100:5.3f}%)"

  if requiresStats:
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


  if execStrs.len > 0:
    l.info "`exec` has been used", execStrs.len(), "times in", pnodeStat.n, "packages"
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


  l.info "Found", totalRequires, "out of which 'canonical'", cmdRequires

  if doStdStats:
    var
      totalFiles: int
      totalUsage: Table[string, int]
      perPackage: Table[string, int]

    for stat in parseStats:
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
      &"per package / {parseStats.len}" |<< sep,
      "in % packages" |<< sep

    for (name, count) in total:
      echo name |<< sep,
        $count |<< sep,
        &"{count / totalFiles:5.4f}" |<< sep,
        $perPackage[name] |<< sep,
        &"{perPackage[name] / parseStats.len:5.4f}"


  if pnodeStat.n > 0:
    l.info "Processed", pnodeStat.n, "packages via pnode in", &"{pnodeStat.sum:5.3f}"
    l.info "average processing time:", &"{pnodeStat.mean():5.3f}"

  if allVersionStat.n > 0:
    l.info "Processed", allVersionStat.n, "package version in", &"{allVersionStat.sum:5.3f}"
    l.info "average processing time:", &"{allVersionStat.mean():5.3f}"

  if nimbleStat.n > 0:
    l.info "Processed", nimbleStat.n, "packages via nimble in", &"{nimbleStat.sum:5.3f}"
    l.info "average processing time:", &"{nimbleStat.mean():5.3f}"


  if commitGraph:
    l.info "Total commit count", commitTimes.len

    var countTable: CountTable[int64]

    for time in commitTimes:
      let day = ( time div (60 * 60 * 24) ) * ( 60 * 60 * 24 )
      countTable.inc day


    var days, count: seq[int]
    let commits = sortedByIt(toSeq(countTable.pairs()), it[0])
    l.info commits[0]
    for (key, val) in commits:
      days.add key.int
      count.add val.int

    when commitGraph:
      let df = seqsToDf(days, count)
      ggplot(df, aes("days", "count")) +
        geom_line() +
        gg_save("/tmp/res.png")

      df.writeCsv("/tmp/commits.csv")

  l.info "done"


  # var cnt = 0

  # if fileExists(errUrlsFile):
  #   for entry in errUrlsFile.parseJson():
  #     errUrls.incl entry.asStr()

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

# Read two odt files from command line, unpack them as zip archives and
# then diff their contents.

import
  hmisc/other/[oswrap, hshell],
  hmisc/core/all,
  hmisc/algo/[hseq_distance, clformat, hstring_algo]

import
  std/[algorithm, strutils]

starthax()


var files = paramStrs()

if files.len == 0:
  files.add "/tmp/file1.odt"
  files.add "/tmp/file2.odt"

let dir = getAppTempDir()
mkDir dir

var lists: tuple[f1, f2: seq[RelFile]]
var dirs: tuple[d1, d2: AbsDir]

for idx, file in files:
  let absf = toAbsFile(file)
  assertExists absf
  let dest = dir / absf.name()
  if idx == 0:
    dirs.d1 = dest

  else:
    dirs.d2 = dest

  rmDir dest

  discard runShell shellCmd(unzip).withIt do:
    it.arg absf
    it.opt("d", " ", $dest)

  for file in walkDir(dest, RelFile, recurse = true):
    if file.ext() == "xml":
      let pretty = evalShellStdout shellCmd(xmllint).withIt do:
        it.opt("pretty", " ", "2")
        it.arg $(dest / file)

      writeFile(dest / file, pretty)

    if idx == 0:
      lists.f1.add file

    else:
      lists.f2.add file

lists.f1.sort()
lists.f2.sort()
let diff = myersDiff(lists.f1, lists.f2)

for line in items(diff):
  case line.kind:
    of dekDelete:
      echov "First archive deleted file", lists.f1[line.oldPos]

    of dekInsert:
      echov "Second archive added file", lists.f2[line.newPos]

    of dekKeep:
      let abs1 = dirs.d1 / lists.f1[line.oldPos]
      let abs2 = dirs.d2 / lists.f2[line.newPos]
      let t1 = abs1.readFile()
      let t2 = abs2.readFile()
      if t1 != t2:
        if lists.f1[line.oldPos].ext() notin ["png"]:
          echov "has changes", lists.f1[line.oldPos]
          let l1 = t1.split('\n')
          let l2 = t2.split('\n')

          echo myersDiff(l1, l2).shiftDiffed(l1, l2).formatDiffed(
            l1, l2,
            maxUnchanged = 0,
            showLines = true,
            wordSplit = proc(s: string): seq[string] =
                          splitTokenize(s, {':', ' ', '<', '>', '='})
          )

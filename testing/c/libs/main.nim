import wn
import std/os
import hmisc/other/hpprint

{.compile: "binsrch.c".}
{.compile: "morph.c".}
{.compile: "search.c".}
{.compile: "wnglobal.c".}
{.compile: "wnhelp.c".}
{.compile: "wnrtl.c".}
{.compile: "wnutil.c".}
{.compile: "wordnet.c".}




proc wordnet_main(
  argc: cint, argv: cstringarray): int {.importc: "wordnet_main".}

template addLinkedField*(typ, field: untyped): untyped {.dirty.} =
  iterator `iter field`(item: ptr typ): ptr typ =
    var res = item
    while not isNil(res):
      yield res
      res = res.field

addLinkedField(Synset, nextss)
addLinkedField(Synset, nextform)
addLinkedField(Synset, ptrlist)

putEnv("WNHOME", "/usr/share/wordnet")

discard wordnet_main(3, allocCstringArray(["", "test", "-synsn"]))
echo "main ok"

for pos in 1.cint .. NUMPARTS.cint:
  let res = findtheinfo_ds("test", pos, HYPERPTR.cint, ALLSENSES)
  pprint res
  for sym in iterNextss(res):
    echo sym.defn
    for p in iterPtrlist(sym):
      echo ">> ", p.defn

{.experimental: "views".}
{.experimental: "strictFuncs".}

import std/[tables, hashes]
type ID = int64

type
  StrView = object
    start, finish: int
    base: ptr string

type DeBruijnGraph* = object
    source*: string
    kmers*: seq[StrView]
    edgesOut*: seq[seq[int32]]
    edgesIn*: seq[seq[int32]]
    kMerMap*: Table[StrView, ID]

proc toStrView(str: var string, start, finish: int): StrView =
  StrView(base: addr str, start: start, finish: finish)

proc hash*(view: StrView): Hash =
  var h: Hash = 0
  for ch in view.start .. view.finish:
    h = h !& hash(view.base[][ch])

  result = !$h

proc findOrCreateKmer(graph: var DeBruijnGraph, kMer: StrView): ID=
  graph.kMerMap[kMer] = graph.kMerMap.len

proc build*(sourceString: var string, kmerLength: int): DeBruijnGraph =
  var t = DeBruijnGraph(source: sourceString, kmers: @[], edgesOut: @[], edgesIn: @[], kMerMap: initTable[StrView, ID]())
  var length = (t.source.len() - (kmerLength-1))
  echo length
  for i in 0 ..< length:
    let kmerL = t.source.toStrView(i, (i-1)+(kmerLength-1) )
    let kmerR = t.source.toStrView(i+1, (i)+(kmerLength-1) )
    #TODO In debug histrogramm of k-mers

    let nodeL: ID = t.findOrCreateKmer(kmerL)
    let nodeR: ID = t.findOrCreateKmer(kmerR)

var base = "CADCADCADCADACADCADACADACDACADCADCADACADCDAD"
let graph = build(base, 0)

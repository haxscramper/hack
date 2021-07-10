import stew/assign2
import std/sequtils
import benchy

var inputSeq = toSeq(0 .. 100_000)
var resSeq = inputSeq


timeIt "Copy sequence using default":
  resSeq = inputSeq
  resSeq.add 90
  discard resSeq[0]

timeIt "Copy sequence using assign2":
  assign(resSeq, inputSeq)
  resSeq.add 90
  discard resSeq[0]

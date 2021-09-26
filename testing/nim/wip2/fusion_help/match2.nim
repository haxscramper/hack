import fusion/matching
import std/options

block: echo Some([@first, all @tail]) ?= some @[1]
block: echo Some([@first, all @tail]) ?= some newSeq[int](0)
block: echo Some([@first, all @tail]) ?= none seq[int]

var s: seq[int]
[@first, all @tail] := s

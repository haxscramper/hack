import sequtils
import benchy

proc reverseWithOpenArray[T](s: var openArray[T]) =
  var
    x  = 0
    y = s.len - 1
  while x < y:
    swap s[x], s[y]
    dec y
    inc x

proc reverseWithSeq[T](s: var seq[T])=
  var
    x  = 0
    y = s.len - 1
  while x < y:
    swap s[x], s[y]
    dec y
    inc x


type S = object

var s1 = newSeqWith(1000000, S())

timeIt "reverse With OpenArray":
  s1.reverseWithOpenArray
timeIt "reverse With Seq":
  s1.reverseWithSeq

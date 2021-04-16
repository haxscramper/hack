import std/sequtils

func delete[T](s: var seq[T]; last, start: BackwardsIndex) =
  s.delete(start.int, last.int)

func pop[T](s: var seq[T], r: HSlice): seq[T] =
  result = s[r.a .. r.b]
  s.delete(r.a, r.b)

var s = @[1,2,3,4,5]
echo pop(s, 0 .. 2)
echo s
